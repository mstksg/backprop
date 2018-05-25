---
title: Equipping your Library
---

Equipping your Library for Backprop
===================================

```haskell top hide
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}


import           Data.Functor.Identity
import qualified Data.List
import           GHC.Generics (Generic)
import           GHC.TypeNats
import           Inliterate.Import
import           Lens.Micro
import           Lens.Micro.TH
import           Numeric.Backprop
import           Numeric.Backprop.Class
import           Numeric.LinearAlgebra.Static (L, R, konst)
import           System.Random
import qualified Data.Vector                  as V
import qualified Numeric.LinearAlgebra.Static as H
import qualified Numeric.LinearAlgebra        as HU
```

So you want your users to be able to use your numerical library with
*backprop*, huh?

This page is specifically for library authors who want to allow their users to
use their library operations and API with *backprop*.  End-users of the
*backprop* library should not have to worry about the contents of this page.

Equipping your library with backprop involves providing "backprop-aware"
versions of your library functions.  *In fact*, it is possible to make a
library fully by providing *only* backprop versions of your functions, since
you can use a backprop-aware function as a normal function with `evalBP`.
Alternatively, you can re-export all of your functions in a separate module with
"backprop-aware" versions.

Know Thy Types
--------------

The most significant effort will be in lifting your library's functions.  If
you have a function:

```haskell
myFunc :: a -> b
```

Then its lifted version would have type:

```haskell
myFunc :: Reifies s W => BVar s a -> BVar s b
```

That is, instead of a function directly taking an `a` and returning a `b`, it's
a function taking a `BVar` containing an `a`, and returning a `BVar` containing
a `b`.

Functions taking multiple arguments can be translated pretty straightforwardly:

```haskell
func1   ::                       a ->        b ->        c
func1BP :: Reifies s W => BVar s a -> BVar s b -> BVar s c
```

And also functions returning multiple arguments:

```haskell
func2   ::                       a -> (       b,        c)
func2BP :: Reifies s W => BVar s a -> (BVar s b, BVar s c)
```

It is recommended (for ease of use with `-XTypeApplications`) that `Reifies s
W` be the *final* constraint in all code you write.

Note that almost all operations involving `BVar`'d items require that the
contents have a `Backprop` instance.  Alternative API's to backprop that
require `Num` instances instead (or explicitly specified addition functions)
are available in *Numeric.Backprop.Num* and *Numeric.Backprop.Explicit*.

The Easy Way
------------

`BVar` based functions are just normal functions, so they can be applied
normally and passed as first-class values.  If possible, if you can *utilize*
functions that are already `BVar`'d/lifted, then you can just define your API
in terms of those lifted functions.  This is also how *users* are expected to
be able to use your library: just use the lifted functions you provide, in
order to make their own lifted functions using normal function application and
composition.

However, if no lifted primitive functions are available, then you do have to do
some legwork to provide information on gradient computation for your types.
Ideally, you would only need to do this for some minimal set of your
operations, and then define the rest of them in terms of the functions you have
already lifted.

Lifting operations manually
---------------------------

A `BVar s a -> BVar s b` really encodes two things:

1.  A `a -> b` (the actual function)
2.  A `a -> b -> a` (the "scaled gradient" function)

The documentation for [Numeric.Backprop.Op][op] gives detail about what these
entail, with rendered math and examples.

[op]: http://hackage.haskell.org/package/backprop/docs/Numeric-Backprop-Op.html

The second function requires some elaboration.  Let's say you are writing a
lifted version of your function \\(y = f(x)\\) (whose derivative is
\\(\frac{dy}{dx}\\)), and that your *final result* at the end of your computation
is \\(z = g(f(x))\\) (whose derivative is \\(\frac{dz}{dx}\\)).  In that case, because of the
chain rule, \\(\frac{dz}{dx} = \frac{dz}{dy} \frac{dy}{dx}\\).

The scaled gradient is the function which, *given* \\(\frac{dy}{dz}\\), *returns*
\\(\frac{dz}{dx}\\). (that is, returns \\(\frac{dz}{dy} \frac{dy}{dx}\\)).

For example, for the mathematical operation \\(y = f(x) = x^2\\), then, considering
\\(z = g(f(x))\\), \\(\frac{dz}{dx} = \frac{dz}{dy} 2x\\).
In fact, for all functions taking and returning scalars (just normal single
numbers), \\(\frac{dz}{dx} = \frac{dz}{dy} f'(x)\\).

With that in mind, let's write our "squared" op:

```haskell top
square
    :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
square = liftOp1 . op1 $ \x ->
    ( x^2              , \dzdy -> dzdy * 2 * x)
--    ^- actual result   ^- scaled gradient function
```

Keeping along the same pattern, for \\(y = f(x) = \sin(x)\\), then, considering \\(z
= g(f(x))\\), \\(\frac{dz}{dx} = \frac{dz}{dy} \cos(x)\\).  So, we have:

```haskell top
liftedSin
    :: (Floating a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
liftedSin = liftOp1 . op1 $ \x ->
    ( sin x, \dzdy -> dzdy * cos x )
```

In general, for functions that take and return scalars:

```haskell
liftedF
    :: (Reifies s W, Backprop a, Num a)
    => BVar s a
    -> BVar s a
liftedF = liftOp1 . op1 $ \x ->
    ( f x, \dzdy -> dzdy * dfdx x )
```

For an example of every single numeric function in base Haskell, see [the
source of Op.hs][opsource] for the `Op` definitions for every method in `Num`,
`Fractional`, and `Floating`.

[opsource]: https://github.com/mstksg/backprop/blob/a7651b4549048a3aca73c79c6fbe07c3e8ee500e/src/Numeric/Backprop/Op.hs#L646-L787

### Non-trivial example

A simple non-trivial example is `sumElements`, which we can define to take the
*hmatrix* library's `R n` type (an n-vector of `Double`).  In this case, we
have to think about \\(g(\mathrm{sum}(\mathbf{x}))\\).  In this case, the types
guide our thinking:

```haskell
sumElements           :: R n -> Double
sumElementsScaledGrad :: R n -> Double -> R n
```

The simplest way for me to do this personally is to just take it element by
element.

1.  *Write out the functions in question, in a simple example*

    In our case:

    *   \\(y = f(\langle a, b, c \rangle) = a + b + c\\)
    *   \\(z = g(y) = g(a + b + c)\\)

2.  *Identify the components in your gradient*

    In our case, we have to return a gradient \\(\langle \frac{\partial z}{\partial a},
    \frac{\partial z}{\partial b}, \frac{\partial z}{\partial c} \rangle\\).

3.  *Work out each component of the gradient until you start to notice a
    pattern*

    Let's start with \\(\frac{\partial z}{\partial a}\\).  We need to find
    \\(\frac{\partial z}{\partial a}\\) in terms of \\(\frac{dz}{dy}\\):

    *   Through the chain rule, \\(\frac{\partial z}{\partial a} =
        \frac{dz}{dy} \frac{\partial y}{\partial a}\\).
    *   Because \\(y = a + b + c\\), we know that \\(\frac{\partial y}{\partial
        a} = 1\\).
    *   Because \\(\frac{\partial y}{\partial a} = 1\\), we know that
        \\(\frac{\partial z}{\partial a} = \frac{dz}{dy} \times 1 =
        \frac{dz}{dy}\\).

    So, our expression of \\(\frac{\partial z}{\partial a}\\) in terms of
    \\(\frac{dz}{dy}\\) is simple -- it's simply \\(\frac{\partial z}{\partial
    a} = \frac{dz}{dy}\\).

    Now, let's look at \\(\frac{\partial z}{\partial b}\\).  We need to find
    \\(\frac{\partial z}{\partial b}\\) in terms of \\(\frac{dz}{dy}\\).

    *   Through the chain rule, \\(\frac{\partial z}{\partial b} =
        \frac{dz}{dy} \frac{\partial y}{\partial b}\\).
    *   Because \\(y = a + b + c\\), we know that \\(\frac{\partial y}{\partial
        b} = 1\\).
    *   Because \\(\frac{\partial y}{\partial b} = 1\\), we know that
        \\(\frac{\partial z}{\partial b} = \frac{dz}{dy} \times 1 =
        \frac{dz}{dy}\\).

    It looks like \\(\frac{\partial z}{\partial b} = \frac{\partial z}{\partial
    y}\\), as well.

    At this point, we start to notice a pattern.  We can apply the same logic
    to see that \\(\frac{\partial z}{\partial c} = \frac{dz}{dy}\\).

4.  *Write out the pattern*

    Extrapolating the pattern, \\(\frac{\partial z}{\partial q}\\), where
    \\(q\\) is *any* component, is always going to be a constant --
    \\(\frac{dz}{dy}\\).

So in the end:

```haskell top hide
instance Backprop (R n) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance (KnownNat n, KnownNat m) => Backprop (L n m) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

sumElements :: KnownNat n => R n -> Double
sumElements = HU.sumElements . H.extract
```

```haskell top
liftedSumElements
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s Double
liftedSumElements = liftOp1 . op1 $ \xs ->
    ( sumElements xs, \dzdy -> konst dzdy )  -- a constant vector
```

### Multiple-argument functions

Lifting multiple-argument functions is the same thing, except using `liftOp2`
and `op2`, or `liftOpN` and `opN`.

A `BVar s a -> BVar s b -> BVar s c` encodes two things:

1.  The actual `a -> b -> c`
2.  The scaled gradient, `a -> b -> c -> (a, b)`.

The `c` parameter of the scaled gradient is again \\(\frac{dz}{dy}\\), and the
final `(a,b)` is a tuple of \\(\frac{\partial z}{\partial x_1}\\) and
\\(\frac{\partial z}{\partial x_2}\\): how \\(\frac{dz}{dy}\\) affects both of
the inputs.

For a simple example, let's look at \\(x + y\\).  Working it out:

*   \\(y = f(x_1, x_2) = x_1 + x_2\\)
*   \\(z = g(f(x_1, x_2)) = g(x_1 + x_2)\\)
*   Looking first for \\(\frac{\partial z}{\partial x_1}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_1}\\) (chain rule)
    *   From \\(y = x_1 + x_2\\), we see that \\(\frac{\partial y}{\partial
        x_1} = 1\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \times 1
        = \frac{dz}{dy}\\).
*   Looking second for \\(\frac{\partial z}{\partial x_2}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_2}\\) (chain rule)
    *   From \\(y = x_1 + x_2\\), we see that \\(\frac{\partial y}{\partial
        x_2} = 1\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy} \times 1
        = \frac{dz}{dy}\\).
*   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy}\\), and also
    \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy}\\).

Putting it into code:

```haskell top
add :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
    -> BVar s a
add = liftOp2 . op2 $ \x1 x2 ->
    ( x1 + x2, \dzdy -> (dzdy, dzdy) )
```

Let's try our hand at multiplication, or \\(x * y\\):

*   \\(y = f(x_1, x_2) = x_1 x_2\\)
*   \\(z = g(f(x_1, x_2)) = g(x_1 x_2)\\)
*   Looking first for \\(\frac{d\partial }{d\partial _1}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_1}\\) (chain rule)
    *   From \\(y = x_1 x_2\\), we see that \\(\frac{\partial y}{\partial x_1}
        = x_2\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} x_2\\).
*   Looking second for \\(\frac{\partial z}{\partial x_2}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_1}\\) (chain rule)
    *   From \\(y = x_1 x_2\\), we see that \\(\frac{\partial y}{\partial x_2}
        = x_1\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy} x_1\\).
*   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} x_2\\), and
    \\(\frac{\partial z}{\partial x_2} = x_1 \frac{dz}{dy}\\).

In code:

```haskell top
mul :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
    -> BVar s a
mul = liftOp2 . op2 $ \x1 x2 ->
    ( x1 * x2, \dzdy -> (dzdy * x2, x1 * dzdy) )
```

For non-trivial examples involving linear algebra, see the source for the *[hmatrix-backprop][]* library.

[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop

Some examples, for the dot product between two vectors and for matrix-vector
multiplication:

```haskell top
-- import qualified Numeric.LinearAlgebra.Static as H

-- | dot product between two vectors
dot
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s (R n)
    -> BVar s Double
dot = liftOp2 . op2 $ \u v ->
    ( u `H.dot` v
    , \dzdy -> (H.konst dzdy * v, u * H.konst dzdy)
    )


-- | matrix-vector multiplication
(#>)
    :: (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (L m n)
    -> BVar s (R n)
    -> BVar s (R m)
(#>) = liftOp2 . op2 $ \mat vec ->
    ( mat H.#> vec
    , \dzdy -> (dzdy `H.outer` vec, H.tr mat H.#> dzdy)
    )
```

### Returning multiple items

You can return tuples inside `BVar`s:

```haskell top
splitAt
    :: (Backprop a, Reifies s W)
    => Int
    -> BVar s [a]
    -> BVar s ([a], [a])
splitAt n = liftOp1 . op1 $ \xs ->
    let (ys, zs) = Data.List.splitAt n xs
    in  ((ys, zs), \(dys,dzs) -> dys ++ dzs)
                      -- assumes dys and dzs have the same lengths as ys and zs
```

This works as expected.  However, it is recommended, for the benefit of your
users, that you return a tuple of `BVar`s instead of a `BVar` of tuples:

```haskell top
splitAt'
    :: (Backprop a, Reifies s W)
    => Int
    -> BVar s [a]
    -> (BVar s [a], BVar s [a])
splitAt' n xs = (yszs ^^. _1, yszs ^^. _2)
  where
    yszs = liftOp1 (op1 $ \xs' ->
        let (ys, zs) = Data.List.splitAt n xs'
        in  ((ys, zs), \(dys,dzs) -> dys ++ dzs)
      ) xs
```

using `_1` and `_2` from the *[microlens][]* or *[lens][]* packages.  This
might also be cleaner if you take advantage of the `T2` or `T3` pattern
synonyms:

[microlens]: http://hackage.haskell.org/package/microlens
[lens]: http://hackage.haskell.org/package/lens

```haskell top
splitAt''
    :: (Backprop a, Reifies s W)
    => Int
    -> BVar s [a]
    -> (BVar s [a], BVar s [a])
splitAt'' n xs = (ys, zs)
  where
    T2 ys zs = liftOp1 (op1 $ \xs' ->
        let (ys, zs) = Data.List.splitAt n xs'
        in  ((ys, zs), \(dys,dzs) -> dys ++ dzs)
      ) xs
```

### Isomorphisms

If your function witnesses an isomorphism, there are handy combinators for
making this easy to write.  This is especially useful in the case of data
constructors:

```haskell top
newtype Foo = MkFoo { getFoo :: Double }
  deriving Generic

instance Backprop Foo

mkFoo
    :: Reifies s W
    => BVar s Double
    -> BVar s Foo
mkFoo = isoVar MkFoo getFoo

-- also:
mkFoo'
    :: BVar s Double
    -> BVar s Foo
mkFoo' = coerceVar          -- requires no `Reifies s W` constraint

data Bar = MkBar { bar1 :: Double, bar2 :: Float }
  deriving Generic

instance Backprop Bar

mkBar
    :: Reifies s W
    => BVar s Double
    -> BVar s Float
    -> BVar s Bar
mkBar = isoVar2 MkBar (\b -> (bar1 b, bar2 b))
```

### NoGrad

If you do decide to go to the extreme, and provide *only* a BVar-based
interface to your library (and no non-BVar based one), then you might have a
situation where you have a function where you cannot define the gradient --
maybe no gradient exists, or you haven't put in the time to write one.  In this
case, you can use `noGrad` and `noGrad1`:

```haskell top
negateNoGrad
    :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
negateNoGrad = liftOp1 (noGrad1 negate)
```

This function can still be used with `evalBP` to get the correct answer.  It
can even be used with `gradBP` if the result is never used in the final answer.

However, if it *is* used in the final answer, then computing the gradient will
throw a runtime exception.

Be sure to warn your users!  Like any partial function, this is not recommended
unless in extreme circumstances.

Monadic Operations
------------------

This should all work if your operations are all "pure".  However, what about
the cases where your operations have to be performed in some Applicative or
Monadic context?

For example, what if `add :: X -> X -> IO X` ?

One option you can do is to newtype-wrap your operations, and then give those a
backprop instance:

```haskell top hide
data X

zeroForX :: X -> X
zeroForX = undefined
addForX  :: X -> X -> IO X
addForX = undefined
oneForX :: X -> X
oneForX = undefined
```

```haskell top
newtype IOX = IOX (IO X)

instance Backprop IOX where
    zero (IOX x) = IOX (fmap zeroForX x)
    -- or, depending on the type of `zeroForX`:
    -- zero (IOX x) = IOX (zeroForX =<< x)

    add (IOX x) (IOX y) = IOX $ do
      x' <- x
      y' <- y
      addForX x' y'

    one (IOX x) = IOX (fmap oneForX x)
```

And you can define your functions in terms of this:

```haskell top
addX
    :: Reifies s W
    => BVar s IOX
    -> BVar s IOX
    -> BVar s IOX
addX = liftOp2 . op2 $ \(IOX x) (IOX y) ->
    ( IOX (do x' <- x; y' <- y; addForX x' y')
    , \dzdy -> (dzdy, dzdy)
    )
```

This should work fine as long as you never "branch" on any *results* of your
actions.  You must not ever need to peek inside the *results* of the action in
order to decide *what* operations to do next.  In other words, this works if
the operations you need to perform are all known and fixed before-hand, before
any actions are performed.  So, this means no access to the `Eq` or `Ord`
instances of BVars (unless your monad has `Eq` or `Ord` instances defined).

A newtype wrapper is provided to give you this behavior automatically -- it's
`ABP`, from *Numeric.Backprop* and *Numeric.Backprop.Class*.

```haskell
type IOX = ABP IO X
```

However, this will not work if you need to do things like compare contents,
etc. to decide what operations to use.

At the moment, this is not supported.  Please open an issue if this becomes an
issue!

Supporting Data Types
---------------------

Your library will probably have data types that you expect your users to use.
To equip your data types for backpropagation, you can take a few steps.

### Backprop Class

First of all, all of your library's types should have instances of the
[`Backprop` typeclass][class].  This allows values of your type to be used in
backpropagatable functions.  See the [Backprop typeclass section][tcdocs] of
this documentation for more information on writing a `Backprop` instance for
your types.

[class]: https://hackage.haskell.org/package/backprop/docs/Numeric-Backprop-Class.html
[tcdocs]: https://backprop.jle.im/04-the-backprop-typeclass.html

In short:

1.  If your type is a type with a single constructor whose fields are all
    instances of `Backprop`, you can just write `instance Backprop MyType`, and
    the instance is generated automatically (as long as your type has a
    `Generic` instance)

    ```haskell top
    data MyType = MkMyType Double [Float] (R 10) (L 20 10) (V.Vector Double)
      deriving Generic

    instance Backprop MyType
    ```

2.  If your type is an instance of `Num`, you can use `zeroNum`, `addNum`, and
    `oneNum` to get free definitions of the typeclass methods.

    ```haskell
    instance Backprop Double where
        zero = zeroNum
        add  = addNum
        one  = oneNum
    ```

3.  If your type is made using a `Functor` instance, you can use `zeroFunctor`
    and `oneFunctor`:

    ```haskell
    instance Backprop a => Backprop (V.Vector a) where
        zero = zeroFunctor
        add  = undefined        -- ??
        one  = oneFunctor
    ```

4.  If your type has an `IsList` instance, you can use `addIsList`:

    ```haskell
    instance Backprop a => Backprop (V.Vector a) where
        zero = zeroFunctor
        add  = addIsList
        one  = oneFunctor
    ```

For more details, see the [aforementioned documentation][tcdocs] or the [actual
typeclass haddock documentation][class].

### Accessors

If you have product types, users should be able to access values inside `BVar`s
of your data type.  There are two main ways to provide access: the lens-based
interface and the higher-kinded-data-based interface.

The lens-based interface gives your users "getter" and "setter" functions for
fields, and the higher-kinded-data-based interface lets your users pattern
match on your data type's original constructor to get fields and construct
values.

#### Lens-Based Interface

If you are defining a product type, like

```haskell top
data MyType = MT { _mtDouble  :: Double
                 , _mtInt     :: Int
                 , _mtDoubles :: [Double]
                 }
```

Users who have a `BVar s MyType` can't normally access the fields inside,
because you can't directly pattern match normally, and the record accessors
are `MyType -> Int` (unlifted).  As a library maintainer, you can provide them
*lenses* to the fields, either generated automatically using the *[lens][]* or
*[microlens-th][]* packages:

[lens]: http://hackage.haskell.org/package/lens
[microlens-th]: http://hackage.haskell.org/package/microlens-th

```haskell top
makeLenses ''MyType
```

or manually by hand:

```haskell top
mtInt' :: Functor f => (Int -> f Int) -> MyType -> f MyType
mtInt' f mt = (\i -> mt { _mtInt = i }) <$> f (_mtInt mt)
```

Now, users can use `^.` or `view` from the *lens* or *[microlens][]* packages
to retrieve your fields:

[microlens]: http://hackage.haskell.org/package/microlens

```haskell
(^. mtDouble)  ::        MyType ->        Double
```

And `(^^.)` and `viewVar` from *backprop* to retrieve fields from a `BVar`:

```haskell
(^^. mtDouble) :: BVar s MyType -> BVar s Double
```

They can also use `set` or `.~` to modify fields, and `setVar` and `.~~` to
modify and "set" fields in a `BVar`:

```haskell
set    mtDouble ::        Double ->        MyType ->        MyType
setVar mtDouble :: BVar s Double -> BVar s MyType -> BVar s MyType
```

#### Higher-Kinded Data Interface

The alternative "Higher-Kinded Data" technique, inspired by [this
article][hkd], allows your users to directly pattern match on `BVar`s of your
types to get their contents.

[hkd]: http://reasonablypolymorphic.com/blog/higher-kinded-data/

Doing this requires modifying the definition of your data types slightly.
Instead of `MyType` above, we can make a type family that can be re-used for
all of your data types:

```haskell top
type family HKD f a where
    HKD Identity a = a
    HKD f        a = f a
```

and define your data types in terms of this type family (remembering to derive
`Generic`):

```haskell top
data MyType2' f = MT2 { mt2Double  :: HKD f Double
                      , mt2Int     :: HKD f Int
                      , mt2Doubles :: HKD f [Double]
                      }
  deriving Generic
```

Now your original data type can be recovered with `MyType2' Identity`, and can
be pattern matched directly in the same way as the original type (the
`Identity` disappears):

```haskell top
type MyType2 = MyType2' Identity

deriving instance Show MyType2
instance Backprop MyType2

getMT2Double :: MyType2 -> Double
getMT2Double (MT2 d _ _) = d
```

But now, users can *pattern match* on a `BVar s MyType2` to get `BVar`s of the
contents, with `splitBV` or the `BV` pattern synonym:

```haskell top
getMT2DoubleBVar
    :: Reifies s W
    => BVar s MyType2
    -> BVar s Double
getMT2DoubleBVar (splitBV -> MT2 d _ _) = d
```

Under `splitBV`, your users can pattern match on the `MT2` constructor and get
the contents as `BVar`s.

Users can also use `joinBV` (or the `BV` pattern synonym in constructor mode)
to re-construct a `BVar` of `MyType2` in terms of `BVar`s of its contents using
the `MT2` constructor:

```haskell top
makeMyType2
    :: Reifies s W
    => BVar s Double
    -> BVar s Int
    -> BVar s [Double]
    -> BVar s MyType2
makeMyType2 d i ds = joinBV $ MT2 d i ds
```

