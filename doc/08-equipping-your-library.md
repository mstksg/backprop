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
import           GHC.Generics (Generic)
import           Lens.Micro
import           Lens.Micro.TH
import           Numeric.Backprop
import           Numeric.Backprop.Class
import           System.Random
import qualified Data.List
import qualified Data.Vector                  as V
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

Lifting operations manually
---------------------------

However, if no lifted primitive functions are available, then you do have to do
some legwork to provide information on gradient computation for your types.
Ideally, you would only need to do this for some minimal set of your
operations, and then define the rest of them in terms of the functions you have
already lifted.

A full tutorial on lifting your library functions [can be found
here][manual-gradients].  It describes the usage of the `liftOp` and `op`
family of functions to fully lift your single-argument single-result and
multiple-argument single-result functions to be backpropagatable.

[manual-gradients]: https://backprop.jle.im/06-manual-gradients.html

### Returning multiple items

As an extension of the [manual gradient tutorial][manual-gradients], we can
consider functions that return multiple items.

You can always return tuples inside `BVar`s:

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

Note also that if you have a newtype with one constructor (or any other two
`Coercible` types), you can simply use `coerceVar`:

```haskell top
mkFoo'
    :: BVar s Double
    -> BVar s Foo
mkFoo' = coerceVar          -- requires no `Reifies s W` constraint
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
-- requires -XTemplateHaskell
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

Likewise, `over` and `%~` can be used to apply a function to the contents of a
field, and `overVar` and `%~~` can be used to apply backpropagatable functions
to over fields of a value in a `BVar`.

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

Note that HKD access through pattern matching is potentially less performant
than access using lens (by about 10-20%).

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
