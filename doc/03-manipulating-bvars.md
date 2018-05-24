---
title: Manipulating BVars
---

Manipulating BVars
==================

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
import           GHC.TypeNats
import           Inliterate.Import
import           Lens.Micro
import           Lens.Micro.TH
import           Numeric.Backprop
import           Numeric.Backprop.Class
import           Numeric.LinearAlgebra.Static (L, R)
import           System.Random
import qualified Numeric.LinearAlgebra.Static as H
```

The most important aspect of the usability of this library is allowing you to
seamlessly manipulate `BVar s a`s as if they were just `a`s, without requiring
you as the user to be able to recognize or acknowledge the difference.  Here
are some techniques to that end.

Typeclass Interface
-------------------

`BVar`s have `Num`, `Fractional`, `Floating`, `Eq`, and `Ord` instances.  These
instances are basically "lifted" to the `BVar` itself, so if you have a `BVar s
Double`, you can use `(*)`, `sqrt`, `(>)`, etc. on it exactly as if it were
just a `Double`.

Constant Values
---------------

If we don't *care* about a value's gradient, we can use `auto`:

```haskell
auto :: a -> BVar s a
```

`auto x` basically gives you a `BVar` that contains just `x` alone.  Useful for
using with functions that expect `BVar`s, but you just have a specific value
you want to use.

Coercible
---------

If `a` and `b` are `Coercible`, then so are `BVar s a` and `BVar s b`, using
the `coerceVar` function.  This is useful for "unwrapping" and "wrapping"
`BVar`s of newtypes:


```haskell top
newtype MyInt = MyInt Int

getMyInt :: BVar s MyInt -> BVar s Int
getMyInt = coerceVar
```

Accessing Contents
------------------

The following techniques can be used to access values inside `BVar`s:

### Traversable Containers

One that we saw earlier was `sequenceVar`, which we used to turn a `BVar`
containing a list into a list of `BVar`s:

```haskell
sequenceVar :: (Backprop a, Reifies s W)
            => BVar s [a]
            -> [BVar s a]
```

If you have a `BVar` containing a list, you can get a list of `BVar`s of all of
that list's elements.  (`sequenceVar` actually works on all `Traversable`
instances, not just lists)  This is very useful when combined with
`-XViewPatterns`, as seen earlier.

### Records and Fields

In practice, a lot of usage involves functions involving contents of records or
data types containing fields.  The previous example, involving a simple ANN,
demonstrates this:

```haskell top
data Net = N { _nWeights1 :: L 20 100
             , _nBias1    :: R 20
             , _nWeights2 :: L  5  20
             , _nBias2    :: R  5
             }
  deriving (Show, Generic)

instance Backprop Net       -- can be automatically defined
```

To compute the result of this network (ran on an `R 100`, a 100-vector) and get
the output `R 5`, we need do a matrix multiplication by the `_nWeights1` field,
add the result to the `_nBias1` field...basically, the result is a function of
linear algebra and related operations on the input and all of the contents of
the `Net` data type.  However, you can't directly use `_nWeights`, since it
takes a `Net`, not `BVar s Net`.  And you also can't directly pattern match on
the `N` constructor.

There are two main options for this: the lens interface, and the higher-kinded
data interface.

#### Lens Interface

The most straightforward way to do this is the lens-based interface, using
`viewVar` or `^^.`.

If we make lenses for `Net` using the *[lens][]* or *[microlens-th][]* packages:

[lens]: http://hackage.haskell.org/package/lens
[microlens-th]: http://hackage.haskell.org/package/microlens-th

```haskell top
makeLenses ''Net
```

Then `^.` from the *lens* or *[microlens][]* packages lets you retrieve a field
from a `Net`:

[microlens]: http://hackage.haskell.org/package/microlens

```haskell
(^. nWeights1) :: Net -> L 20 100
(^. nBias1   ) :: Net -> R 20
(^. nWeights2) :: Net -> L  5  20
(^. nBias2   ) :: Net -> R  5
```

And, `^^.` from *backprop* (also aliased as `viewVar`) lets you do the same
thing from a `BVar s Net` (a `BVar` containing your `Net`):

```haskell
(^^. nWeights1) :: BVar s Net -> BVar s (L 20 100)
(^^. nBias1   ) :: BVar s Net -> BVar s (R 20)
(^^. nWeights2) :: BVar s Net -> BVar s (L  5  20)
(^^. nBias2   ) :: BVar s Net -> BVar s (R  5)
```

```haskell top hide
instance Backprop (R n) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance (KnownNat n, KnownNat m) => Backprop (L n m) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

(#>)
    :: (KnownNat n, KnownNat m, Reifies s W)
    => BVar s (L n m) -> BVar s (R m) -> BVar s (R n)
(#>) = liftOp2 . op2 $ \xs y ->
    ( xs H.#> y
    , \d -> (d `H.outer` y, H.tr xs H.#> d)
    )

dot :: (KnownNat n, Reifies s W) => BVar s (R n) -> BVar s (R n) -> BVar s Double
dot = liftOp2 . op2 $ \x y ->
    ( x `H.dot` y
    , \d -> let d' = H.konst d
            in  (d' * y, x * d')
    )
```

With our lenses and `^^.`, we can write our network running function.  This
time, I'll include the type!

```haskell top
runNet :: Reifies s W
       => BVar s Net
       -> BVar s (R 100)
       -> BVar s (R 5)
runNet net x = z
  where
    -- run first layer
    y = logistic $ (net ^^. nWeights1) #> x + (net ^^. nBias1)
    -- run second layer
    z = logistic $ (net ^^. nWeights2) #> y + (net ^^. nBias2)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
```

Note that we are using versions of `#>` lifted for `BVar`s, from the
*[hmatrix-backprop][]* library:

```haskell
(#>) :: BVar s (L m n) -> BVar s (R n) -> BVar s (R m)
```

[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop

#### Higher-Kinded Data Interface

Using the lens based interface, you can't directly pattern match and construct
fields.  To allow for directly pattern matching, there's another interface
option involving the "Higher-Kinded Data" techniques described in [this
article][hkd].

[hkd]: http://reasonablypolymorphic.com/blog/higher-kinded-data/

If we had a type-family (that can be re-used for all of your data types):

```haskell top
type family HKD f a where
    HKD Identity a = a
    HKD f        a = f a
```

We can define `Net` instead as:

```haskell top
data Met' f = M { _mWeights1 :: HKD f (L 20 100)
                , _mBias1    :: HKD f (R 20)
                , _mWeights2 :: HKD f (L  5  20)
                , _mBias2    :: HKD f (R  5)
                }
  deriving Generic
```

Then our *original* type is:

```haskell top
type Met = Met' Identity

deriving instance Show Met
instance Backprop Met
```

`Met` is the same as `Net` in every way -- it can be pattern matched on to get
the `L 20 100`, etc.

The benefit of this is that we can now directly pattern match on a `BVar s Met`
to get the internal fields as `BVar`s using `splitBV` as a view pattern (or the
`BV` pattern synonym):

```haskell top
runMet :: Reifies s W
       => BVar s Met
       -> BVar s (R 100)
       -> BVar s (R 5)
runMet (splitBV -> M w1 b1 w2 b2) x = z
  where
    -- run first layer
    y = logistic $ w1 #> x + b1
    -- run second layer
    z = logistic $ w2 #> y + b2
```

Now, the `M w1 b1 w2 b2` pattern can be used to deconstruct *both* "normal"
`Met`s, as well as a `BVar s Met` (with `splitBV`).

### Potential or Many Fields

Some values "may" or "may not" have values of a given field.  An example would
include the nth item in a list or vector, or the `Just` of a `Maybe`.

For these, the lens-based (prism-based/traversal-based) interface is the main way to access
partial fields.  You can use `(^^?)` or `previewVar` with any `Traversal`:

```haskell
(^?)  ::        a -> Traversal' a b -> Maybe         b
(^^?) :: BVar s a -> Traversal' a b -> Maybe (BVar s b)
```

If the value in the `BVar` "has" that field, then you'll get a `Just` with the
`BVar` of that field's contents.  If it doesn't, you'll get a `Nothing`.

You can use this with any prism or traversal, like using `_head` to get the
first item in a list if it exists.

If you have a type that might contain *many* values of a field (like a tree or
list), you can use `(^^..)` or `toListOfVar`, which works on any `Traversal`:

```haskell
(^..)  ::        a -> Traversal' a b -> [b]
(^^..) :: BVar s a -> Traversal' a b -> [BVar s b]
```

This can be used to implement `sequenceVar`, actually:

```haskell
sequenceVar :: BVar s [a] -> [BVar s a]
sequenceVar xs = xs ^^.. traverse
```

### Tuples

The `T2` pattern is provided, which allow you to pattern match on a
`BVar s (a, b)` to get a `BVar s a` and `BVar s b`.  The `T3` pattern is also
provided, which does the same thing for three-tuples.

Combining BVars
---------------

The following techniques can be used to "combine" `BVar`s:

### Foldable Containers

The "opposite" of `sequenceVar` is `collectVar`, which takes a foldable
container of `BVar`s and returns a `BVar` containing that foldable container of
contents:

```haskell
collectVar :: (Backprop a, Foldable t, Functor t, Reifies s W)
           => t (BVar s a)
           -> BVar s (t a)
```

### Constructors

Sometimes you would like to combine a bunch of `BVar`s into a `BVar` of
specific container or data type.

#### isoVar

The simplest way to do this is using the `isoVar`, `isoVar2`, etc. family of
functions:

```haskell
isoVar2
    :: (Backprop a, Backprop b, Backprop c, Reifies s W)
    => (a -> b -> c)
    -> (c -> (a, b))
    -> BVar s a
    -> BVar s b
    -> BVar s c
```

So if we had a type like:

```haskell top
data DoubleInt = DI Double Int
```

We can combine a `Double` and `Int` into a `DoubleInt` using `isoVar2`:

```haskell
isoVar2 DI (\(DI x y) -> (x,y))
    :: BVar s Double -> BVar s Int -> BVar s DoubleInt
```

#### Higher-Kinded Data Interface

You can also use the ["Higher Kinded Data"][hkd] interface, as well.  For our
`Met` type above, you can use `joinBV`, or the `BV` pattern synonym:

```haskell top
makeMet :: Reifies s W
        => BVar s (L 20 100)
        -> BVar s (R 20)
        -> BVar s (L  5  20)
        -> BVar s (R  5)
        -> BVar s Met
makeMet w1 b1 w2 b2 = joinBV (M w1 b1 w2 b2)
```

### Modifying fields

If you just want to "set" a specific field, you can use the lens-based
interface with `(.~~)` or `setVar`.  For example, if we wanted to set the
`_nWeights2` field of a `Net` to a new matrix, we can do:

```haskell
myNet & nWeights2 .~~ newMatrix
```

or

```haskell
setVar nWeights2 :: BVar s (L 20 5) -> BVar s Net -> BVar s Net
```

### Tuples

The `T2` and `T3` patterns can also be used to turn, say, a `BVar s a` and
`BVar s b` into a `BVar s (a, b)`, for convenience.

Prelude Modules
---------------

Finally, the *Prelude.Backprop* module has a lot of your normal Prelude
functions "lifted" to work on `BVar`s of values.  For many situations, these
aren't necessary, and normal Prelude functions will work just fine on `BVar`s
of values (like `(.)`).  However, it does have some convenient functions, like
`minimum`, `foldl'`, `fmap`, `toList`, `fromIntegral`, `realToFrac`, etc.
lifted to work on `BVar`s.  This module is meant to be imported qualified.
