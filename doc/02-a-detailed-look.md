---
title: A Detailed Look
---

A Detailed Look
===============

```haskell top hide
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}


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

So, what's really going on?

The BVar
--------

The entire library revolves around the `BVar`, a variable holding a
"backpropagatable value".  As you use a `BVar`, the *backprop* library will
track how it is used and where you use it.  You can use `evalBP` to simply get
the result, but using `gradBP` will perform backpropagation ("reverse-mode
[automatic differentiation][autodiff]")

[autodiff]: https://en.wikipedia.org/wiki/Automatic_differentiation

For example, we looked earlier at a function that computes the square root of a
quadrupled number:

```haskell top
myFunc :: Double
       -> Double
myFunc x = sqrt (x * 4)
```

As we are using it, its type is "really":

```haskell top
myFunc' :: Reifies s W
        => BVar s Double
        -> BVar s Double
myFunc' x = sqrt (x * 4)
```

`myFunc'` takes a `BVar s Double` (a `BVar` containing a `Double`) and returns
a new one that is the square root of the quadrupled number.  You can think of
the `Reifies s W` as being a necessary constraint that allows backpropagation
to happen.

`BVar`s have `Num`, `Fractional`, and `Floating` instances, and so can be used
with addition, multiplication, square rooting, etc.  The "most general" type of
`myFunc` is `myFunc :: Floating a => a -> a`, and since `BVar s Double` has a
`Floating` instance, you could even just use it directly as a backpropagatable
function.

This means you can basically treat a `BVar s Double` almost exactly like it was
a `Double` --- you'll practically never tell the difference!  `BVar`s also have
`Ord` and `Eq` instances, so you can compare them and branch on the results,
too.

```haskell top
myAbs :: Reifies s W
      => BVar s Double
      -> BVar s Double
myAbs x | x < 0     = negate x
        | otherwise = x
```

The goal of the `BVar` interface is that you should be able to treat a `BVar s
a` (a `BVar` containing an `a`) as if it was an `a`, with no easily noticeable
differences.

Runners
-------

The entire point of the library is to write your computation as a normal
function taking a `BVar` (or many) and returning a single `BVar`.  Just treat
`BVar`s as if they actually were the value they are containing, and you can't
go wrong.

Once you do this, you can use `evalBP` to "run" the function itself:

```haskell
evalBP :: (forall s. Reifies s W => BVar s a -> BVar s b)
       -> (a -> b)
```

This can be read as taking a `BVar s a -> BVar s b` and returning the `a -> b`
that that function encodes.  The RankN type there (the `forall s.`) is mostly
there to prevent leakage of `BVar`s (same as it is used in *Control.Monad.ST*
and `runST`).  It ensures that no `BVar`s "escape" the function somehow.

`evalBP` is extremely efficient, and usually carries virtually zero overhead
over writing your function directly on your values without `BVar`s.

*But*, the more interesting thing of course is computing the *gradient* of your
function.  This is done with `gradBP`:

```haskell
gradBP :: (Backprop a, Backprop b)
       => (forall s. Reifies s W => BVar s a -> BVar s b)
       -> a
       -> a
```

Which takes a `BVar s a -> BVar s b` backpropagatable function and an input,
and returns *the gradient at that input*.  It gives the direction of greatest
positive change (in the output) of your input, and also how much a variation in
your input will affect your output.

And that's all there is to it!  Instead of `a -> b`'s, write `BVar s a -> BVar
s b`'s to compute what you want to know the gradient of.  These are normal
functions, so you can use all of your favorite higher order functions and
combinators (like `(.)`, `map`, etc.).  And once you're done, use `gradBP` to
compute that gradient.

### Backprop Typeclass

Note that `gradBP` requires a `Backprop` constraint on the input and output of
your function.  `Backprop` is essentially the typeclass of values that can be
"backpropagated".  For product types, this instance is automatically derivable.
But writing your own custom instances for your own types is also fairly
straightforward; more on this later!

Manipulating BVars
------------------

That is the core of the library, from the perspective of the end user.  The
rest of the library is essentially tools to allow you to seamlessly manipulate
`BVar s a`s as if they were just `a`s.

### Constant Values

If we don't *care* about a value's gradient, we can use `auto`:

```haskell
auto :: a -> BVar s a
```

`auto x` basically gives you a `BVar` that contains just `x` alone.  Useful for
using with functions that expect `BVar`s, but you just have a specific value
you want to use.

### Containers

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

Also convenient would be `collectVar`, which essentially does the opposite.

```haskell
collectVar :: (Backprop a, Reifies s W)
           => [BVar s a]
           -> BVar s [a]
```

### Records and Fields


