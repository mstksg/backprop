---
title: Getting Started
---

Getting Started
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
import           Numeric.Backprop.Class
import           Numeric.LinearAlgebra.Static (L, R)
import           System.Random
import qualified Numeric.LinearAlgebra.Static as H
```

*backprop* is a Haskell library available on hackage, so can be used in your
package however way you like to require libraries.  Be sure to add it to your
cabal file's (or package.yaml's) build-depends field.

Automatic Backpropagated Functions
----------------------------------

With *backprop*, you can write your functions in Haskell as normal functions:

```haskell top
import Numeric.Backprop
import Lens.Micro
import Lens.Micro.TH

myFunc x = sqrt (x * 4)
```

They can be run with `evalBP`:

```haskell eval
evalBP myFunc (9 :: Double)
```

And...the twist?  You can also get the gradient of your functions!

```haskell eval
gradBP myFunc (9 :: Double)
```

And that's the gist of the entire library: write your functions to compute your
things, and `gradBP` will give you the gradients and derivatives of those
functions.

### Multiple Same-Type Inputs

Multiple inputs of the same type can be handled with `sequenceVar`:

```haskell top
funcOnList (sequenceVar->[x,y,z]) = sqrt (x / y) * z
```

```haskell eval
evalBP funcOnList [3,5,-2] :: Double
```

```haskell eval
gradBP funcOnList [3,5,-2] :: [Double]
```

Heterogeneous Backprop
----------------------

But the real magic happens when you mix and match types.  Let's make a simple
type representing a feed-forward fully connected artificial neural network with
100 inputs, a single hidden layer of 20 nodes, and 5 outputs:

```haskell top
data Net = N { _nWeights1 :: L 20 100
             , _nBias1    :: R 20
             , _nWeights2 :: L  5  20
             , _nBias2    :: R  5
             }
  deriving (Show, Generic)

instance Backprop Net

makeLenses ''Net
```

using the `L m n` type from the *[hmatrix][]* library to represent an m-by-n
matrix, and the `R n` type to represent an n-vector.

[hmatrix]: http://hackage.haskell.org/package/hmatrix

We can write a function to "run" the network on a `R 100` and get an `R 5`
back, using `^^.` for lens access and `#>` from the *[hmatrix-backprop][]* library for
matrix-vector multiplication:

[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop

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

```haskell top
runNet net x = z
  where
    -- run first layer
    y = logistic $ (net ^^. nWeights1) #> x + (net ^^. nBias1)
    -- run second layer
    z = logistic $ (net ^^. nWeights2) #> y + (net ^^. nBias2)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
```

We can *run* this with a network and input vector:

```haskell top hide
myVector :: R 100
myVector = H.randomVector 93752345 H.Uniform - 0.5

myTarget :: R 5
myTarget = H.randomVector 93752345 H.Uniform - 0.5

myNet :: Net
myNet = N (H.uniformSample 2394834 (-0.5) 0.5)
          (H.randomVector 84783451 H.Uniform - 0.5)
          (H.uniformSample 9293092 (-0.5) 0.5)
          (H.randomVector 64814524 H.Uniform - 0.5)

instance KnownNat n => AskInliterate (R n) where
    askInliterate = answerWith (show . H.extract)
instance AskInliterate Net where
    askInliterate = answerWith (unlines . (++ ["-- ..."]) . take 5 . lines . show)
```

```haskell eval
evalBP2 runNet myNet myVector
```

But --- and here's the fun part --- if we write a "loss function" to evaluate
"how badly" our network has done, using `dot` from the *hmatrix-backprop*
library:

```haskell top
squaredError target output = error `dot` error
  where
    error = target - output
```

we can "test" our networks:

```haskell top
netError target input net = squaredError (auto target)
                                         (runNet net (auto input))
```

(more on `auto` later)

```haskell eval
evalBP (netError myTarget myVector) myNet
```

At this point, we've *written a normal function to compute the error of our
network*.  And, with the backprop library...we now have a way to compute the
*gradient* of our network's error with respect to all of our weights!

```haskell eval
gradBP (netError myTarget myVector) myNet
```

We can now use the gradient to "[train][]" our network to give the correct
responses given a certain input!  This can be done by computing the gradient
for every expected input-output pair, and adjusting the network in the opposite
direction of the gradient every time.

[train]: https://blog.jle.im/entry/purely-functional-typed-models-1.html

Main Idea
---------

The main pattern of usage for this library is:

1.  Write your function normally to compute something (like the loss function)
2.  Use `gradBP` to automatically get the gradient of that something with
    respect to your inputs!

In the case of optimizing models, you:

1.  Write your function normally to compute the thing you want to minimize
2.  Use `gradBP` to automatically get the gradient of the thing you want to
    minimize with respect to your inputs.  Then, adjust your inputs according
    to this gradient until you get the perfect minimal result!

Now that you've had a taste, let's [look at the details][details].  You can
also just go ahead and [jump into the haddock documentation][haddock]!

[details]: https://backprop.jle.im/02-a-detailed-look.html
[haddock]: https://hackage.haskell.org/package/backprop
