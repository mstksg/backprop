backprop
========

[![Build Status](https://travis-ci.org/mstksg/backprop.svg?branch=master)](https://travis-ci.org/mstksg/backprop)

[**Literate Haskell Tutorial/Demo on MNIST data set**][mnist-lhs]

Automatic *heterogeneous* back-propagation that can be used either *implicitly*
(in the style of the [ad][] library) or using *explicit* graphs built in
monadic style.  Implements reverse-mode automatic differentiation.  Differs
from [ad][] by offering full heterogeneity -- each intermediate step and the
resulting value can have different types.  Mostly intended for usage with
tensor manipulation libraries to implement automatic back-propagation for
gradient descent and other optimization techniques.

[ad]: http://hackage.haskell.org/package/ad

Documentation is currently rendered [on github pages][docs]!

[docs]: https://mstksg.github.io/backprop

MNIST Digit Classifier Example
------------------------------

Tutorial and example on training on the MNIST data set [available here as a
literate haskell file][mnist-lhs], or [rendered here as a PDF][mnist-pdf]!
**Read this first!**

[mnist-lhs]: https://github.com/mstksg/backprop/blob/master/samples/MNIST.lhs
[mnist-pdf]: https://github.com/mstksg/backprop/blob/master/renders/MNIST.pdf


Brief example
-------------

The quick example below describes the running of a neural network with one
hidden layer to calculate its squared error with respect to target `targ`,
which is parameterized by two weight matrices and two bias vectors.
Vector/matrix types are from the *hmatrix* package.

~~~haskell
logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

neuralNetImplicit
      :: (KnownNat m, KnownNat n, KnownNat o)
      => R m
      -> BPOpI s '[ L n m, R n, L o n, R o ] (R o)
neuralNetImplicit inp = \(w1 :< b1 :< w2 :< b2 :< Ø) ->
    let z = logistic (liftB2 matVec w1 x + b1)
    in  logistic (liftB2 matVec w2 z + b2)
  where
    x = constRef inp

neuralNetExplicit
      :: (KnownNat m, KnownNat n, KnownNat o)
      => R m
      -> BPOp s '[ L n m, R n, L o n, R o ] (R o)
neuralNetExplicit inp = withInps $ \(w1 :< b1 :< w2 :< b2 :< Ø) -> do
    y1  <- matVec ~$ (w1 :< x1 :< Ø)
    let x2 = logistic (y1 + b1)
    y2  <- matVec ~$ (w2 :< x2 :< Ø)
    return $ logistic (y2 + b2)
  where
    x1 = constVar inp
~~~

Now `neuralNetExplicit` and `neuralNetImplicit` can be "run" with the input
vectors and parameters (a `L n m`, `R n`, `L o n`, and `R o`) and calculate the
output of the neural net.

~~~haskell
runNet
    :: (KnownNat m, KnownNat n, KnownNat o)
    => R m
    -> Tuple '[ L n m, R n, L o n, R o ]
    -> R o
runNet inp = evalBPOp (neuralNetExplicit inp)
~~~

But, in defining `neuralNet`, we also generated a graph that *backprop* can
use to do backpropagation, too!

~~~haskell
netGrad
    :: forall m n o. (KnownNat m, KnownNat n, KnownNat o)
    => R m
    -> R o
    -> Tuple '[ L n m, R n, L o n, R o ]
    -> Tuple '[ L n m, R n, L o n, R o ]
netGrad inp targ params = gradBPOp opError params
  where
    -- calculate squared error, in *explicit* style
    -- (implicit style also possible)
    opError :: BPOp s '[ L n m, R n, L o n, R o ] Double
    opError = do
        res <- neuralNetExplicit inp
        err <- bindRef (res - t)
        dot ~$ (err :< err :< Ø)
      where
        t = constRef targ
~~~

The result is the gradient of the input tuple's components, with respect
to the `Double` result of `opError` (the squared error).  We can then use
this gradient to do gradient descent.

For a more fleshed out example, see the [MNIST tutorial][mnist-lhs] (also
[rendered as a pdf][mnist-pdf])

What's Next
-----------

1.  Benchmarks against numeric algorithms implemented from scratch, to gauge
    how much overhead this library adds
2.  Benchmarks against other automatic differentiation libraries.
3.  Open question: can we offer pattern matching/sum type support for
    "implicit-mode" backprop?  It's already possible for the explicit-graph
    mode backprop.
4.  Consider shifting over the implementation to use `unsafePerformIO` like the
    *ad* library does to leverage some interesting benefits.
