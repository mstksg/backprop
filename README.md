backprop
========

[![backprop on Hackage](https://img.shields.io/hackage/v/backprop.svg?maxAge=2592000)](https://hackage.haskell.org/package/backprop)
[![Build Status](https://travis-ci.org/mstksg/backprop.svg?branch=master)](https://travis-ci.org/mstksg/backprop)

[**Literate Haskell Tutorial/Demo on MNIST data set**][mnist-lhs] (and [PDF
rendering][mnist-pdf])

Automatic *heterogeneous* back-propagation that can be used either *implicitly*
(in the style of the [ad][] library) or using *explicit* graphs built in
monadic style.  Implements reverse-mode automatic differentiation.  Differs
from [ad][] by offering full heterogeneity -- each intermediate step and the
resulting value can have different types.  Mostly intended for usage with
tensor manipulation libraries to implement automatic back-propagation for
gradient descent and other optimization techniques.

[ad]: http://hackage.haskell.org/package/ad

Currently up on [hackage][] (with 100% documentation coverage), but more
up-to-date documentation is currently rendered [on github pages][docs]!

[hackage]: http://hackage.haskell.org/package/backprop
[docs]: https://mstksg.github.io/backprop

At the moment this project is in **pre-alpha** (*v0.0.1.0*), and is
published/put up on Hackage as a call for comments and thoughts.  It has 100%
documentation coverage at the moment.  Performance was not yet a priority
before this, but will be from now on.  (Previously, highest priority was
API/usability). See [the todos section][todos] for more information on what's
missing, and how one would be able to contribute!

[todos]: https://github.com/mstksg/backprop#todo

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

matVec
    :: (KnownNat m, KnownNat n)
    => Op '[ L m n, R n ] (R m)

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
use to do back-propagation, too!

~~~haskell
dot :: KnownNat n
    => Op '[ R n  , R n ] Double

netGrad
    :: forall m n o. (KnownNat m, KnownNat n, KnownNat o)
    => R m
    -> R o
    -> Tuple '[ L n m, R n, L o n, R o ]
    -> Tuple '[ L n m, R n, L o n, R o ]
netGrad inp targ params = gradBPOp opError params
  where
    -- calculate squared error, in *explicit* style
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

Benchmarks
----------

The current version isn't optimized, but here are some basic benchmarks
comparing the library's automatic differentiation process to "manual"
differentiation by hand.  When using the [MNIST tutorial][mnist-lhs] as an
example:

![benchmarks](http://i.imgur.com/xIZbhHa.png)

Calculating the gradient using *backprop* and calculating it by hand (by manual
symbolic differentiation) are within an order of magnitude of each-other,
time-wise.  Using the *backprop* library takes about *6.5x* as long
in this case.

However, a full *update* step (calculate the gradient and update the neural
net) adds a lot of constant costs, so for a full training step, the *backprop*
library takes only *2.7x* as long as manual symbolic differentation.

This means that this library is only about 2.5x slower than implementing it
directly using only *hmatrix*.

It's still definitely not ideal that more than half of the computation time is
overhead from the library, but this is just where we stand at the moment.
Optimization is just now starting!

Todo
----

1.  Profiling, to gauge where the overhead comes from (compared to "manual"
    back-propagation) and how to bring it down.

2.  Some simple performance and API tweaks that are probably possible now and
    would clearly benefit: (if you want to contribute)

    a.  ~~Providing optimized `Num`/`Fractional`/`Floating` instances for `BVal`
        by supplying known gradients directly instead of relying on *ad*.~~
        (Now finished, since [b3898ae][optnum])

[optnum]: https://github.com/mstksg/backprop/commit/b3898ae676b8048e03709fb5d3d38a6fedb48e1e

    b.  Switch from `ST s` to `IO`, and use `unsafePerformIO` to automatically
        bind `BVal`s (like *ad* does) when using `liftB`.  This might remove
        some overhead during graph building, and, from an API standpoint,
        remove the need for explicit binding.

    c.  Switch from `STRef`s/`IORef`s to `Array`.  (This one I'm unclear if it
        would help any)

3.  Benchmark against competing back-propagation libraries like *ad*, and
    auto-differentiating tensor libraries like *[grenade][]*

[grenade]: https://github.com/HuwCampbell/grenade

4.  Explore opportunities for parallelization.  There are some naive ways of
    directly parallelizing right now, but potential overhead should be
    investigated.

5.  Some open questions:

    a.  Is it possible to offer pattern matching on sum types/with different
        constructors for implicit-graph backprop?  It's possible for
        explicit-graph versions already, with `choicesVar`, but not yet with
        the implicit-graph interface.  Could be similar to an "Applicative vs.
        Monad" issue where you can only have pre-determined fixed computation
        paths when using `Applicative`, but I'm not sure.  Still, it would be
        nice, because if this was possible, we could possibly do away with
        explicit-graph mode completely.

    b.  Though we already have sum type support with explicit-graph mode, we
        can't support GADTs yet.  It'd be nice to see if this is possible,
        because a lot of dependently typed neural network stuff is made much
        simpler with GADTs.
