[backprop][docs]
================

[![backprop on Hackage](https://img.shields.io/hackage/v/backprop.svg?maxAge=86400)](https://hackage.haskell.org/package/backprop)
[![backprop on Stackage LTS 11](http://stackage.org/package/backprop/badge/lts-11)](http://stackage.org/lts-11/package/backprop)
[![backprop on Stackage Nightly](http://stackage.org/package/backprop/badge/nightly)](http://stackage.org/nightly/package/backprop)
[![Build Status](https://travis-ci.org/mstksg/backprop.svg?branch=master)](https://travis-ci.org/mstksg/backprop)

[![Join the chat at https://gitter.im/haskell-backprop/Lobby](https://badges.gitter.im/haskell-backprop/Lobby.svg)](https://gitter.im/haskell-backprop/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Beerpay](https://beerpay.io/mstksg/backprop/badge.svg?style=beer-square)](https://beerpay.io/mstksg/backprop)

[**Documentation and Walkthrough**][docs]

[docs]: https://backprop.jle.im

Automatic *heterogeneous* back-propagation.

Write your functions to compute your result, and the library will automatically
generate functions to compute your gradient.

Differs from [ad][] by offering full heterogeneity -- each intermediate step
and the resulting value can have different types (matrices, vectors, scalars,
lists, etc.).

[ad]: http://hackage.haskell.org/package/ad

Useful for applications in [differentiable programming][dp] and deep learning
for creating and training numerical models, especially as described in this
blog post on [a purely functional typed approach to trainable models][models].
Overall, intended for the implementation of gradient descent and other numeric
optimization techniques.  Comparable to the python library [autograd][].

[dp]: https://www.facebook.com/yann.lecun/posts/10155003011462143
[models]: https://blog.jle.im/entry/purely-functional-typed-models-1.html
[autograd]: https://github.com/HIPS/autograd

Currently up on [hackage][], with haddock documentation!  However, a proper
library introduction and usage tutorial [is available here][docs].  See also my
[introductory blog post][blog].  You can also find help or support on the
[gitter channel][gitter].

[hackage]: http://hackage.haskell.org/package/backprop
[blog]: https://blog.jle.im/entry/introducing-the-backprop-library.html
[gitter]: https://gitter.im/haskell-backprop/Lobby

If you want to provide *backprop* for users of your library, see this **[guide
to equipping your library with backprop][library]**.

[library]: https://backprop.jle.im/06-equipping-your-library.html


MNIST Digit Classifier Example
------------------------------

My [blog post][blog] introduces the concepts in this library in the context of
training a handwritten digit classifier.  I recommend reading that first.

There are some [literate haskell examples][mnist-lhs] in the source, though
([rendered as pdf here][mnist-pdf]), which can be built (if [stack][] is
installed) using:

[mnist-lhs]: https://github.com/mstksg/backprop/blob/master/samples/backprop-mnist.lhs
[mnist-pdf]: https://github.com/mstksg/backprop/blob/master/renders/backprop-mnist.pdf
[stack]: http://haskellstack.org/

```bash
$ ./Build.hs exe
```

There is a follow-up tutorial on using the library with more advanced types,
with extensible neural networks a la [this blog post][blog], [available as
literate haskell][neural-lhs] and also [rendered as a PDF][neural-pdf].

[blog]: https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html
[neural-lhs]: https://github.com/mstksg/backprop/blob/master/samples/extensible-neural.lhs
[neural-pdf]: https://github.com/mstksg/backprop/blob/master/renders/extensible-neural.pdf

Brief example
-------------

(This is a really brief version of [the documentation walkthrough][docs] and my
[blog post][blog])

The quick example below describes the running of a neural network with one
hidden layer to calculate its squared error with respect to target `targ`,
which is parameterized by two weight matrices and two bias vectors.
Vector/matrix types are from the *hmatrix* package.

Let's make a data type to store our parameters, with convenient accessors using
*[lens][]*:

[lens]: http://hackage.haskell.org/package/lens

```haskell
import Numeric.LinearAlgebra.Static.Backprop

data Network = Net { _weight1 :: L 20 100
                   , _bias1   :: R 20
                   , _weight2 :: L  5  20
                   , _bias2   :: R  5
                   }

makeLenses ''Network
```

(`R n` is an n-length vector, `L m n` is an m-by-n matrix, etc., `#>` is
matrix-vector multiplication)

"Running" a network on an input vector might look like this:

```haskell
runNet net x = z
  where
    y = logistic $ (net ^^. weight1) #> x + (net ^^. bias1)
    z = logistic $ (net ^^. weight2) #> y + (net ^^. bias2)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
```

And that's it!  `neuralNet` is now backpropagatable!

We can "run" it using `evalBP`:

```haskell
evalBP2 runNet :: Network -> R 100 -> R 5
```

If we write a function to compute errors:

```haskell
squaredError target output = error `dot` error
  where
    error = target - output
```

we can "test" our networks:

```haskell
netError target input net = squaredError (auto target)
                                         (runNet net (auto input))
```

This can be run, again:

```haskell
evalBP (netError myTarget myVector) :: Network -> Network
```

Now, we just wrote a *normal function to compute the error of our network*.
With the *backprop* library, we now also have a way to *compute the gradient*,
as well!

```haskell
gradBP (netError myTarget myVector) :: Network -> Network
```

Now, we can perform gradient descent!

```haskell
gradDescent
    :: R 100
    -> R 5
    -> Network
    -> Network
gradDescent x targ n0 = n0 - 0.1 * gradient
  where
    gradient = gradBP (netError targ x) n0
```

Ta dah!  We were able to compute the gradient of our error function, just by
only saying how to compute *the error itself*.

For a more fleshed out example, see [the documentaiton][docs], my [blog
post][blog] and the [MNIST tutorial][mnist-lhs] (also [rendered as a
pdf][mnist-pdf])

Benchmarks and Performance
--------------------------

Here are some basic benchmarks comparing the library's automatic
differentiation process to "manual" differentiation by hand.  When using the
[MNIST tutorial][bench] as an example:

[bench]: https://github.com/mstksg/backprop/blob/master/bench/bench.hs

![benchmarks](https://i.imgur.com/7L5NV4P.png)

Here we compare:

1.  "Manual" differentiation of a 784 x 300 x 100 x 10 fully-connected
    feed-forward ANN.
2.  Automatic differentiation using *backprop* and the lens-based accessor
    interface
3.  Automatic differentiation using *backprop* and the "higher-kinded
    data"-based pattern matching interface
4.  A hybrid approach that manually provides gradients for individual layers
    but uses automatic differentiation for chaining the layers together.

We can see that simply *running* the network and functions (using `evalBP`)
incurs virtually zero overhead.  This means that library authors could actually
export *only* backprop-lifted functions, and users would be able to use them
without losing any performance.

As for computing gradients, there exists some associated overhead, from three
main sources.  Of these, the building of the computational graph and the
Wengert Tape wind up being negligible.  For more information, see [a detailed
look at performance, overhead, and optimization techniques][performance] in the
documentation.

[performance]: https://backprop.jle.im/08-performance.html

Note that the manual and hybrid modes almost overlap in the range of their
random variances.

Comparisons
-----------

*backprop* can be compared and contrasted to many other similar libraries with
some overlap:

1.  The *[ad][]* library (and variants like *[diffhask][]*) support automatic
    differentiation, but only for *homogeneous*/*monomorphic* situations.  All
    values in a computation must be of the same type --- so, your computation
    might be the manipulation of `Double`s through a `Double -> Double`
    function.

    *backprop* allows you to mix matrices, vectors, doubles, integers, and even
    key-value maps as a part of your computation, and they will all be
    backpropagated properly with the help of the `Backprop` typeclass.

2.  The *[autograd][]* library is a very close equivalent to *backprop*,
    implemented in Python for Python applications.  The difference between
    *backprop* and *autograd* is mostly the difference between Haskell and
    Python --- static types with type inference, purity, etc.

3.  There is a link between *backprop* and deep learning/neural network
    libraries like *[tensorflow][]*, *[caffe][]*, and *[theano][]*, which all
    all support some form of heterogeneous automatic differentiation.  Haskell
    libraries doing similar things include *[grenade][]*.

    These are all frameworks for working with neural networks or other
    gradient-based optimizations --- they include things like built-in
    optimizers, methods to automate training data, built-in models to use out
    of the box.  *backprop* could be used as a *part* of such a framework, like
    I described in my [A Purely Functional Typed Approach to Trainable
    Models][models] blog series; however, the *backprop* library itself does
    not provide any built in models or optimizers or automated data processing
    pipelines.

[diffhask]: https://hackage.haskell.org/package/diffhask
[tensorflow]: https://www.tensorflow.org/
[caffe]: http://caffe.berkeleyvision.org/
[theano]: http://www.deeplearning.net/software/theano/
[grenade]: http://hackage.haskell.org/package/grenade

See [documentation][comparisons] for a more detailed look.

[comparisons]: https://backprop.jle.im/07-comparisons.html

Todo
----

1.  Benchmark against competing back-propagation libraries like *ad*, and
    auto-differentiating tensor libraries like *[grenade][]*

    [grenade]: https://github.com/HuwCampbell/grenade

2.  Write tests!

3.  Explore opportunities for parallelization.  There are some naive ways of
    directly parallelizing right now, but potential overhead should be
    investigated.

4.  Some open questions:

    a.  Is it possible to support constructors with existential types?

    b.  How to support "monadic" operations that depend on results of previous
        operations? (`ApBP` already exists for situations that don't)

    c.  What needs to be done to allow us to automatically do second,
        third-order differentiation, as well?  This might be useful for certain
        ODE solvers which rely on second order gradients and hessians.
