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

Useful for applications in *[differential programming][dp]* and deep learning for
creating and training numerical models, especially as described in this blog
post on [a purely functional typed approach to trainable models][models].
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

Benchmarks
----------

Here are some basic benchmarks comparing the library's automatic
differentiation process to "manual" differentiation by hand.  When using the
[MNIST tutorial][bench] as an example:

[bench]: https://github.com/mstksg/backprop/blob/master/bench/MNISTBench.hs

![benchmarks](https://i.imgur.com/9DXUaOI.png)

*   For computing the gradient, there is about a 2.5ms overhead (or about 3.5x)
    compared to computing the gradients by hand.  Some more profiling and
    investigation can be done, since there are two main sources of potential
    slow-downs:

    1.  "Inefficient" gradient computations, because of automated
        differentiation not being as efficient as what you might get from doing
        things by hand and simplifying.  This sort of cost is probably not
        avoidable.
    2.  Overhead incurred by the book-keeping and actual automatic
        differentiating system, which involves keeping track of a dependency
        graph and propagating gradients backwards in memory.  This sort of
        overhead is what we would be aiming to reduce.

    It is unclear which one dominates the current slowdown.

*   However, it may be worth noting that this isn't necessarily a significant
    bottleneck.  *Updating* the networks using *hmatrix* actually dominates the
    runtime of the training.  Manual gradient descent takes 3.2ms, so the extra
    overhead is about 60%-70%.

*   Running the network (and the backprop-aware functions) incurs virtually
    zero overhead (about 4%), meaning that library authors could actually
    export backprop-aware functions by default and not lose any performance.

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
