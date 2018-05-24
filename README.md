backprop
========

[![backprop on Hackage](https://img.shields.io/hackage/v/backprop.svg?maxAge=86400)](https://hackage.haskell.org/package/backprop)
[![backprop on Stackage LTS 11](http://stackage.org/package/backprop/badge/lts-11)](http://stackage.org/lts-11/package/backprop)
[![backprop on Stackage Nightly](http://stackage.org/package/backprop/badge/nightly)](http://stackage.org/nightly/package/backprop)
[![Build Status](https://travis-ci.org/mstksg/backprop.svg?branch=master)](https://travis-ci.org/mstksg/backprop)

[![Join the chat at https://gitter.im/haskell-backprop/Lobby](https://badges.gitter.im/haskell-backprop/Lobby.svg)](https://gitter.im/haskell-backprop/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Beerpay](https://beerpay.io/mstksg/backprop/badge.svg?style=beer-square)](https://beerpay.io/mstksg/backprop)

[**Introductory blog post**][blog]

[blog]: https://blog.jle.im/entry/introducing-the-backprop-library.html

Automatic *heterogeneous* back-propagation.

Write your functions to compute your result, and the library will automatically
generate functions to compute your gradient.

Differs from [ad][] by offering full heterogeneity -- each intermediate step
and the resulting value can have different types.  Mostly intended for usage
with gradient descent and other numeric optimization techniques.  Comparable to
the python library [autograd][].

[ad]: http://hackage.haskell.org/package/ad
[autograd]: https://github.com/HIPS/autograd

Currently up on [hackage][] (with 100% documentation coverage), but more
up-to-date documentation is currently rendered [on github pages][docs]!  You
can also find help or support on the [gitter channel][gitter].

[hackage]: http://hackage.haskell.org/package/backprop
[docs]: https://mstksg.github.io/backprop
[gitter]: https://gitter.im/haskell-backprop/Lobby

If you want to provide *backprop* for users of your library, see this **[guide
to equipping your library with backprop][library]**.

[library]: https://github.com/mstksg/backprop/wiki/Equipping-your-Library-with-Backprop


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

(This is a really brief version of my [blog post][blog])

The quick example below describes the running of a neural network with one
hidden layer to calculate its squared error with respect to target `targ`,
which is parameterized by two weight matrices and two bias vectors.
Vector/matrix types are from the *hmatrix* package.

Let's make a data type to store our parameters, with convenient accessors using
*[lens][]*:

[lens]: http://hackage.haskell.org/package/lens

```haskell
data Network i h o = Net { _weight1 :: L h i
                         , _bias1   :: R h
                         , _weight2 :: L o h
                         , _bias2   :: R o
                         }

makeLenses ''Network
```

Normally, we might write code to "run" a neural network on an input like this:

```haskell
neuralNet
    :: R i
    -> Network i h o
    -> R h
neuralNet x n = z
  where
    y = logistic $ (n ^. weight1) #> x + (n ^. bias1)
    z = logistic $ (n ^. weight2) #> y + (n ^. bias2)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
```

(`R i` is an i-length vector, `L h i` is an h-by-i matrix, etc., `#>` is
matrix-vector multiplication, and `^.` is access to a field via lens.)

When given an input vector and the network, we compute the result of the neural
network ran on the input vector.

We can write it, instead, using *backprop*:

```haskell
neuralNet
    :: Reifies s W
    => BVar s (R i)
    -> BVar s (Network i h o)
    -> BVar s (R o)
neuralNet x n = z
  where
    y = logistic $ (n ^^. weight1) #> x + (n ^^. bias1)
    z = logistic $ (n ^^. weight2) #> y + (n ^^. bias2)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
```

(`#>!` is a backprop-aware version of `#>`, and `^^.` is access to a field via
lens in a `BVar`)

And that's it!  `neuralNet` is now backpropagatable!

We can "run" it using `evalBP`:

```haskell
evalBP (neuralNet (constVar x)) :: Network i h o -> R o
```

And we can find the gradient using `gradBP`:

```haskell
gradBP (neuralNet (constVar x)) :: Network i h o -> Network i h o
```

If we write a function to compute errors:

```haskell
netError
    :: Reifies s W
    => BVar s (R i)
    -> BVar s (R o)
    -> BVar s (Network i h o)
    -> BVar s Double
netError x targ n = norm_2 (neuralNet x - t)
```

(`norm_2` is a backprop-aware euclidean norm)

Now, we can perform gradient descent!

```haskell
gradDescent
    :: R i
    -> R o
    -> Network i h o
    -> Network i h o
gradDescent x targ n0 = n0 - 0.1 * gradient
  where
    gradient = gradBP (netError (constVar x) (constVar targ)) n0
```

Ta dah!  We were able to compute the gradient of our error function, just by
only saying how to compute *the error itself*.

For a more fleshed out example, see my [blog post][blog] and the [MNIST
tutorial][mnist-lhs] (also [rendered as a pdf][mnist-pdf])

Lens Access
-----------

A lot of the friction of dealing with `BVar s a`s instead of `a`s directly is
alleviated with the lens interface.

With a lens, you can "view" and "set" items inside a `BVar`, as if they were
the actual values:

```haskell
(^.)  ::        a -> Lens' a b ->        b
(^^.) :: BVar s a -> Lens' a b -> BVar s b

(.~)  :: Lens' a b ->        b ->        a ->        a
(.~~) :: Lens' a b -> BVar s b -> BVar s a -> BVar s a
```

And you can also extract multiple potential targets, as well, using
`Traversal`s and `Prism`s:

```haskell
-- | Actually takes a Traversal, to be more general.
-- Can be used to implement "pattern matching" on BVars
(^?)  ::        a -> Prism' a b -> Maybe (       b)
(^^?) :: BVar s a -> Prism' a b -> Maybe (BVar s b)

(^..)  ::        a -> Traversal' a b -> [       b]
(^^..) :: BVar s a -> Traversal' a b -> [BVar s b]
```

Note that the library itself has no *lens* dependency, using *[microlens][]*
instead.

[microlens]: http://hackage.haskell.org/package/microlens

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
