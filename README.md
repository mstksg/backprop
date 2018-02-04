backprop
========

[![backprop on Hackage](https://img.shields.io/hackage/v/backprop.svg?maxAge=2592000)](https://hackage.haskell.org/package/backprop)
[![Build Status](https://travis-ci.org/mstksg/backprop.svg?branch=master)](https://travis-ci.org/mstksg/backprop)

[**Literate Haskell Tutorial/Demo on MNIST data set**][mnist-lhs] (and [PDF
rendering][mnist-pdf])

Automatic *heterogeneous* back-propagation.

Write your functions to compute your result, and the library will automatically
generate functions to compute your gradient.

Differs from [ad][] by offering full heterogeneity -- each intermediate step
and the resulting value can have different types.  Mostly intended for usage
with gradient descent and other numeric optimization techniques.

[ad]: http://hackage.haskell.org/package/ad

Currently up on [hackage][] (with 100% documentation coverage), but more
up-to-date documentation is currently rendered [on github pages][docs]!

[hackage]: http://hackage.haskell.org/package/backprop
[docs]: https://mstksg.github.io/backprop

MNIST Digit Classifier Example
------------------------------

Tutorial and example on training on the MNIST data set [available here as a
literate haskell file][mnist-lhs], or [rendered here as a PDF][mnist-pdf]!
**Read this first!**

[mnist-lhs]: https://github.com/mstksg/backprop/blob/master/samples/backprop-mnist.lhs
[mnist-pdf]: https://github.com/mstksg/backprop/blob/master/renders/backprop-mnist.pdf

The [literate haskell file][mnist-lhs] is a standalone haskell file that you
can compile (preferably with `-O2`) on its own with stack or some other
dependency manager.  It can also be compiled with the build script in the
project directory (if [stack][] is installed, and appropriate dependencies are
installed), using

[stack]: http://haskellstack.org/

```bash
$ ./Build.hs exe
```

Brief example
-------------

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
    y = logistic $ (n ^^. weight1) #>! x + (n ^^. bias1)
    z = logistic $ (n ^^. weight2) #>! y + (n ^^. bias2)

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
netError x targ n = sum' (err <.>! err)
  where
    err = neuralNet x - t
```

(`sum'` is a backprop-aware vector sum, and `<.>!` is a backprop-aware dot
product)

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

For a more fleshed out example, see the [MNIST tutorial][mnist-lhs] (also
[rendered as a pdf][mnist-pdf])

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

    a. Is it meaningful to support sum types?
    b. Is it possible to support constructors with existential types?
