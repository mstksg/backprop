---
title: Comparisons
---

Comparisons
===========

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

[ad]: https://hackage.haskell.org/package/ad
[diffhask]: https://hackage.haskell.org/package/diffhask
[autograd]: https://github.com/HIPS/autograd
[tensorflow]: https://www.tensorflow.org/
[caffe]: http://caffe.berkeleyvision.org/
[theano]: http://www.deeplearning.net/software/theano/
[grenade]: http://hackage.haskell.org/package/grenade
[models]: https://blog.jle.im/entry/purely-functional-typed-models-1.html
