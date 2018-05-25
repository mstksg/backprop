---
title: Applications and Resources
---

Applications and Resources
==========================

Congratulations!  You are now a *backprop* master.  Maybe you've even looked at
the [haddocks][haddock], which has the technical run-down of all of the
functions and types in this library.  Now what?

*   Check out my [Introducing the backprop library][intro] blog post where I
    announced the library to the world.  In it, I introduce the library by
    building and training a full artificial neural network with it, and use it
    to classify the famous MNIST handwritten digit data set.

*   If you want an even more high-level perspective and inspiration, check out
    my [A Purely Functional Typed Approach to Trainable Models][models] blog
    series, where I talk about how looking at modeling through the lens of
    differentiable programming with purely functional typed code can provide
    new insights and help you develop and train effective models.

*   While they are mostly re-phrasings of the two things above, I also have
    some [example projects as literate haskell files][lhs] on the github
    repository for the library.  These are also [rendered as pdfs][renders] for
    easier reading.

*   If you're doing anything with linear algebra, why not check out the
    *[hmatrix-backprop][]* library, which provides the "backprop-lifted"
    operations that all of the above examples rely on for linear algebra
    operations?

[haddock]: https://hackage.haskell.org/package/backprop
[intro]: https://blog.jle.im/entry/introducing-the-backprop-library.html
[models]: https://blog.jle.im/entry/purely-functional-typed-models-1.html
[lhs]: https://github.com/mstksg/backprop/blob/master/samples
[renders]: https://github.com/mstksg/backprop/tree/master/renders
[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop

This is the end of the "end-user" documentation for *backprop*!  The rest of
all you need to know to use the library is in the **[haddocks on
hackage][haddock]**.

However, if you are a library writer who wants to offer your users the ability
to backpropagate your library functions, let's move on to the **[library
maintainer's guide to equipping your library with backprop][equipping]**!

[equipping]: https://backprop.jle.im/06-equipping-your-library.html
