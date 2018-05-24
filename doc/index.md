---
title: Home
---

Welcome to Backprop
===================

Automatic *heterogeneous* back-propagation.

*Write your functions normally* to compute your result, and the library will
*automatically compute your gradient*!

```haskell top hide
import           Numeric.Backprop
```

```haskell eval
gradBP (\x -> x^2 + 3) (9 :: Double)
```

Differs from [ad][] by offering full heterogeneity -- each intermediate step
and the resulting value can have different types (matrices, vectors, scalars,
lists, etc.)

```haskell eval
gradBP2 (\x xs -> sum (map (**2) (sequenceVar xs)) / x)
        (9       :: Double  )
        ([1,6,2] :: [Double])
```

Useful for applications in *differential programming* and deep learning for
creating and training numerical models, especially as described in this blog
post on [a purely functional typed approach to trainable models][models].
Overall, intended for the implementation of gradient descent and other numeric
optimization techniques.  Comparable to the python library [autograd][].

[models]: https://blog.jle.im/entry/purely-functional-typed-models-1.html
[ad]: http://hackage.haskell.org/package/ad
[autograd]: https://github.com/HIPS/autograd

[Get started][getting started] with the introduction and walkthrough!  Full
technical documentation is also [available on hackage][hackage] if you want to
skip the introduction and get right into using the library.  Support is
available on the [gitter channel][gitter]!

[getting started]: https://backprop.jle.im/01-getting-started.html

[hackage]: http://hackage.haskell.org/package/backprop
[gitter]: https://gitter.im/haskell-backprop/Lobby

[![Join the chat at https://gitter.im/haskell-backprop/Lobby](https://badges.gitter.im/haskell-backprop/Lobby.svg)](https://gitter.im/haskell-backprop/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Beerpay](https://beerpay.io/mstksg/backprop/badge.svg?style=beer-square)](https://beerpay.io/mstksg/backprop)

[![backprop on Hackage](https://img.shields.io/hackage/v/backprop.svg?maxAge=86400)](https://hackage.haskell.org/package/backprop)
[![backprop on Stackage LTS 11](http://stackage.org/package/backprop/badge/lts-11)](http://stackage.org/lts-11/package/backprop)
[![backprop on Stackage Nightly](http://stackage.org/package/backprop/badge/nightly)](http://stackage.org/nightly/package/backprop)
[![Build Status](https://travis-ci.org/mstksg/backprop.svg?branch=master)](https://travis-ci.org/mstksg/backprop)

