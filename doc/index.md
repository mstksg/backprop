---
title: Home
---

Backprop
========

Automatic *heterogeneous* back-propagation.

Write your functions to compute your result, and the library will automatically
generate functions to compute your gradient.  Useful for applications in
*differential programming* and deep learning.

Differs from [ad][] by offering full heterogeneity -- each intermediate step
and the resulting value can have different types.  Mostly intended for usage
with gradient descent and other numeric optimization techniques.  Comparable to
the python library [autograd][].

[ad]: http://hackage.haskell.org/package/ad
[autograd]: https://github.com/HIPS/autograd

Be sure to check out [documentation on hackage][hackage].  Support is also
available on the [gitter channel][gitter]!

[hackage]: http://hackage.haskell.org/package/backprop
[gitter]: https://gitter.im/haskell-backprop/Lobby
