---
title: Manual Gradients
---

Providing Hand-Written Gradients
================================

```haskell top hide
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}


import           Data.Functor.Identity
import qualified Data.List
import           GHC.Generics (Generic)
import           GHC.TypeNats
import           Inliterate.Import
import           Lens.Micro
import           Lens.Micro.TH
import           Numeric.Backprop
import           Numeric.Backprop.Class
import           Numeric.LinearAlgebra.Static (L, R, konst)
import           System.Random
import qualified Data.Vector                  as V
import qualified Numeric.LinearAlgebra.Static as H
import qualified Numeric.LinearAlgebra        as HU
```

Providing and writing hand-written gradients for operations can be useful if
you are [peforming low-level optimizations][performance] or [equipping your
library for backprop][equipping].

[performance]: https://backprop.jle.im/07-performance.html
[equipping]: https://backprop.jle.im/08-equipping-your-library.html

Ideally, as an *end user*, you should never have to do this.  The whole point
of the *backprop* library is to allow you to use backpropagatable functions as
normal functions, and to let you build complicated functions by simply
composing normal Haskell functions, where the *backprop* library automatically
infers your gradients.

However, if you are writing a library, you probably need to provide "primitive"
backpropagatable functions (like matrix-vector multiplication for a linear
algebra library) for your users, so your users can then use those primitive
functions to write their own code, without ever having to be aware of any
gradients.

If you are writing code and recognize some bottlenecks related to library
overhead as [described in this post][performance], then you might also want to
provide manual gradients as a last resort.  However, this should always be a
last resort, as *figuring out* manual gradients is a tedious and error-prone
process that can introduce subtle bugs in ways that don't always appear in
testing.  It also makes your code much more fragile and difficult to refactor
and shuffle around (since you aren't using normal function composition and
application anymore) and much harder to read.  Only proceed if you decide that
the huge cognitive costs are worth it.

The Lifted Function
-------------------

A lifted function of type

```haskell
myFunc :: Reifies s W => BVar s a -> BVar s b
```

represents a backpropagatble function taking an `a` and returning a `b`.  It is
represented as a function taking a `BVar` containing an `a` and returning a
`BVar` containing a `b`; the `BVar s` with the `Reifies s W` is what allows for
tracking of backpropagation.

A `BVar s a -> BVar s b` is really, actually, under the hood:

```haskell
type BVar s a -> BVar s b
    = a -> (b, b -> a)
```

That is, given an input `a`, you get:

1.  A `b`, the result (the "forward pass")
2.  A `b -> a`, the "scaled gradient" function.

A full technical description is given in the documentation for [Numeric.Backprop.Op][op].

[op]: http://hackage.haskell.org/package/backprop/docs/Numeric-Backprop-Op.html

The `b` result is simple enough; it's the result of your function.  The "scaled
gradient" function requires some elaboration.  Let's say you are writing a
lifted version of your function \\(y = f(x)\\) (whose derivative is
\\(\frac{dy}{dx}\\)), and that your *final result* at the end of your
computation is \\(z = g(f(x))\\) (whose derivative is \\(\frac{dz}{dx}\\)).  In
that case, because of the chain rule, \\(\frac{dz}{dx} = \frac{dz}{dy}
\frac{dy}{dx}\\).

The scaled gradient `b -> a` is the function which, *given*
\\(\frac{dy}{dz}\\) `:: b`, *returns* \\(\frac{dz}{dx}\\) `:: a`. (that is,
returns \\(\frac{dz}{dy} \frac{dy}{dx}\\) `:: a`).

For example, for the mathematical operation \\(y = f(x) = x^2\\), then,
considering \\(z = g(f(x))\\), \\(\frac{dz}{dx} = \frac{dz}{dy} 2x\\). In fact,
for all functions taking and returning scalars (just normal single numbers),
\\(\frac{dz}{dx} = \frac{dz}{dy} f'(x)\\).

Simple Example
--------------

With that in mind, let's a lifted "squared" operation, that takes `x` and
returns `x^2`:

```haskell top
square
    :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
square = liftOp1 . op1 $ \x ->
    ( x^2              , \dzdy -> dzdy * 2 * x)
--    ^- actual result   ^- scaled gradient function
```

We can write one for `sin`, as well.  For \\(y = f(x) = \sin(x)\\), we consider
\\(z = g(f(x))\\) to see \\(\frac{dz}{dx} = \frac{dz}{dy} \cos(x)\\). So, we
have:

```haskell top
liftedSin
    :: (Floating a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
liftedSin = liftOp1 . op1 $ \x ->
    ( sin x, \dzdy -> dzdy * cos x )
```

In general, for functions that take and return scalars:

```haskell
liftedF
    :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
liftedF = liftOp1 . op1 $ \x ->
    ( f x, \dzdy -> dzdy * dfdx x )
```

For an example of every single numeric function in base Haskell, see [the
source of Op.hs][opsource] for the `Op` definitions for every method in `Num`,
`Fractional`, and `Floating`.

[opsource]: https://github.com/mstksg/backprop/blob/a7651b4549048a3aca73c79c6fbe07c3e8ee500e/src/Numeric/Backprop/Op.hs#L646-L787

Non-trivial example
-------------------

A simple non-trivial example is `sumElements`, which we can define to take the
*hmatrix* library's `R n` type (an n-vector of `Double`).  In this case, we
have to think about \\(g(\mathrm{sum}(\mathbf{x}))\\).  In this case, the types
guide our thinking:

```haskell
sumElements           :: R n -> Double
sumElementsScaledGrad :: R n -> Double -> R n
```

The simplest way for me to do this personally is to just take it element by
element.

1.  *Write out the functions in question, in a simple example*

    In our case:

    *   \\(y = f(\langle a, b, c \rangle) = a + b + c\\)
    *   \\(z = g(y) = g(a + b + c)\\)

2.  *Identify the components in your gradient*

    In our case, we have to return a gradient \\(\langle \frac{\partial z}{\partial a},
    \frac{\partial z}{\partial b}, \frac{\partial z}{\partial c} \rangle\\).

3.  *Work out each component of the gradient until you start to notice a
    pattern*

    Let's start with \\(\frac{\partial z}{\partial a}\\).  We need to find
    \\(\frac{\partial z}{\partial a}\\) in terms of \\(\frac{dz}{dy}\\):

    *   Through the chain rule, \\(\frac{\partial z}{\partial a} =
        \frac{dz}{dy} \frac{\partial y}{\partial a}\\).
    *   Because \\(y = a + b + c\\), we know that \\(\frac{\partial y}{\partial
        a} = 1\\).
    *   Because \\(\frac{\partial y}{\partial a} = 1\\), we know that
        \\(\frac{\partial z}{\partial a} = \frac{dz}{dy} \times 1 =
        \frac{dz}{dy}\\).

    So, our expression of \\(\frac{\partial z}{\partial a}\\) in terms of
    \\(\frac{dz}{dy}\\) is simple -- it's simply \\(\frac{\partial z}{\partial
    a} = \frac{dz}{dy}\\).

    Now, let's look at \\(\frac{\partial z}{\partial b}\\).  We need to find
    \\(\frac{\partial z}{\partial b}\\) in terms of \\(\frac{dz}{dy}\\).

    *   Through the chain rule, \\(\frac{\partial z}{\partial b} =
        \frac{dz}{dy} \frac{\partial y}{\partial b}\\).
    *   Because \\(y = a + b + c\\), we know that \\(\frac{\partial y}{\partial
        b} = 1\\).
    *   Because \\(\frac{\partial y}{\partial b} = 1\\), we know that
        \\(\frac{\partial z}{\partial b} = \frac{dz}{dy} \times 1 =
        \frac{dz}{dy}\\).

    It looks like \\(\frac{\partial z}{\partial b} = \frac{\partial z}{\partial
    y}\\), as well.

    At this point, we start to notice a pattern.  We can apply the same logic
    to see that \\(\frac{\partial z}{\partial c} = \frac{dz}{dy}\\).

4.  *Write out the pattern*

    Extrapolating the pattern, \\(\frac{\partial z}{\partial q}\\), where
    \\(q\\) is *any* component, is always going to be a constant --
    \\(\frac{dz}{dy}\\).

So in the end:

```haskell top hide
instance Backprop (R n) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance (KnownNat n, KnownNat m) => Backprop (L n m) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

sumElements :: KnownNat n => R n -> Double
sumElements = HU.sumElements . H.extract
```

```haskell top
liftedSumElements
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s Double
liftedSumElements = liftOp1 . op1 $ \xs ->
    ( sumElements xs, \dzdy -> konst dzdy )  -- a constant vector
```

### Multiple-argument functions

Lifting multiple-argument functions is the same thing, except using `liftOp2`
and `op2`, or `liftOpN` and `opN`.

A `BVar s a -> BVar s b -> BVar s c` is, really, under the hood:

```haskell
type BVar s a -> BVar s b -> BVar s c =
    a -> b -> (c, c -> (a, b))
```

That is, given an input `a` and `b`, you get:

1.  A `c`, the result (the "forward pass")
2.  A `c -> (a, b)`, the "scaled gradient" function returning the gradient of
    both inputs.

The `c` parameter of the scaled gradient is again \\(\frac{dz}{dy}\\), and the
final `(a,b)` is a tuple of \\(\frac{\partial z}{\partial x_1}\\) and
\\(\frac{\partial z}{\partial x_2}\\): how \\(\frac{dz}{dy}\\) affects both of
the inputs.

For a simple example, let's look at \\(x + y\\).  Working it out:

*   \\(y = f(x_1, x_2) = x_1 + x_2\\)
*   \\(z = g(f(x_1, x_2)) = g(x_1 + x_2)\\)
*   Looking first for \\(\frac{\partial z}{\partial x_1}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_1}\\) (chain rule)
    *   From \\(y = x_1 + x_2\\), we see that \\(\frac{\partial y}{\partial
        x_1} = 1\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \times 1
        = \frac{dz}{dy}\\).
*   Looking second for \\(\frac{\partial z}{\partial x_2}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_2}\\) (chain rule)
    *   From \\(y = x_1 + x_2\\), we see that \\(\frac{\partial y}{\partial
        x_2} = 1\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy} \times 1
        = \frac{dz}{dy}\\).
*   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy}\\), and also
    \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy}\\).

Putting it into code:

```haskell top
add :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
    -> BVar s a
add = liftOp2 . op2 $ \x1 x2 ->
    ( x1 + x2, \dzdy -> (dzdy, dzdy) )
```

Let's try our hand at multiplication, or \\(x * y\\):

*   \\(y = f(x_1, x_2) = x_1 x_2\\)
*   \\(z = g(f(x_1, x_2)) = g(x_1 x_2)\\)
*   Looking first for \\(\frac{d\partial }{d\partial _1}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_1}\\) (chain rule)
    *   From \\(y = x_1 x_2\\), we see that \\(\frac{\partial y}{\partial x_1}
        = x_2\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} x_2\\).
*   Looking second for \\(\frac{\partial z}{\partial x_2}\\) in terms of
    \\(\frac{dz}{dy}\\):
    *   \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} \frac{\partial
        y}{\partial x_1}\\) (chain rule)
    *   From \\(y = x_1 x_2\\), we see that \\(\frac{\partial y}{\partial x_2}
        = x_1\\)
    *   Therefore, \\(\frac{\partial z}{\partial x_2} = \frac{dz}{dy} x_1\\).
*   Therefore, \\(\frac{\partial z}{\partial x_1} = \frac{dz}{dy} x_2\\), and
    \\(\frac{\partial z}{\partial x_2} = x_1 \frac{dz}{dy}\\).

In code:

```haskell top
mul :: (Num a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s a
    -> BVar s a
mul = liftOp2 . op2 $ \x1 x2 ->
    ( x1 * x2, \dzdy -> (dzdy * x2, x1 * dzdy) )
```

For non-trivial examples involving linear algebra, see the source for the *[hmatrix-backprop][]* library.

[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop

Some examples, for the dot product between two vectors and for matrix-vector
multiplication:

```haskell top
-- import qualified Numeric.LinearAlgebra.Static as H

-- | dot product between two vectors
dot
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s (R n)
    -> BVar s Double
dot = liftOp2 . op2 $ \u v ->
    ( u `H.dot` v
    , \dzdy -> (H.konst dzdy * v, u * H.konst dzdy)
    )


-- | matrix-vector multiplication
(#>)
    :: (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (L m n)
    -> BVar s (R n)
    -> BVar s (R m)
(#>) = liftOp2 . op2 $ \mat vec ->
    ( mat H.#> vec
    , \dzdy -> (dzdy `H.outer` vec, H.tr mat H.#> dzdy)
    )
```

Possibilities
-------------

That's it for this introductory tutorial on lifting single operations.  More
information on the ways to apply these techniques to fully equip your library
for backpropagation (including arguments with multiple results, taking
advantage of isomorphisms, providing non-gradient functions) can be [found
here][equipping]!
