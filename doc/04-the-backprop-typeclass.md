---
title: The Backprop Typeclass
---

The Backprop Typeclass
======================

```haskell top hide
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}

import           GHC.Generics (Generic)
import           GHC.TypeNats
import           Numeric.LinearAlgebra.Static (L, R)
import           Numeric.Backprop
import           Numeric.Backprop.Class
import qualified Data.Vector as V
```

Most of the functions in this module require a `Backprop` constraint on values
you wish to backpropagate.  Even if you manage to get around it for the most
part, `gradBP` (the actual function to compute gradients) requires it on both
the inputs and outputs.  Let's dig deeper into what it is, and how to define
instances.

The Class
---------

The typeclass contains three methods: `zero`, `add`, and `one`:

```haskell
class Backprop a where
    zero :: a -> a
    add  :: a -> a -> a
    one  :: a -> a
```

`zero` is "zero" in the verb sense -- it takes a value and "zeroes out" all
components.  For a vector, this means returning a zero vector of the same
shape.  For a list, this means replacing all of the items with zero and
returning a list of the same length.  `one` does the same thing but with one.
`add` is used to add together contributions in gradients, and is usually a
component-wise addition.

Instances are provided for most common data types where it makes sense.

Custom Instances
----------------

### Generics

When defining your own custom types, if your custom type is has *a single
constructor* where all fields are instances of `Backprop`,  then *GHC.Generics*
can be used to write your instances automatically:

```haskell top hide
instance Backprop (R n) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance (KnownNat n, KnownNat m) => Backprop (L n m) where
    zero = zeroNum
    add  = addNum
    one  = oneNum
```

```haskell top
data MyType = MkMyType Double [Float] (R 10) (L 20 10) (V.Vector Double)
  deriving Generic
```

Nice type.  Since it has a single constructor and all of its fields are already
`Backprop` instances, we can just write:

```haskell top
instance Backprop MyType
```

and now your type can be backpropagated!

### Common Patterns

For writing "primitive" `Backprop` instances (types that aren't product types),
you can use the provided "helpers" from the *Numeric.Backprop.Class* module.

If your type is a `Num` instance, you can use `zeroNum`, `addNum`, and
`oneNum`:

```haskell
instance Backprop Double where
    zero = zeroNum
    add  = addNum
    one  = oneNum
```

If your type is made using a `Functor` instance, you can use `zeroFunctor` and
`oneFunctor`:

```haskell
instance Backprop a => Backprop (V.Vector a) where
    zero = zeroFunctor
    add  = undefined        -- ??
    one  = oneFunctor
```

And if your type has an `IsList` instance, you can use `addIsList`:

```haskell
instance Backprop a => Backprop (V.Vector a) where
    zero = zeroFunctor
    add  = addIsList
    one  = oneFunctor
```

### Completely Custom

Completely custom instances are also possible; you just need to implement
`zero`, `add`, and `one` as they make sense for your type.  Just make sure that
you obey [the laws][laws] for sane behavior!

[laws]: http://hackage.haskell.org/package/backprop/docs/Numeric-Backprop-Class.html

Moving On
=========

At this point, feel free to [jump into the haddocks][haddock], or read on
further for [a list of applications and resources][applications].

[haddock]: https://hackage.haskell.org/package/backprop
[applications]: https://backprop.jle.im/05-applications.html
