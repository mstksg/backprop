{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Numeric.Backprop.Num
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the exact same API as "Numeric.Backprop", except requiring
-- 'Num' instances for all types involved instead of 'Backprop' instances.
--
-- This was the original API of the library (for version 0.1).
--
-- 'Num' is strictly more powerful than 'Backprop', and is a stronger
-- constraint on types than is necessary for proper backpropagating.  In
-- particular, 'fromInteger' is a problem for many types, preventing useful
-- backpropagation for lists, variable-length vectors (like "Data.Vector")
-- and variable-size matrices from linear algebra libraries like /hmatrix/
-- and /accelerate/.
--
-- However, this module might be useful in situations where you are working
-- with external types with 'Num' instances, and you want to avoid writing
-- orphan instances for external types.
--
-- If you have external types that are not 'Num' instances, consider
-- instead "Numeric.Backprop.External".

module Numeric.Backprop.Num (
    -- * Types
    BVar, W
    -- * Running
  , backprop, E.evalBP, gradBP
    -- ** Multiple inputs
  , backprop2, E.evalBP2, gradBP2
  , backpropN, E.evalBPN, gradBPN, Every
    -- * Manipulating 'BVar'
  , E.constVar, E.coerceVar
  , (^^.), (.~~), (^^?), (^^..)
  , viewVar, setVar
  , sequenceVar, collectVar
  , previewVar, toListOfVar
    -- ** With Isomorphisms
  , isoVar, isoVar2, isoVar3, isoVarN
    -- ** With 'Op's#liftops#
    -- $liftops
  , liftOp
  , liftOp1, liftOp2, liftOp3
    -- * 'Op'
  , Op(..)
    -- ** Creation
  , op0, opConst, idOp
  , opConst'
    -- *** Giving gradients directly
  , op1, op2, op3
    -- *** From Isomorphisms
  , opCoerce, opTup, opIso, opIsoN, opLens
    -- *** No gradients
  , noGrad1, noGrad
    -- * Utility
    -- ** Inductive tuples/heterogeneous lists
  , Prod(..), pattern (:>), only, head'
  , Tuple, pattern (::<), only_
  , I(..)
    -- ** Misc
  , Reifies
  ) where

import           Data.Reflection
import           Data.Type.Index
import           Data.Type.Length
import           Lens.Micro
import           Numeric.Backprop.Explicit (BVar, W)
import           Numeric.Backprop.Op
import           Type.Class.Known
import qualified Numeric.Backprop.Explicit as E

-- $liftops
--
-- This library provides a few primitive actions for manipulating 'BVar's
-- and the values inside them, including its 'Num', 'Fractional', and
-- 'Floating' instances, and lens-based operations like '^^.', '.~~' '^^?',
-- and '^^..'.
--
-- However, the power of this library comes from manipulating many
-- different types from libraries, like matrices and vectors.  Libraries
-- can provide their own @'BVar' s a -> 'BVar' s b@ functions, alongside
-- (or in lieu of) @a -> b@ functions for their types.
--
-- The easiest way to create a 'BVar' function is to use 'liftOp' with an
-- 'Op' constructor.  For example, imagine a vector library providing a dot
-- product function.  We can write this using 'liftOp2' and 'op2':
--
-- @
-- dot :: 'BVar' s Vec -> BVar s Vec -> BVar s Double
-- dot = 'liftOp2' . op2 $ \\xs ys ->
--         ( sum (zipWith (*) xs ys)
--         , \\g -> (map (*g) ys, map (*g) xs)
--         )
-- @
--
-- We provide a function that, given the two inputs, returns:
--
--     (1) The result of the function on those two inputs
--     (2) A function taking the "total derivative", and returning the
--     gradient with respect to each of the inputs.
--
-- See documentation in "Numeric.Backprop.Op" for more information on the
-- second part (the gradient).
--
-- Nice 'Op's are how /backprop/ links together 'BVar's and tracks them to
-- determine their gradient.  Ideally, users would never have to deal with
-- these when backpropagating their own functions, and library authors
-- providing their matrix and vector operations, etc. would provide 'BVar'
-- variants of their normal operations.
--
-- In fact, 'BVar' operations could even be defined /instead/ of normal
-- operations, since it is easy to go from @'BVar' s a -> 'BVar' s b@ to @a
-- -> b@, using 'evalBP', and this carries virtually zero overhead, so some
-- libraries might even provide 'BVar' versions by default.

-- | 'backprop' generalized to multiple inputs of different types.  See the
-- "Numeric.Backprop.Op#prod" for a mini-tutorial on heterogeneous lists.
--
-- Not strictly necessary, because you can always uncurry a function by
-- passing in all of the inputs in a data type containing all of the
-- arguments or a tuple from "Numeric.Backprop.Tuple".   You could also
-- pass in a giant tuple with
-- <https://hackage.haskell.org/package/NumInstances NumInstances>.
-- However, this can be convenient if you don't want to make a custom
-- larger tuple type or pull in orphan instances.  This could potentially
-- also be more performant.
--
-- A @'Prod' ('BVar' s) '[Double, Float, Double]@, for instance, is a tuple
-- of @'BVar' s 'Double'@, @'BVar' s 'Float'@, and @'BVar' s 'Double'@, and
-- can be pattern matched on using ':<' (cons) and 'Ø' (nil).
--
-- Tuples can be built and pattern matched on using '::<' (cons) and 'Ø'
-- (nil), as well.
--
-- The @'Every' 'Num' as@ in the constraint says that every value in the
-- type-level list @as@ must have a 'Num' instance.  This means you can
-- use, say, @'[Double, Float, Int]@, but not @'[Double, Bool, String]@.
--
-- If you stick to /concerete/, monomorphic usage of this (with specific
-- types, typed into source code, known at compile-time), then @'Every'
-- 'Num' as@ should be fulfilled automatically.
--
backpropN
    :: forall as b. (Every Num as, Known Length as, Num b)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, Tuple as)
backpropN = E.backpropN E.zfNums E.ofNum
{-# INLINE backpropN #-}

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents, also computing its gradient @a@ as well.
--
-- The Rank-N type @forall s. 'Reifies' s 'W' => ...@ is used to ensure
-- that 'BVar's do not leak out of the context (similar to how it is used
-- in "Control.Monad.ST"), and also as a reference to an ephemeral Wengert
-- tape used to track the graph of references.
--
-- Note that every type involved has to be an instance of 'Num'.  This is
-- because gradients all need to be "summable" (which is implemented using
-- 'sum' and '+'), and we also need to able to generate gradients of 1
-- and 0.  Really, only '+' and 'fromInteger' methods are used from the
-- 'Num' typeclass.
--
-- This might change in the future, to allow easier integration with tuples
-- (which typically do not have a 'Num' instance), and potentially make
-- types easier to use (by only requiring '+', 0, and 1, and not the rest
-- of the 'Num' class).
--
-- See the <https://github.com/mstksg/backprop README> for a more detailed
-- discussion on this issue.
--
-- If you need a 'Num' instance for tuples, you can use the canonical 2-
-- and 3-tuples for the library in "Numeric.Backprop.Tuple".  If you need
-- one for larger tuples, consider making a custom product type instead
-- (making Num instances with something like
-- <https://hackage.haskell.org/package/one-liner-instances one-liner-instances>).
-- You can also use the orphan instances in the
-- <https://hackage.haskell.org/package/NumInstances NumInstances> package
-- (in particular, "Data.NumInstances.Tuple") if you are writing an
-- application and do not have to worry about orphan instances.
backprop
    :: forall a b. (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop = E.backprop E.zfNum E.ofNum
{-# INLINE backprop #-}

---- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
---- that it represents.
----
---- Benchmarks show that this should have virtually no overhead over
---- directly writing a @a -> b@. 'BVar' is, in this situation, a zero-cost
---- abstraction, performance-wise.
----
---- Has a nice advantage over using 'backprop' in that it doesn't require
---- 'Num' constraints on the input and output.
----
---- See documentation of 'backprop' for more information.
----
--evalBP :: (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> b
--evalBP = E.evalBP
--{-# INLINE evalBP #-}

-- | Take a function @'BVar' s a -> 'BVar' s b@, interpreted as a function
-- @a -> b@, and compute its gradient with respect to its input.
--
-- The resulting @a -> a@ tells how the input (and its components) affects
-- the output.  Positive numbers indicate that the result will vary in the
-- same direction as any adjustment in the input.  Negative numbers
-- indicate that the result will vary in the opposite direction as any
-- adjustment in the input.  Larger numbers indicate a greater sensitivity
-- of change, and small numbers indicate lower sensitivity.
--
-- See documentation of 'backprop' for more information.
--
gradBP
    :: forall a b. (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> a
gradBP = E.gradBP E.zfNum E.ofNum
{-# INLINE gradBP #-}

-- | 'gradBP' generalized to multiple inputs of different types.  See
-- documentation for 'backpropN' for more details.
gradBPN
    :: forall as b. (Every Num as, Known Length as, Num b)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> Tuple as
gradBPN = E.gradBPN E.zfNums E.ofNum
{-# INLINE gradBPN #-}

-- | 'backprop' for a two-argument function.
--
-- Not strictly necessary, because you can always uncurry a function by
-- passing in all of the argument inside a data type, or use 'T2'. However,
-- this could potentially be more performant.
--
-- For 3 and more arguments, consider using 'backpropN'.
backprop2
    :: forall a b c. (Num a, Num b, Num c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c, (a, b))
backprop2 = E.backprop2 E.zfNum E.zfNum E.ofNum
{-# INLINE backprop2 #-}

-- -- | 'evalBP' for a two-argument function.  See 'backprop2' for notes.
-- evalBP2
--     :: (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
--     -> a
--     -> b
--     -> c
-- evalBP2 = E.evalBP2
-- {-# INLINE evalBP2 #-}

-- | 'gradBP' for a two-argument function.  See 'backprop2' for notes.
gradBP2
    :: (Num a, Num b, Num c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (a, b)
gradBP2 = E.gradBP2 E.zfNum E.zfNum E.ofNum
{-# INLINE gradBP2 #-}

-- | An infix version of 'viewVar', meant to evoke parallels to '^.' from
-- lens.
--
-- With normal values, you can extract something from that value with
-- a lens:
--
-- @
-- x '^.' myLens
-- @
--
-- would extract a piece of @x :: b@, specified by @myLens :: 'Lens'' b a@.
-- The result has type @a@.
--
-- @
-- xVar '^^.' myLens
-- @
--
-- would extract a piece out of @xVar :: 'BVar' s b@ (a 'BVar' holding a
-- @b@), specified by @myLens :: Lens' b a@.   The result has type @'BVar'
-- s a@ (a 'BVar' holding a @a@)
--
-- This is the main way to pull out values from 'BVar' of container types.
--
-- __WARNING__: Do not use with any lenses that operate "numerically" on
-- the contents (like 'multiplying').
--
(^^.)
    :: forall a b s. (Reifies s W, Num a)
    => BVar s b
    -> Lens' b a
    -> BVar s a
x ^^. l = viewVar l x
infixl 8 ^^.
{-# INLINE (^^.) #-}

-- | Using a 'Lens'', extract a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to 'view' from lens.
--
-- See documentation for '^^.' for more information.
viewVar
    :: forall a b s. (Reifies s W, Num a)
    => Lens' b a
    -> BVar s b
    -> BVar s a
viewVar = E.viewVar E.afNum E.zfNum
{-# INLINE viewVar #-}


-- | An infix version of 'setVar', meant to evoke parallels to '.~' from
-- lens.
--
-- With normal values, you can set something in a value with a lens:
-- a lens:
--
-- @
-- x '&' myLens '.~' 'y'
-- @
--
-- would "set" a part of @x :: b@, specified by @myLens :: 'Lens'' a b@, to
-- a new value @y :: a@.
--
-- @
-- xVar '&' myLens '.~~' yVar
-- @
--
-- would "set" a part of @xVar :: 'BVar' s b@ (a 'BVar' holding a @b@),
-- specified by @myLens :: 'Lens'' a b@, to a new value given by @yVar ::
-- 'BVar' s a@.  The result is a new (updated) value of type @'BVar' s b@.
--
-- This is the main way to set values inside 'BVar's of container types.
--
(.~~)
    :: forall a b s. (Reifies s W, Num a, Num b)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
l .~~ x = setVar l x
infixl 8 .~~
{-# INLINE (.~~) #-}

-- | Using a 'Lens'', set a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to "set" from lens.
--
-- See documentation for '.~~' for more information.
setVar
    :: forall a b s. (Reifies s W, Num a, Num b)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
setVar = E.setVar E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE setVar #-}


-- | An infix version of 'previewVar', meant to evoke parallels to '^?'
-- from lens.
--
-- With normal values, you can (potentially) extract something from that
-- value with a lens:
--
-- @
-- x '^?' myPrism
-- @
--
-- would (potentially) extract a piece of @x :: b@, specified by
-- @myPrism :: 'Traversal'' b a@. The result has type @'Maybe' a@.
--
-- @
-- xVar '^^?' myPrism
-- @
--
-- would (potentially) extract a piece out of @xVar :: 'BVar' s b@ (a
-- 'BVar' holding a @b@), specified by @myPrism :: Prism' b a@.
-- The result has type @'Maybe' ('BVar' s a)@ ('Maybe' a 'BVar' holding
-- a @a@).
--
-- This is intended to be used with 'Prism''s (which hits at most one
-- target), but will actually work with /any/ 'Traversal''.  If the
-- traversal hits more than one target, the first one found will be
-- extracted.
--
-- This can be used to "pattern match" on 'BVar's, by using prisms on
-- constructors.
--
-- Note that many automatically-generated prisms by the /lens/ package use
-- tuples, which cannot normally be backpropagated (because they do not
-- have a 'Num' instance).
--
-- If you are writing an application or don't have to worry about orphan
-- instances, you can pull in the orphan instances from
-- <https://hackage.haskell.org/package/NumInstances NumInstances>.
-- Alternatively, you can chain those prisms with conversions to the
-- anonymous canonical strict tuple types in "Numeric.Backprop.Tuple",
-- which do have 'Num' instances.
--
-- @
-- myPrism                   :: 'Prism'' c (a, b)
-- myPrism . 'iso' 'tupT2' 't2Tup' :: 'Prism'' c ('T2' a b)
-- @
(^^?)
    :: forall b a s. (Num a, Reifies s W)
    => BVar s b
    -> Traversal' b a
    -> Maybe (BVar s a)
v ^^? t = previewVar t v
{-# INLINE (^^?) #-}

-- | Using a 'Traversal'', extract a single value /inside/ a 'BVar', if it
-- exists.  If more than one traversal target exists, returns te first.
-- Meant to evoke parallels to 'preview' from lens.  Really only intended
-- to be used wth 'Prism''s, or up-to-one target traversals.
--
-- See documentation for '^^?' for more information.
previewVar
    :: forall b a s. (Reifies s W, Num a)
    => Traversal' b a
    -> BVar s b
    -> Maybe (BVar s a)
previewVar = E.previewVar E.afNum E.zfNum
{-# INLINE previewVar #-}

-- | An infix version of 'toListOfVar', meant to evoke parallels to '^..'
-- from lens.
--
-- With normal values, you can extract all targets of a 'Traversal' from
-- that value with a:
--
-- @
-- x '^..' myTraversal
-- @
--
-- would extract all targets inside of @x :: b@, specified by @myTraversal
-- :: 'Traversal'' b a@. The result has type @[a]@.
--
-- @
-- xVar '^^..' myTraversal
-- @
--
-- would extract all targets inside of @xVar :: 'BVar' s b@ (a 'BVar'
-- holding a @b@), specified by @myTraversal :: Traversal' b a@.   The result
-- has type @['BVar' s a]@ (A list of 'BVar's holding @a@s).
--
(^^..)
    :: forall b a s. (Num a, Reifies s W)
    => BVar s b
    -> Traversal' b a
    -> [BVar s a]
v ^^.. t = toListOfVar t v
{-# INLINE (^^..) #-}

-- | Using a 'Traversal'', extract all targeted values /inside/ a 'BVar'.
-- Meant to evoke parallels to 'toListOf' from lens.
--
-- See documentation for '^^..' for more information.
toListOfVar
    :: forall b a s. (Num a, Reifies s W)
    => Traversal' b a
    -> BVar s b
    -> [BVar s a]
toListOfVar = E.toListOfVar E.afNum E.zfNum
{-# INLINE toListOfVar #-}

-- | Extract all of the 'BVar's out of a 'Traversable' container of
-- 'BVar's.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the gradient is assumed to
-- correspond with the second item in the input, etc.; this can cause
-- unexpected behavior in 'Foldable' instances that don't have a fixed
-- number of items.
sequenceVar
    :: forall t a s. (Num a, Reifies s W, Traversable t)
    => BVar s (t a)
    -> t (BVar s a)
sequenceVar = E.sequenceVar E.afNum E.zfNum
{-# INLINE sequenceVar #-}

-- | Collect all of the 'BVar's in a container into a 'BVar' of that
-- container's contents.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the total derivative and
-- gradient is assumed to correspond with the second item in the input,
-- etc.; this can cause unexpected behavior in 'Foldable' instances that
-- don't have a fixed number of items.
--
-- Note that this requires @t a@ to have a 'Num' instance.  If you are
-- using a list, I recommend using
-- <https://hackage.haskell.org/package/vector-sized vector-sized> instead:
-- it's a fixed-length vector type with a very appropriate 'Num' instance!
collectVar
    :: forall t a s. (Num a, Num (t a), Reifies s W, Foldable t, Functor t)
    => t (BVar s a)
    -> BVar s (t a)
collectVar = E.collectVar E.afNum E.zfNum E.zfNum
{-# INLINE collectVar #-}

-- | Lift an 'Op' with an arbitrary number of inputs to a function on the
-- appropriate number of 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information, and "Numeric.Backprop.Op#prod" for a mini-tutorial on using
-- 'Prod' and 'Tuple'.
liftOp
    :: forall as b s. (Every Num as, Known Length as, Num b, Reifies s W)
    => Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOp = E.liftOp E.afNums E.zfNum
{-# INLINE liftOp #-}

-- | Lift an 'Op' with a single input to be a function on a single 'BVar'.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp1
    :: forall a b s. (Num a, Num b, Reifies s W)
    => Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 = E.liftOp1 E.afNum E.zfNum
{-# INLINE liftOp1 #-}

-- | Lift an 'Op' with two inputs to be a function on a two 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp2
    :: forall a b c s. (Num a, Num b, Num c, Reifies s W)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 = E.liftOp2 E.afNum E.afNum E.zfNum
{-# INLINE liftOp2 #-}

-- | Lift an 'Op' with three inputs to be a function on a three 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp3
    :: forall a b c d s. (Num a, Num b, Num c, Num d, Reifies s W)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 = E.liftOp3 E.afNum E.afNum E.afNum E.zfNum
{-# INLINE liftOp3 #-}

-- | Convert the value inside a 'BVar' using a given isomorphism.  Useful
-- for things like constructors.
--
-- Warning: This is unsafe!  It assumes that the isomorphisms themselves
-- have derivative 1, so will break for things like 'exp' & 'log'.
-- Basically, don't use this for any "numeric" isomorphisms.
--
-- @since 0.1.4.0
isoVar
    :: (Num a, Num b, Reifies s W)
    => (a -> b)
    -> (b -> a)
    -> BVar s a
    -> BVar s b
isoVar f g = liftOp1 (opIso f g)
{-# INLINE isoVar #-}

-- | Convert the values inside two 'BVar's using a given isomorphism.
-- Useful for things like constructors.  See 'isoVar' for caveats.
--
-- @since 0.1.4.0
isoVar2
    :: (Num a, Num b, Num c, Reifies s W)
    => (a -> b -> c)
    -> (c -> (a, b))
    -> BVar s a
    -> BVar s b
    -> BVar s c
isoVar2 f g = liftOp2 (opIso2 f g)
{-# INLINE isoVar2 #-}

-- | Convert the values inside three 'BVar's using a given isomorphism.
-- Useful for things like constructors.  See 'isoVar' for caveats.
--
-- @since 0.1.4.0
isoVar3
    :: (Num a, Num b, Num c, Num d, Reifies s W)
    => (a -> b -> c -> d)
    -> (d -> (a, b, c))
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
isoVar3 f g = liftOp3 (opIso3 f g)
{-# INLINE isoVar3 #-}

-- | Convert the values inside a tuple of 'BVar's using a given
-- isomorphism. Useful for things like constructors.  See 'isoVar' for
-- caveats.
--
-- @since 0.1.4.0
isoVarN
    :: (Every Num as, Known Length as, Num b, Reifies s W)
    => (Tuple as -> b)
    -> (b -> Tuple as)
    -> Prod (BVar s) as
    -> BVar s b
isoVarN f g = liftOp (opIsoN f g)
{-# INLINE isoVarN #-}

