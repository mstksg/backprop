{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Numeric.Backprop
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Automatic differentation and backpropagation.
--
-- Main idea: Write a function computing what you want, and the library
-- automatically provies the gradient of that function as well, for usage
-- with gradient descent and other training methods.
--
-- In more detail: instead of working directly with values to produce your
-- result, you work with 'BVar's containing those values.  Working with
-- these 'BVar's is made smooth with the usage of lenses and other
-- combinators, and libraries can offer operatons on 'BVar's instead of
-- those on normal types directly.
--
-- Then, you can use:
--
-- @
-- 'evalBP' :: (forall s. 'Reifies' s 'W'. 'BVar' s a -> BVar s b) -> (a -> b)
-- @
--
-- to turn a 'BVar' function into the function on actual values @a -> b@.
-- This has virtually zero overhead over writing the actual function
-- directly.
--
-- Then, there's:
--
-- @
-- 'gradBP' :: (forall s. 'Reifies' s 'W'. 'BVar' s a -> BVar s b) -> (a -> a)
-- @
--
-- to automatically get the /gradient/, as well, for a given input.
--
-- See the <https://github.com/mstksg/backprop README> for more information
-- and links to demonstrations and tutorials, or dive striaght in by
-- reading the docs for 'BVar'.
--
-- In the original version 0.1, this module required 'Num' instances for
-- methods instead of 'Backprop' instances.  This interface is still
-- available in "Numeric.Backprop.Num", which has the same API as this
-- module, except with 'Num' constraints on all values instead of
-- 'Backprop' constraints.
--
-- See "Prelude.Backprop.Explicit" for a version allowing you to provide
-- 'zero', 'add', and 'one' explicitly, which can be useful when attempting
-- to avoid orphan instances.
--

module Numeric.Backprop (
    -- * Types
    BVar, W, Backprop(..)
  , module Numeric.Backprop.Class
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
import           Numeric.Backprop.Class
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
-- arguments or a giant tuple.  However, this could potentially also be
-- more performant.
--
-- A @'Prod' ('BVar' s) '[Double, Float, Double]@, for instance, is a tuple
-- of @'BVar' s 'Double'@, @'BVar' s 'Float'@, and @'BVar' s 'Double'@, and
-- can be pattern matched on using ':<' (cons) and 'Ø' (nil).
--
-- Tuples can be built and pattern matched on using '::<' (cons) and 'Ø'
-- (nil), as well.
--
-- The @'Every' 'Backprop' as@ in the constraint says that every value in
-- the type-level list @as@ must have a 'Backprop' instance.  This means
-- you can use, say, @'[Double, Float, Int]@, but not @'[Double, Bool,
-- String]@.
--
-- If you stick to /concerete/, monomorphic usage of this (with specific
-- types, typed into source code, known at compile-time), then @'Every'
-- 'Backprop' as@ should be fulfilled automatically.
backpropN
    :: forall as b. (Every Backprop as, Known Length as, Backprop b)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, Tuple as)
backpropN = E.backpropN E.zeroFuncs E.oneFunc
{-# INLINE backpropN #-}

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents, also computing its gradient @a@ as well.
--
-- The Rank-N type @forall s. 'Reifies' s 'W' => ...@ is used to ensure
-- that 'BVar's do not leak out of the context (similar to how it is used
-- in "Control.Monad.ST"), and also as a reference to an ephemeral Wengert
-- tape used to track the graph of references.
backprop
    :: forall a b. (Backprop a, Backprop b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop = E.backprop E.zeroFunc E.oneFunc
{-# INLINE backprop #-}

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
    :: forall a b. (Backprop a, Backprop b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> a
gradBP = E.gradBP E.zeroFunc E.oneFunc
{-# INLINE gradBP #-}

-- | 'gradBP' generalized to multiple inputs of different types.  See
-- documentation for 'backpropN' for more details.
gradBPN
    :: forall as b. (Every Backprop as, Known Length as, Backprop b)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> Tuple as
gradBPN = E.gradBPN E.zeroFuncs E.oneFunc
{-# INLINE gradBPN #-}

-- | 'backprop' for a two-argument function.
--
-- Not strictly necessary, because you can always uncurry a function by
-- passing in all of the argument inside a data type, or just use a tuple.
-- However, this could potentially be more performant.
--
-- For 3 and more arguments, consider using 'backpropN'.
backprop2
    :: forall a b c. (Backprop a, Backprop b, Backprop c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c, (a, b))
backprop2 = E.backprop2 E.zeroFunc E.zeroFunc E.oneFunc
{-# INLINE backprop2 #-}

-- | 'gradBP' for a two-argument function.  See 'backprop2' for notes.
gradBP2
    :: (Backprop a, Backprop b, Backprop c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (a, b)
gradBP2 = E.gradBP2 E.zeroFunc E.zeroFunc E.oneFunc
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
    :: forall a b s. (Reifies s W, Backprop a)
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
    :: forall a b s. (Reifies s W, Backprop a)
    => Lens' b a
    -> BVar s b
    -> BVar s a
viewVar = E.viewVar E.addFunc E.zeroFunc
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
    :: forall a b s. (Reifies s W, Backprop a, Backprop b)
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
    :: forall a b s. (Reifies s W, Backprop a, Backprop b)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
setVar = E.setVar E.addFunc E.addFunc E.zeroFunc E.zeroFunc
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
(^^?)
    :: forall b a s. (Backprop a, Reifies s W)
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
    :: forall b a s. (Reifies s W, Backprop a)
    => Traversal' b a
    -> BVar s b
    -> Maybe (BVar s a)
previewVar = E.previewVar E.addFunc E.zeroFunc
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
    :: forall b a s. (Backprop a, Reifies s W)
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
    :: forall b a s. (Backprop a, Reifies s W)
    => Traversal' b a
    -> BVar s b
    -> [BVar s a]
toListOfVar = E.toListOfVar E.addFunc E.zeroFunc
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
    :: forall t a s. (Backprop a, Reifies s W, Traversable t)
    => BVar s (t a)
    -> t (BVar s a)
sequenceVar = E.sequenceVar E.addFunc E.zeroFunc
{-# INLINE sequenceVar #-}

-- | Collect all of the 'BVar's in a container into a 'BVar' of that
-- container's contents.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the total derivative and
-- gradient is assumed to correspond with the second item in the input,
-- etc.; this can cause unexpected behavior in 'Foldable' instances that
-- don't have a fixed number of items.
collectVar
    :: forall t a s. (Backprop a, Backprop (t a), Reifies s W, Foldable t, Functor t)
    => t (BVar s a)
    -> BVar s (t a)
collectVar = E.collectVar E.addFunc E.zeroFunc E.zeroFunc
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
    :: forall as b s. (Every Backprop as, Known Length as, Backprop b, Reifies s W)
    => Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOp = E.liftOp E.addFuncs E.zeroFunc
{-# INLINE liftOp #-}

-- | Lift an 'Op' with a single input to be a function on a single 'BVar'.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp1
    :: forall a b s. (Backprop a, Backprop b, Reifies s W)
    => Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 = E.liftOp1 E.addFunc E.zeroFunc
{-# INLINE liftOp1 #-}

-- | Lift an 'Op' with two inputs to be a function on a two 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp2
    :: forall a b c s. (Backprop a, Backprop b, Backprop c, Reifies s W)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 = E.liftOp2 E.addFunc E.addFunc E.zeroFunc
{-# INLINE liftOp2 #-}

-- | Lift an 'Op' with three inputs to be a function on a three 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp3
    :: forall a b c d s. (Backprop a, Backprop b, Backprop c, Backprop d, Reifies s W)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 = E.liftOp3 E.addFunc E.addFunc E.addFunc E.zeroFunc
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
    :: (Backprop a, Backprop b, Reifies s W)
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
    :: (Backprop a, Backprop b, Backprop c, Reifies s W)
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
    :: (Backprop a, Backprop b, Backprop c, Backprop d, Reifies s W)
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
    :: (Every Backprop as, Known Length as, Backprop b, Reifies s W)
    => (Tuple as -> b)
    -> (b -> Tuple as)
    -> Prod (BVar s) as
    -> BVar s b
isoVarN f g = liftOp (opIsoN f g)
{-# INLINE isoVarN #-}
