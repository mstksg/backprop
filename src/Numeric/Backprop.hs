{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

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
-- 'evalBP' :: ('BVar' s a -> BVar s b) -> (a -> b)
-- @
--
-- to turn a 'BVar' function into the function on actual values @a -> b@.
-- This has virtually zero overhead over writing the actual function
-- directly.
--
-- Then, there's:
--
-- @
-- 'gradBP' :: ('BVar' s a -> BVar s b) -> (a -> a)
-- @
--
-- To automatically get the /gradient/, as well, for a given input.
--
-- See the <https://github.com/mstksg/backprop README> for more information
-- and links to demonstrations and tutorials.
--

module Numeric.Backprop (
    -- * Types
    BVar, W
    -- * Running
  , backprop, evalBP, gradBP
    -- * Manipulating 'BVar'
  , constVar
  , viewVar, (^^.), (.~~)
  , uncurryVar, uncurryVar3
    -- ** With 'Op's#liftops#
    -- $liftops
  , liftOp
  , liftOp1, liftOp2, liftOp3, liftOp4
    -- * 'Op'
  , Op(..)
    -- ** Creation
  , op0, opConst, idOp
  , opConst'
    -- *** Giving gradients directly
  , op1, op2, op3
    -- *** Automatic creation using the /ad/ library
  , op1', op2', op3', opN'
  , Replicate
    -- *** From Isomorphisms
  , opCoerce, opTup, opIso, opLens
  ) where

import           Data.Reflection
import           Lens.Micro
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Op

-- $liftops
--
-- This library provides a few primitive actions for manipulating 'BVar's
-- and the values inside them, including its 'Num', 'Fractional', and
-- 'Floating' instances, and lens-based operations like '^^.' and '.~~'.
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
--         , \g -> (map (*g) ys, map (*g) xs)
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

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents.
--
-- Benchmarks show that this should have virtually no overhead over
-- directly writing a @a -> b@. 'BVar' is, in this situation, a zero-cost
-- abstraction, performance-wise.
--
-- Note that every type involved has to be an instance of 'Num'.  This is
-- because gradients all need to be "summable" (which is implemented using
-- 'sum' and '+'), and we also need to able to generate gradients of '1'
-- and '0'.  Really, only '+' and 'fromInteger' methods are used from the
-- 'Num' typeclass.
--
evalBP
    :: forall a b. (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> b
evalBP f = fst . backprop f
{-# INLINE evalBP #-}

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
-- Note that every type involved has to be an instance of 'Num'.  This is
-- because gradients all need to be "summable" (which is implemented using
-- 'sum' and '+'), and we also need to able to generate gradients of '1'
-- and '0'.  Really, only '+' and 'fromInteger' methods are used from the
-- 'Num' typeclass.
--
gradBP
    :: forall a b. (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> a
gradBP f = snd . backprop f
{-# INLINE gradBP #-}

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
-- Would extract a piece of @x :: b@, specified by @myLens :: 'Lens'' b a@.
-- The result has type @a@.
--
-- @
-- xVar '^^.' myLens
-- @
--
-- Would extract a piece out of @xVar :: 'BVar' s b@ (a 'BVar' holding a
-- @b@), specified by @myLens :: Lens' b a@.   The result has type @'BVar'
-- s a@ (a 'BVar' holding a @a@)
--
-- This is the main way to pull out values from 'BVar' of container types.
--
(^^.)
    :: forall a b s. (Reifies s W, Num a)
    => BVar s b
    -> Lens' b a
    -> BVar s a
x ^^. l = viewVar l x
infixl 8 ^^.
{-# INLINE (^^.) #-}

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
-- Would "set" a part of @x :: b@, specified by @myLens :: 'Lens'' a b@, to
-- a new value @y :: a@.
--
-- @
-- xVar '&' myLens '.~~' yVar
-- @
--
-- Would "set" a part of @xVar :: 'BVar' s b@ (a 'BVar' holding a @b@),
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

-- | Convenient function to uncurry a 'BVar' function, so it can be used
-- with 'backprop', 'evalBP', and 'gradBP'.
uncurryVar
    :: (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> (forall s. Reifies s W => BVar s (a, b) -> BVar s c)
uncurryVar f xy = f (xy ^^. _1) (xy ^^. _2)
{-# INLINE uncurryVar #-}

-- | Convenient function to uncurry a 'BVar' function, so it can be used
-- with 'backprop', 'evalBP', and 'gradBP'.
uncurryVar3
    :: (Num a, Num b, Num c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c -> BVar s d)
    -> (forall s. Reifies s W => BVar s (a, b, c) -> BVar s d)
uncurryVar3 f xyz = f (xyz ^^. _1) (xyz ^^. _2) (xyz ^^. _3)
{-# INLINE uncurryVar3 #-}

