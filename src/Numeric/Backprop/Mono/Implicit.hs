{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

-- |
-- Module      : Numeric.Backprop.Mono.Implicit
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Offers full functionality for implicit-graph backpropagation with
-- monomorphic inputs.  The intended usage is to write a 'BPOp', which is
-- a normal Haskell function from 'BVar's to a result 'BVar'. These 'BVar's
-- can be manipulated using their 'Num' / 'Fractional' / 'Floating'
-- instances.
--
-- The library can then perform backpropagation on the function (using
-- 'backprop' or 'grad') by using an implicitly built graph.
--
-- This is an "implicit-only" version of "Numeric.Backprop.Mono", and
-- a monomorphic version of "Numeric.Backprop.Implicit", monomorphic in the
-- sense that all of the inputs are of the same type.
--
-- Like for "Numeric.Backprop.Implicit", this should actually be powerful
-- enough for most use cases, but falls short because without explicit
-- graph capabilities, recomputation can sometimes be inevitable.  If the
-- result of a function on 'BVar's is used twice (like @z@ in @let
-- z = x * y in z + z@), this will allocate a new redundant graph node for
-- every usage site of @z@.  You can explicitly /force/ @z@, but only using
-- an explicit graph description using "Numeric.Backprop.Mono".
--
-- Like "Numeric.Backprop.Implicit", this can't handle sum types, but
-- neither can "Numeric.Backprop.Mono", so no loss here :)
--
-- This module implements pretty much the same functionality as
-- "Numeric.AD" and "Numeric.AD.Mode.Reverse" from the /ad/ package,
-- because it uses the same implicit-graph backpropagation method.  It
-- can't compute jacobians/generalized gradients, however.  This isn't
-- a fundamental limitation of the implementaiton, though, but rather just
-- a conscious design decision for this module's API.
--


module Numeric.Backprop.Mono.Implicit (
  -- * Types
  -- ** Backprop types
    BVar, BPOp, Op, BP.OpB
  -- ** Vectors
  -- | See "Numeric.Backprop.Mono#vec" for a mini-tutorial on 'VecT' and
  -- 'Vec'
  , VecT(..), Vec, I(..)
  -- * Backpropagation
  , backprop, grad, eval
  -- * Var manipulation
  , constVar, liftB, (.$), liftB1, liftB2, liftB3
  -- * Op
  , op1, op2, op3, opN
  -- * Utility
  , pattern (:+), (*:), (+:), head'
  -- ** 'Nat' type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  ) where

import           Data.Type.Nat
import           Data.Type.Vector
import           Numeric.Backprop.Mono hiding (backprop, BPOp)
import           Type.Class.Known
import qualified Numeric.Backprop.Mono        as BP

-- | An operation on 'BVar's that can be backpropagated. A value of type:
--
-- @
-- 'BPOp' n r a
-- @
--
-- takes a vector ('VecT') of 'BVar's containg @n@ @r@s and uses them to
-- (purely) produce a 'BVar' containing an @a@.
--
-- @
-- foo :: 'BPOp' 'N2' Double Double
-- foo (x ':*' y ':*' 'ØV') = x + sqrt y
-- @
--
-- 'BPOp' here is related to 'Numeric.Backprop.Mono.BPOpI' from the normal
-- explicit-graph backprop module "Numeric.Backprop.Mono".
type BPOp n a b = forall s. VecT n (BVar s n a) a -> BVar s n a b

-- | Run backpropagation on a 'BPOp' function, getting both the result and
-- the gradient of the result with respect to the inputs.
--
-- @
-- foo :: 'BPOp' 'N2' Double Double
-- foo (x :* y :* ØV) =
--   let z = x * sqrt y
--   in  z + x ** y
-- @
--
-- >>> 'backprop' foo (2 :+ 3 :+ ØV)
-- (11.46, 13.73 :+ 6.12 :+ ØV)
backprop
    :: forall n a b. (Num a, Known Nat n)
    => BPOp n a b
    -> Vec n a
    -> (b, Vec n a)
backprop f = BP.backprop $ BP.withInps (return . f)

-- | Run the 'BPOp' on an input tuple and return the gradient of the result
-- with respect to the input tuple.
--
-- @
-- foo :: 'BPOp' 'N2' Double Double
-- foo (x :* y :* ØV) =
--   let z = x * sqrt y
--   in  z + x ** y
-- @
--
-- >>> 'grad' foo (2 :+ 3 :+ ØV)
-- 13.73 :+ 6.12 :+ ØV
grad
    :: forall n a b. (Num a, Known Nat n)
    => BPOp n a b
    -> Vec n a
    -> Vec n a
grad f = snd . backprop f

-- | Simply run the 'BPOp' on an input tuple, getting the result without
-- bothering with the gradient or with backpropagation.
--
-- @
-- foo :: 'BPOp' 'N2' Double Double
-- foo (x :* y :* ØV) =
--   let z = x * sqrt y
--   in  z + x ** y
-- @
--
-- >>> 'eval' foo (2 :+ 3 :+ ØV)
-- 11.46
eval
    :: forall n a b. (Num a, Known Nat n)
    => BPOp n a b
    -> Vec n a
    -> b
eval f = fst . backprop f

