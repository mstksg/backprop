{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Backprop.Mono.Implicit (
  -- * Types
    BPRef, BPOp, Op
  -- * Backpropagation
  , backprop, grad, eval
  -- * Ref manipulation
  , constRef, liftR, liftR1, liftR2, liftR3
  -- * Op
  , op1, op2, op3, opN
  -- * Utility
  -- ** Type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  -- ** Vectors
  , VecT(..), Vec, pattern (:+), (*:), (+:), head'
  ) where

import           Data.Type.Nat
import           Data.Type.Vector
import           Numeric.Backprop.Mono hiding (backprop, BPOp)
import           Type.Class.Known
import qualified Numeric.Backprop.Mono        as BP

type BPOp n a b = forall s. VecT n (BPRef s n a) a -> BPRef s n a b

backprop
    :: forall n a b. (Num a, Known Nat n)
    => (forall s. VecT n (BPRef s n a) a -> BPRef s n a b)
    -> Vec n a
    -> (b, Vec n a)
backprop f = BP.backprop $ BP.withInps (return . f)

grad
    :: forall n a b. (Num a, Known Nat n)
    => (forall s. VecT n (BPRef s n a) a -> BPRef s n a b)
    -> Vec n a
    -> Vec n a
grad f = snd . backprop f

eval
    :: forall n a b. (Num a, Known Nat n)
    => (forall s. VecT n (BPRef s n a) a -> BPRef s n a b)
    -> Vec n a
    -> b
eval f = fst . backprop f

