{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Backprop.Mono.Implicit (
    backprop, grad, eval
  ) where

import           Data.Type.Nat
import           Data.Type.Vector
import           Type.Class.Known
import qualified Numeric.Backprop.Mono as BP

backprop
    :: forall n a b. (Num a, Known Nat n)
    => (forall s. VecT n (BP.BPRef s n a) a -> BP.BPRef s n a b)
    -> Vec n a
    -> (b, Vec n a)
backprop f = BP.backprop $ BP.withInps (return . f)

grad
    :: forall n a b. (Num a, Known Nat n)
    => (forall s. VecT n (BP.BPRef s n a) a -> BP.BPRef s n a b)
    -> Vec n a
    -> Vec n a
grad f = snd . backprop f

eval
    :: forall n a b. (Num a, Known Nat n)
    => (forall s. VecT n (BP.BPRef s n a) a -> BP.BPRef s n a b)
    -> Vec n a
    -> b
eval f = fst . backprop f

