{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Backprop.Implicit (
    backprop, grad, eval
  , backprop', grad', eval'
  ) where

import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Numeric.Backprop.Internal
import           Type.Class.Known
import qualified Numeric.Backprop          as BP

backprop'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop' ss us f = BP.backprop' ss us $ BP.withInps' (prodLength ss) (return . f)

backprop
    :: (Known Length rs, Every Num rs)
    => (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop f = BP.backprop $ BP.withInps (return . f)

grad'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> Tuple rs
grad' ss us f = snd . backprop' ss us f

grad
    :: (Known Length rs, Every Num rs)
    => (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> Tuple rs
grad f = snd . backprop f

eval'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> a
eval' ss us f = fst . backprop' ss us f

eval
    :: (Known Length rs, Every Num rs)
    => (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> a
eval f = fst . backprop f
