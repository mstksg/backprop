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
import           Type.Class.Known
import qualified Numeric.Backprop.Explicit        as BP
import           Numeric.Backprop.Internal

backprop'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop' ss us f = BP.backprop' ss us (return $ f (BP.inpRefs' (prodLength ss)))

backprop
    :: (Known Length rs, Every Num rs)
    => (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop f = BP.backprop (return $ f BP.inpRefs)

grad'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> Tuple rs
grad' ss us f = BP.gradBPOp' ss us (return $ f (BP.inpRefs' (prodLength ss)))

grad
    :: (Known Length rs, Every Num rs)
    => (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> Tuple rs
grad f = BP.gradBPOp (return $ f BP.inpRefs)

eval'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> a
eval' ss us f = BP.runBPOp' ss us (return $ f (BP.inpRefs' (prodLength ss)))
  
eval
    :: (Known Length rs, Every Num rs)
    => (forall s. Prod (BPRef s rs) rs -> BPRef s rs a)
    -> Tuple rs
    -> a
eval f = BP.runBPOp (return $ f BP.inpRefs)

