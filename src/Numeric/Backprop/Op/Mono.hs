{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Backprop.Op.Mono (
  -- * Types
    Op
  -- * Running
  , runOp', runOp, gradOp, gradOpWith, gradOpWith'
  -- * Creation
  , op1, op2, op3, opN
  -- * Utility
  -- ** Type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  -- ** Vectors
  , VecT(..), Vec, pattern (:+), (*:), (+:), head'
 ) where

import           Data.Bifunctor
import           Data.Reflection             (Reifies)
import           Data.Type.Nat
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.AD.Internal.Reverse (Reverse, Tape)
import           Numeric.AD.Mode.Forward     (AD, Forward)
import           Type.Class.Known
import           Type.Family.Nat
import qualified Numeric.Backprop.Op         as BP

type Op n a       = BP.Op (Replicate n a)

op1 :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Op N1 a a
op1 f = BP.op1 f

op2 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op N2 a a
op2 = BP.op2

op3 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op N3 a a
op3 = BP.op3

opN :: (Num a, Known Nat n)
    => (forall s. Reifies s Tape => Vec n (Reverse s a) -> Reverse s a)
    -> Op n a a
opN = BP.opN

runOp' :: Op n a b -> Vec n a -> (b, Maybe b -> Vec n a)
runOp' o xs = (second . fmap) (prodAlong xs)
            . BP.runOp' o
            . vecToProd
            $ xs

runOp :: Op n a b -> Vec n a -> b
runOp o = fst . runOp' o

gradOpWith' :: Op n a b -> Vec n a -> Maybe b -> Vec n a
gradOpWith' o = snd . runOp' o

gradOpWith :: Op n a b -> Vec n a -> b -> Vec n a
gradOpWith o i = gradOpWith' o i . Just

gradOp :: Op n a b -> Vec n a -> Vec n a
gradOp o i = gradOpWith' o i Nothing

