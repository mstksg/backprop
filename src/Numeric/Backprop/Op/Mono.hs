{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Numeric.Backprop.Op.Mono
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides monomorphic versions of the types and combinators in
-- "Numeric.Backprop.Op", for usage with "Numeric.Backprop.Mono" and
-- "Numeric.Backprop.Mono.Implicit".
--
-- Note that 'Op' is a /subset/ or /subtype/ of 'OpM', and so, any function
-- that expects an @'OpM' m as a@ (or an @'Numeric.Backprop.OpB' s as a@)
-- can be given an @'Op' as a@ and it'll work just fine.
--

module Numeric.Backprop.Op.Mono (
  -- * Types
  -- ** Op and synonyms
    Op, pattern Op, OpM, pattern OpM
  -- ** Vector types
  , VecT(..), Vec
  -- * Running
  -- ** Pure
  , runOp, gradOp, gradOp', gradOpWith, gradOpWith', runOp'
  -- ** Monadic
  , runOpM, gradOpM, gradOpM', gradOpWithM, gradOpWithM', runOpM'
  -- * Creation
  , op0, opConst, composeOp
  -- ** Automatic creation using the /ad/ library
  , op1, op2, op3, opN
  , Replicate
  -- ** Giving gradients directly
  , op1', op2', op3'
  -- * Utility
  -- ** Vectors
  , pattern (:+), (*:), (+:), head'
  -- ** Type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
 ) where

import           Data.Bifunctor
import           Data.Reflection                  (Reifies)
import           Data.Type.Nat
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.AD.Internal.Reverse      (Reverse, Tape)
import           Numeric.AD.Mode.Forward          (AD, Forward)
import           Type.Class.Known
import           Type.Family.Nat
import qualified Numeric.Backprop.Internal.Helper as BP
import qualified Numeric.Backprop.Op              as BP

type Op n a b  = BP.Op (Replicate n a) b
type OpM m n a = BP.OpM m (Replicate n a)

pattern Op :: Known Nat n => (Vec n a -> (b, Maybe b -> Vec n a)) -> Op n a b
pattern Op runOp' <- BP.Op (\f xs -> (second . fmap) (prodAlong xs)
                                    . f
                                    . vecToProd
                                    $ xs
                             -> runOp'
                           )
  where
    Op f = BP.Op (\xs -> (second . fmap) vecToProd . f . prodToVec' known $ xs)

pattern OpM :: (Known Nat n, Functor m) => (Vec n a -> m (b, Maybe b -> m (Vec n a))) -> OpM m n a b
pattern OpM runOpM' <- BP.OpM (\f xs -> (fmap . second . fmap . fmap) (prodAlong xs)
                                      . f
                                      . vecToProd
                                      $ xs
                               -> runOpM'
                              )
  where
    OpM f = BP.OpM (\xs -> (fmap . second . fmap . fmap) vecToProd . f . prodToVec' known $ xs)

op0 :: a -> Op N0 b a
op0 x = BP.op0 x

opConst :: forall n a b. (Known Nat n, Num b) => a -> Op n b a
opConst x = BP.opConst' (BP.nSummers' @n @b known) x

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

op1'
    :: (a -> (b, Maybe b -> a))
    -> Op N1 a b
op1' = BP.op1'

op2'
    :: (a -> a -> (b, Maybe b -> (a, a)))
    -> Op N2 a b
op2' = BP.op2'

op3'
    :: (a -> a -> a -> (b, Maybe b -> (a, a, a)))
    -> Op N3 a b
op3' = BP.op3'

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

gradOp' :: Op n a b -> Vec n a -> (b, Vec n a)
gradOp' o = second ($ Nothing) . runOp' o

runOpM' :: Functor m => OpM m n a b -> Vec n a -> m (b, Maybe b -> m (Vec n a))
runOpM' o xs = (fmap . second . fmap . fmap) (prodAlong xs)
             . BP.runOpM' o
             . vecToProd
             $ xs

runOpM :: Functor m => OpM m n a b -> Vec n a -> m b
runOpM o = fmap fst . runOpM' o

gradOpM :: Monad m => OpM m n a b -> Vec n a -> m (Vec n a)
gradOpM o i = do
    (_, gF) <- runOpM' o i
    gF Nothing

gradOpM' :: Monad m => OpM m n a b -> Vec n a -> m (b, Vec n a)
gradOpM' o i = do
    (x, gF) <- runOpM' o i
    g <- gF Nothing
    return (x, g)

gradOpWithM' :: Monad m => OpM m n a b -> Vec n a -> Maybe b -> m (Vec n a)
gradOpWithM' o i d = do
    (_, gF) <- runOpM' o i
    gF d

gradOpWithM :: Monad m => OpM m n a b -> Vec n a -> b -> m (Vec n a)
gradOpWithM o i d = do
    (_, gF) <- runOpM' o i
    gF (Just d)

-- | Compose 'OpM's together, similar to '.'.  But, because all 'OpM's are
-- \(\mathbb{R}^N \rightarrow \mathbb{R}\), this is more like 'sequence'
-- for functions, or @liftAN@.
--
-- That is, given an @o@ of @'OpM' m n a b@s, it can compose them with an
-- @'OpM' m o b c@ to create an @'OpM' m o a c@.
composeOp
    :: forall m n o a b c. (Monad m, Num a, Known Nat n)
    => VecT o (OpM m n a) b
    -> OpM m o b c
    -> OpM m n a c
composeOp v o = BP.composeOp' (BP.nSummers' @n @a known) (vecToProd v) o

