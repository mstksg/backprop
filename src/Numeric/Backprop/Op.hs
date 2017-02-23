{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Backprop.Op
  ( Op(..)
  , Scaler(..)
  , Summer(..)
  , op0
  , op1, op1'
  , op2, op2'
  , op3, op3'
  ) where

import           Data.Reflection                (Reifies)
import           Data.Type.Combinator
import           Data.Type.Product
import           Numeric.AD
import           Numeric.AD.Internal.Reverse    (Reverse, Tape)
import           Numeric.AD.Mode.Forward hiding (grad')
import           Numeric.Backprop.Internal

op0 :: a -> Op '[] a
op0 x = Op $ \case
    Ø -> (x, Ø)

op1 :: (a -> (b, a)) -> Op '[a] b
op1 f = Op $ \case
    I x :< Ø ->
      let (y, dx) = f x
      in  (y, I dx :< Ø)

op2 :: (a -> b -> (c, (a, b))) -> Op '[a,b] c
op2 f = Op $ \case
    I x :< I y :< Ø ->
      let (z, (dx, dy)) = f x y
      in  (z, I dx :< I dy :< Ø)

op3 :: (a -> b -> c -> (d, (a, b, c))) -> Op '[a,b,c] d
op3 f = Op $ \case
    I x :< I y :< I z :< Ø ->
      let (q, (dx, dy, dz)) = f x y z
      in  (q, I dx :< I dy :< I dz :< Ø)

op1'
    :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Op '[a] a
op1' f = op1 $ diff' f

op2'
    :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a] a
op2' f = op2 $ \x y ->
    let (z, [dx, dy]) = grad' (\[x',y'] -> f x' y') [x,y]
    in  (z, (dx, dy))

op3'
    :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a,a] a
op3' f = op3 $ \x y z ->
    let (q, [dx, dy, dz]) = grad' (\[x',y',z'] -> f x' y' z') [x,y,z]
    in  (q, (dx, dy, dz))
