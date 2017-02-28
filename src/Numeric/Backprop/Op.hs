{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Backprop.Op
  ( Op(..)
  , runOp, gradOp, gradOpWith, gradOpWith'
  , op0
  , op1, op2, op3, opN
  , op1', op2', op3', opN'
  , Replicate
  ) where

import           Data.Bifunctor
import           Data.Reflection                (Reifies)
import           Data.Type.Combinator
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.AD
import           Numeric.AD.Internal.Reverse    (Reverse, Tape)
import           Numeric.AD.Mode.Forward hiding (grad')
import           Numeric.Backprop.Internal
import           Type.Class.Known

runOp :: Op f as a -> Prod f as -> f a
runOp o = fst . runOp' o

gradOpWith' :: Op f as a -> Prod f as -> Maybe (f a) -> Prod f as
gradOpWith' o = snd . runOp' o

gradOpWith :: Op f as a -> Prod f as -> f a -> Prod f as
gradOpWith o i = gradOpWith' o i . Just

gradOp :: Op f as a -> Prod f as -> Prod f as
gradOp o i = gradOpWith' o i Nothing

op0 :: f a -> Op f '[] a
op0 x = Op $ \case
    Ø -> (x, const Ø)

op1'
    :: (f a -> (f b, Maybe (f b) -> f a))
    -> Op f '[a] b
op1' f = Op $ \case
    x :< Ø ->
      let (y, dx) = f x
      in  (y, only . dx)

op2'
    :: (f a -> f b -> (f c, Maybe (f c) -> (f a, f b)))
    -> Op f '[a,b] c
op2' f = Op $ \case
    x :< y :< Ø ->
      let (z, dxdy) = f x y
      in  (z, (\(dx,dy) -> dx :< dy :< Ø) . dxdy)

op3'
    :: (f a -> f b -> f c -> (f d, Maybe (f d) -> (f a, f b, f c)))
    -> Op f '[a,b,c] d
op3' f = Op $ \case
    x :< y :< z :< Ø ->
      let (q, dxdydz) = f x y z
      in  (q, (\(dx, dy, dz) -> dx :< dy :< dz :< Ø) . dxdydz)

opN' :: (Num (f a), Known Nat n)
     => (Vec n (f a) -> (f b, Maybe (f b) -> Vec n (f a)))
     -> Op f (Replicate n a) b
opN' f = Op $ (second . fmap) (vecToProd . vmap getI)
            . f
            . vmap I
            . prodToVec' known

op1 :: Num (f a)
    => (forall s. AD s (Forward (f a)) -> AD s (Forward (f a)))
    -> Op f '[a] a
op1 f = op1' $ \x ->
    let (z, dx) = diff' f x
    in  (z, maybe dx (* dx))

op2 :: Num (f a)
    => (forall s. Reifies s Tape => Reverse s (f a) -> Reverse s (f a) -> Reverse s (f a))
    -> Op f '[a,a] a
op2 f = opN $ \case I x :* I y :* ØV -> f x y

op3 :: Num (f a)
    => (forall s. Reifies s Tape => Reverse s (f a) -> Reverse s (f a) -> Reverse s (f a) -> Reverse s (f a))
    -> Op f '[a,a,a] a
op3 f = opN $ \case I x :* I y :* I z :* ØV -> f x y z

opN :: (Num (f a), Known Nat n)
    => (forall s. Reifies s Tape => Vec n (Reverse s (f a)) -> Reverse s (f a))
    -> Op f (Replicate n a) a
opN f = opN' $ \xs ->
    let (y, dxs) = grad' f xs
    in  (y, maybe dxs (\q -> (q *) <$> dxs))
