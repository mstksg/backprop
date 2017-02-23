{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Numeric.Backprop.Op where

import           Data.Reflection                (Reifies)
import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Numeric.AD
import           Numeric.AD.Internal.Reverse    (Reverse, Tape)
import           Numeric.AD.Mode.Forward hiding (grad')
import           Type.Class.Known

newtype Op as a = Op { runOp :: Tuple as -> (a, Tuple as) }
    deriving (Functor)

newtype Scaler a b = Scaler { runScaler :: a -> b -> b }
newtype Summer a   = Summer { runSummer :: [a] -> a }

instance Num a => Known (Scaler a) a where
    type KnownC (Scaler a) a = Num a
    known = Scaler (+)

instance Num a => Known Summer a where
    type KnownC Summer a = Num a
    known = Summer sum

summers
    :: (Known Length as, Every (Known Summer) as)
    => Prod Summer as
summers = known

scalers
    :: (Known Length as, Every (Known (Scaler a)) as)
    => Prod (Scaler a) as
scalers = known

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
