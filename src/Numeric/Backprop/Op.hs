{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.Backprop.Op
  ( Op(..)
  , runOp, gradOp, gradOpWith, gradOpWith'
  , op0
  , op1, op2, op3, opN, opCoerce, opTup, opIso
  , op1', op2', op3', opN', opCoerce', opTup', opIso'
  , Replicate
  ) where

import           Data.Bifunctor
import           Data.Coerce
import           Data.Maybe
import           Data.Reflection                (Reifies)
import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.AD
import           Numeric.AD.Internal.Reverse    (Reverse, Tape)
import           Numeric.AD.Mode.Forward hiding (grad')
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

runOp :: Op as a -> Tuple as -> a
runOp o = fst . runOp' o

gradOpWith' :: Op as a -> Tuple as -> Maybe a -> Tuple as
gradOpWith' o = snd . runOp' o

gradOpWith :: Op as a -> Tuple as -> a -> Tuple as
gradOpWith o i = gradOpWith' o i . Just

gradOp :: Op as a -> Tuple as -> Tuple as
gradOp o i = gradOpWith' o i Nothing

opCoerce' :: Coercible a b => Unity a -> Op '[a] b
opCoerce' u = opIso' u coerce coerce

opCoerce :: (Coercible a b, Num a) => Op '[a] b
opCoerce = opIso coerce coerce

opTup'
    :: Prod Unity as
    -> Op as (Tuple as)
opTup' u = Op $ \xs -> (xs, fromMaybe (map1 (I . getUnity) u))

opTup
    :: (Every Num as, Known Length as)
    => Op as (Tuple as)
opTup = opTup' (map1 ((// known) . every @_ @Num) indices)

opIso'
    :: Unity a
    -> (a -> b)
    -> (b -> a)
    -> Op '[ a ] b
opIso' u f g = op1' $ \x -> (f x, maybe (getUnity u) g)

opIso
    :: Num a
    => (a -> b)
    -> (b -> a)
    -> Op '[ a ] b
opIso = opIso' known

op0 :: a -> Op '[] a
op0 x = Op $ \case
    Ø -> (x, const Ø)

op2'
    :: (a -> b -> (c, Maybe c -> (a, b)))
    -> Op '[a,b] c
op2' f = Op $ \case
    I x :< I y :< Ø ->
      let (z, dxdy) = f x y
      in  (z, (\(dx,dy) -> dx ::< dy ::< Ø) . dxdy)

op3'
    :: (a -> b -> c -> (d, Maybe d -> (a, b, c)))
    -> Op '[a,b,c] d
op3' f = Op $ \case
    I x :< I y :< I z :< Ø ->
      let (q, dxdydz) = f x y z
      in  (q, (\(dx, dy, dz) -> dx ::< dy ::< dz ::< Ø) . dxdydz)

op2 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a] a
op2 f = opN $ \case I x :* I y :* ØV -> f x y

op3 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a,a] a
op3 f = opN $ \case I x :* I y :* I z :* ØV -> f x y z
