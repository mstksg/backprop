{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}


module Numeric.Backprop.Mono
  ( BP, BPOp, Replicate
  , BPRef
  , newBPRef
  , newBPRef0
  , newBPRef1
  , newBPRef2
  , newBPRef3
  , backprop
  , runBPOp
  , plugBP
  , inpRef, inpRefs, withInps
  , Op
  , op0, op1, op2, op3, opN
  , runOp', runOp, gradOp, gradOpWith, gradOpWith'
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  , VecT(..), Vec, pattern (:+), (*:), (+:)
  ) where

import           Data.Bifunctor
import           Data.Reflection             (Reifies)
import           Data.Type.Fin
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.AD.Internal.Reverse (Reverse, Tape)
import           Numeric.AD.Mode.Forward     (AD, Forward)
import           Type.Class.Known
import           Type.Family.Nat
import qualified Numeric.Backprop            as BP
import qualified Numeric.Backprop.Internal   as BP
import qualified Numeric.Backprop.Op         as BP

type BP s n a     = BP.BP s (Replicate n a)
type BPRef s n a  = BP.BPRef s (Replicate n a)
type BPOp s n a b = BP s n a (BPRef s n a b)
type Op n a       = BP.Op (Replicate n a)

op0 :: a -> Op N0 r a
op0 = BP.op0

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

newBPRef
    :: forall s m n a b. Num b
    => VecT m (BPRef s n a) a
    -> Op m a b
    -> BP s n a (BPRef s n a b)
newBPRef i o = BP.newBPRef (vecToProd i) o

newBPRef0
    :: forall s n a b. Num b
    => Op N0 a b
    -> BP s n a (BPRef s n a b)
newBPRef0 o = newBPRef @_ @_ @n @a ØV o

newBPRef1
    :: forall s n a b. Num b
    => BPRef s n a a
    -> Op N1 a b
    -> BP s n a (BPRef s n a b)
newBPRef1 x o = newBPRef @_ @_ @n (x :* ØV) o

newBPRef2
    :: forall s n a b. Num b
    => BPRef s n a a
    -> BPRef s n a a
    -> Op N2 a b
    -> BP s n a (BPRef s n a b)
newBPRef2 x y o = newBPRef @_ @_ @n (x :* y :* ØV) o

newBPRef3
    :: forall s n a b. Num b
    => BPRef s n a a
    -> BPRef s n a a
    -> BPRef s n a a
    -> Op N3 a b
    -> BP s n a (BPRef s n a b)
newBPRef3 x y z o = newBPRef @_ @_ @n (x :* y :* z :* ØV) o

backprop
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> (b, Vec n a)
backprop bp i = (x, prodAlong i g)
  where
    (x, g) = BP.backprop' bp (toSummers i) (toUnities i) (vecToProd i)

runBPOp
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> b
runBPOp bp = fst . backprop bp


plugBP
    :: (Num b, Num c)
    => VecT m (BPRef s n a) b
    -> BPOp s m b c
    -> BPOp s n a c
plugBP i = BP.plugBP' (vecToProd i) (toSummers i) (toUnities i) (BP.Summer sum)

inpRef
    :: Fin n
    -> BPRef s n a a
inpRef = BP.BPRInp . finIndex

inpRefs
    :: Known Nat n
    => VecT n (BPRef s n a) a
inpRefs = vgen_ inpRef

withInps
    :: Known Nat n
    => (VecT n (BPRef s n a) a -> BP s n a b)
    -> BP s n a b
withInps f = f inpRefs






toSummers
    :: Num a
    => VecT n f a
    -> Prod BP.Summer (Replicate n a)
toSummers = \case
    ØV      -> Ø
    _ :* xs -> BP.Summer sum :< toSummers xs

toUnities
    :: Num a
    => VecT n f a
    -> Prod BP.Unity (Replicate n a)
toUnities = \case
    ØV      -> Ø
    _ :* xs -> BP.Unity 1 :< toUnities xs
