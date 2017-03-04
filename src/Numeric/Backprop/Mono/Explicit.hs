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


module Numeric.Backprop.Mono.Explicit (
  -- * Types
    BP, BPOp, BPRef
  , Op
  , Replicate
  -- * BP
  -- ** Backprop
  , backprop, runBPOp, gradBPOp
  -- ** Inputs
  , withInps
  -- * Refs
  , constRef
  , inpRef, inpRefs
  -- ** From Ops
  , opRef, (-$)
  , opRef1, opRef2, opRef3
  -- ** Transforming BP
  , plugBP
  -- ** Combining
  , liftR, liftR1, liftR2, liftR3
  -- * Op
  , op1, op2, op3, opN
  , runOp', runOp, gradOp, gradOpWith, gradOpWith'
  -- * Utility
  -- ** Type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  -- ** Vectors
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
import qualified Numeric.Backprop.Explicit   as BP
import qualified Numeric.Backprop.Internal   as BP

type BP s n a     = BP.BP s (Replicate n a)
type BPRef s n a  = BP.BPRef s (Replicate n a)
type BPOp s n a b = BP s n a (BPRef s n a b)
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

opRef
    :: forall s m n a b. Num b
    => VecT m (BPRef s n a) a
    -> Op m a b
    -> BP s n a (BPRef s n a b)
opRef i o = BP.opRef (vecToProd i) o

infixr 1 -$
(-$)
    :: Num b
    => Op m a b
    -> VecT m (BPRef s n a) a
    -> BP s n a (BPRef s n a b)
(-$) = flip opRef

constRef
    :: b
    -> BPRef s n a b
constRef = BP.constRef

opRef1
    :: forall s n a b. Num b
    => BPRef s n a a
    -> Op N1 a b
    -> BP s n a (BPRef s n a b)
opRef1 x o = opRef @_ @_ @n (x :* ØV) o

opRef2
    :: forall s n a b. Num b
    => BPRef s n a a
    -> BPRef s n a a
    -> Op N2 a b
    -> BP s n a (BPRef s n a b)
opRef2 x y o = opRef @_ @_ @n (x :* y :* ØV) o

opRef3
    :: forall s n a b. Num b
    => BPRef s n a a
    -> BPRef s n a a
    -> BPRef s n a a
    -> Op N3 a b
    -> BP s n a (BPRef s n a b)
opRef3 x y z o = opRef @_ @_ @n (x :* y :* z :* ØV) o

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

gradBPOp
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> Vec n a
gradBPOp bp = snd . backprop bp



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

liftR
    :: Op m a b
    -> VecT m (BPRef s n r) a
    -> BPRef s n r b
liftR o = BP.liftR o . vecToProd

liftR1
    :: Op N1 a a
    -> BPRef s n r a
    -> BPRef s n r a
liftR1 = BP.liftR1

liftR2
    :: Op N2 a a
    -> BPRef s n r a
    -> BPRef s n r a
    -> BPRef s n r a
liftR2 = BP.liftR2

liftR3
    :: Op N3 a a
    -> BPRef s n r a
    -> BPRef s n r a
    -> BPRef s n r a
    -> BPRef s n r a
liftR3 = BP.liftR3








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
