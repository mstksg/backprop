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


module Numeric.Backprop.Mono (
  -- * Types
    BP, BPOp, BVar
  , Op
  -- * BP
  -- ** Backprop
  , backprop, evalBPOp, gradBPOp
  , bpOp
  -- ** Inputs
  , withInps, implicitly
  -- * Refs
  , constRef
  , inpRef, inpRefs
  -- ** From Ops
  , opRef, (~$)
  , opRef1, opRef2, opRef3
  , (-$)
  -- ** Combining
  , liftR, liftR1, liftR2, liftR3
  -- * Op
  , op1, op2, op3, opN
  -- * Utility
  -- ** Type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  -- ** Vectors
  , VecT(..), Vec, pattern (:+), (*:), (+:), head'
  ) where

import           Data.Type.Fin
import           Data.Type.Nat
import           Data.Type.Product hiding  (head')
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.Backprop.Op.Mono
import           Type.Class.Known
import qualified Numeric.Backprop          as BP

type BP s n a      = BP.BP s (Replicate n a)
type BVar s n a    = BP.BVar s (Replicate n a)
type BPOp s n a b  = BP s n a (BVar s n a b)
type BPOpI s n a b = VecT n (BVar s n a) a -> BVar s n a b

type OpB s n a b   = BP.OpB s (Replicate n a) b

opRef
    :: forall s m n a b. Num b
    => VecT m (BVar s n a) a
    -> OpB s m a b
    -> BP s n a (BVar s n a b)
opRef i o = BP.opRef (vecToProd i) o

infixr 1 ~$
(~$)
    :: Num b
    => OpB s m a b
    -> VecT m (BVar s n a) a
    -> BP s n a (BVar s n a b)
(~$) = flip opRef

infixr 1 -$
(-$)
    :: forall s m n a b. Num b
    => BPOp s m a b
    -> VecT m (BVar s n a) a
    -> BP s n a (BVar s n a b)
o -$ xs = bpOp @_ @_ @a o ~$ xs

constRef
    :: b
    -> BVar s n a b
constRef = BP.constRef

opRef1
    :: forall s n a b. Num b
    => BVar s n a a
    -> OpB s N1 a b
    -> BP s n a (BVar s n a b)
opRef1 x o = opRef @_ @_ @n (x :* ØV) o

opRef2
    :: forall s n a b. Num b
    => BVar s n a a
    -> BVar s n a a
    -> OpB s N2 a b
    -> BP s n a (BVar s n a b)
opRef2 x y o = opRef @_ @_ @n (x :* y :* ØV) o

opRef3
    :: forall s n a b. Num b
    => BVar s n a a
    -> BVar s n a a
    -> BVar s n a a
    -> OpB s N3 a b
    -> BP s n a (BVar s n a b)
opRef3 x y z o = opRef @_ @_ @n (x :* y :* z :* ØV) o

backprop
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> (b, Vec n a)
backprop bp i = (x, prodAlong i g)
  where
    (x, g) = BP.backprop' (toSummers i) (toUnities i) bp (vecToProd i)

evalBPOp
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> b
evalBPOp bp = fst . backprop bp

gradBPOp
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> Vec n a
gradBPOp bp = snd . backprop bp

bpOp
    :: forall s n a b. ()
    => BPOp s n a b
    -> OpB s n a b
bpOp = undefined


inpRef
    :: Fin n
    -> BVar s n a a
inpRef = BP.inpRef . finIndex

inpRefs
    :: Known Nat n
    => VecT n (BVar s n a) a
inpRefs = vgen_ inpRef

withInps
    :: Known Nat n
    => (VecT n (BVar s n a) a -> BP s n a b)
    -> BP s n a b
withInps f = f inpRefs

implicitly
    :: Known Nat n
    => BPOpI s n a b
    -> BPOp s n a b
implicitly f = withInps (return . f)

liftR
    :: OpB s m a b
    -> VecT m (BVar s n r) a
    -> BVar s n r b
liftR o = BP.liftR o . vecToProd

liftR1
    :: OpB s N1 a a
    -> BVar s n r a
    -> BVar s n r a
liftR1 = BP.liftR1

liftR2
    :: OpB s N2 a a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
liftR2 = BP.liftR2

liftR3
    :: OpB s N3 a a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
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
