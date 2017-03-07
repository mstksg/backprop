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
    BP, BPOp, BPOpI, BVar
  , Op, OpB
  -- * BP
  -- ** Backprop
  , backprop, evalBPOp, gradBPOp
  , bpOp
  -- ** Inputs
  , withInps, implicitly
  -- * Vars
  , constVar
  , inpVar, inpVars
  -- ** From Ops
  , opVar, (~$)
  , opVar1, opVar2, opVar3
  , (-$)
  -- ** Combining
  , liftB, (.$), liftB1, liftB2, liftB3
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
import           Data.Type.Product hiding         (head')
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.Backprop.Internal.Helper
import           Numeric.Backprop.Op.Mono
import           Type.Class.Known
import qualified Numeric.Backprop                 as BP

type BP s n a      = BP.BP s (Replicate n a)
type BVar s n a    = BP.BVar s (Replicate n a)
type BPOp s n a b  = BP s n a (BVar s n a b)
type BPOpI s n a b = VecT n (BVar s n a) a -> BVar s n a b

type OpB s n a b   = BP.OpB s (Replicate n a) b

opVar
    :: forall s m n a b. Num b
    => OpB s m a b
    -> VecT m (BVar s n a) a
    -> BP s n a (BVar s n a b)
opVar o = BP.opVar o . vecToProd

infixr 1 ~$
(~$)
    :: Num b
    => OpB s m a b
    -> VecT m (BVar s n a) a
    -> BP s n a (BVar s n a b)
(~$) = opVar

infixr 1 -$
(-$)
    :: forall s m n a b. (Num a, Num b, Known Nat m)
    => BPOp s m a b
    -> VecT m (BVar s n a) a
    -> BP s n a (BVar s n a b)
o -$ xs = bpOp @_ @_ @a o ~$ xs

constVar
    :: b
    -> BVar s n a b
constVar = BP.constVar

opVar1
    :: forall s n a b. Num b
    => OpB s N1 a b
    -> BVar s n a a
    -> BP s n a (BVar s n a b)
opVar1 o x = opVar @_ @_ @n o (x :* ØV)

opVar2
    :: forall s n a b. Num b
    => OpB s N2 a b
    -> BVar s n a a
    -> BVar s n a a
    -> BP s n a (BVar s n a b)
opVar2 o x y = opVar @_ @_ @n o (x :* y :* ØV)

opVar3
    :: forall s n a b. Num b
    => OpB s N3 a b
    -> BVar s n a a
    -> BVar s n a a
    -> BVar s n a a
    -> BP s n a (BVar s n a b)
opVar3 o x y z = opVar @_ @_ @n o (x :* y :* z :* ØV)

backprop
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> (b, Vec n a)
backprop bp i = (x, prodAlong i g)
  where
    (x, g) = BP.backprop' (toSummers i) (toUnities i) bp (vecToProd i)

evalBPOp
    :: forall n a b. ()
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> b
evalBPOp bp = BP.evalBPOp bp . vecToProd

gradBPOp
    :: forall n a b. Num a
    => (forall s. BPOp s n a b)
    -> Vec n a
    -> Vec n a
gradBPOp bp = snd . backprop bp

bpOp
    :: forall s n a b. (Num a, Known Nat n)
    => BPOp s n a b
    -> OpB s n a b
bpOp b = BP.bpOp' (nSummers' @n @a n) (nUnities' @n @a n) b
  where
    n :: Nat n
    n = known


inpVar
    :: Fin n
    -> BVar s n a a
inpVar = BP.inpVar . finIndex

inpVars
    :: Known Nat n
    => VecT n (BVar s n a) a
inpVars = vgen_ inpVar

withInps
    :: Known Nat n
    => (VecT n (BVar s n a) a -> BP s n a b)
    -> BP s n a b
withInps f = f inpVars

implicitly
    :: Known Nat n
    => BPOpI s n a b
    -> BPOp s n a b
implicitly f = withInps (return . f)

liftB
    :: forall s m n a b r. ()
    => OpB s m a b
    -> VecT m (BVar s n r) a
    -> BVar s n r b
liftB o = BP.liftB o . vecToProd

(.$)
    :: forall s m n a b r. ()
    => OpB s m a b
    -> VecT m (BVar s n r) a
    -> BVar s n r b
o .$ x = liftB @_ @_ @_ @_ @_ @r o x

liftB1
    :: OpB s N1 a a
    -> BVar s n r a
    -> BVar s n r a
liftB1 = BP.liftB1

liftB2
    :: OpB s N2 a a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
liftB2 = BP.liftB2

liftB3
    :: OpB s N3 a a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
liftB3 = BP.liftB3








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

