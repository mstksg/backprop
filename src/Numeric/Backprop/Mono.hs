{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}


module Numeric.Backprop.Mono
  ( BP
  , BPRef
  , newBPRef
  , newBPRef0
  , newBPRef1
  , newBPRef2
  , newBPRef3
  , backprop
  , inpRef, inpRefs, withInps
  , Op
  , BP.runOp', BP.runOp, BP.gradOp
  ) where

import           Data.Type.Fin
import           Data.Type.Index
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Vector
import           Type.Class.Known
import           Type.Family.Nat
import qualified Numeric.Backprop          as BP
import qualified Numeric.Backprop.Internal as BP
import qualified Numeric.Backprop.Op       as BP

type family Rep (n :: N) (a :: k) = (as :: [k]) | as -> n where
    Rep 'Z     a = '[]
    Rep ('S n) a = a ': Rep n a

type BP s n a    = BP.BP s (Rep n a)
type BPRef s n a = BP.BPRef s (Rep n a)
type Op n a      = BP.Op (Rep n a)

newBPRef
    :: forall s m n a b. Num b
    => VecT m (BPRef s n a) a
    -> Op m a b
    -> BP s n a (BPRef s n a b)
newBPRef i o = BP.newBPRef (vecToProd i) o (BP.Summer sum)

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
    => (forall s. BP s n a (BPRef s n a b))
    -> Vec n a
    -> (b, Vec n a)
backprop bp i = (x, prodAlong i g)
  where
    (x, g) = BP.backprop bp (toSummers i) (toUnities i) (vecToProd i)

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






prodAlong
    :: VecT n f b
    -> Prod f (Rep n a)
    -> VecT n f a
prodAlong = \case
    ØV -> \case
      Ø       -> ØV
    _ :* v -> \case
      x :< xs -> x :* prodAlong v xs

toSummers
    :: Num a
    => Vec n a
    -> Prod BP.Summer (Rep n a)
toSummers = \case
    ØV      -> Ø
    _ :* xs -> BP.Summer sum :< toSummers xs

toUnities
    :: Num a
    => Vec n a
    -> Prod BP.Unity (Rep n a)
toUnities = \case
    ØV      -> Ø
    _ :* xs -> BP.Unity 1 :< toUnities xs

vecToProd
    :: VecT n f a
    -> Prod f (Rep n a)
vecToProd = \case
    ØV      -> Ø
    x :* xs -> x :< vecToProd xs

finIndex
    :: Fin n
    -> Index (Rep n a) a
finIndex = \case
    FZ   -> IZ
    FS f -> IS (finIndex f)
