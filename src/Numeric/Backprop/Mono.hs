{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
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
  -- , inpRef, inpRefs, withInps
  , Op(..)
  ) where

import           Data.Kind
import           Data.Type.Combinator
import           Data.Type.Equality
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Vector
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Family.Nat
import qualified Numeric.Backprop     as BP

type family Rep (n :: N) (a :: k) :: [k] where
    Rep 'Z     a = '[]
    Rep ('S n) a = a ': Rep n a

type BP s n a       = BP.BP s (Rep n a)
type BPRef s n a    = BP.BPRef s (Rep n a)
type Op n a         = BP.Op (Rep n a)

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

-- inpRef
--     :: Index as a
--     -> BPRef s as a
-- inpRef = BPRInp

-- inpRefs
--     :: Known Length as
--     => Prod (BPRef s as) as
-- inpRefs = map1 BPRInp indices

-- withInps
--     :: Known Length as
--     => (Prod (BPRef s as) as -> BP s as a)
--     -> BP s as a
-- withInps f = f (map1 BPRInp indices)






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


-- data Replicate :: N -> k -> [k] -> Type where
--     RZ :: Replicate 'Z a '[]
--     RS :: Replicate n a as -> Replicate ('S n) a (a ': as)

-- replicate
--     :: Nat n
--     -> Replicate n a (Rep n a)
-- replicate = \case
--     Z_   -> RZ
--     S_ n -> RS (replicate n)

-- eqReplicate
--     :: Replicate n a bs
--     -> Replicate n a cs
--     -> (bs :~: cs)
-- eqReplicate = \case
--     RZ -> \case
--       RZ -> Refl
--     RS r1 -> \case
--       RS r2 -> case eqReplicate r1 r2 of
--                  Refl -> Refl

-- data BP :: Type -> N -> Type -> Type -> Type where
--     BP :: { bpRep :: Replicate n a as
--           , bpBP  :: BP.BP s as b
--           }
--        -> BP s n a b

-- deriving instance Functor (BP s n a)

-- instance Known Nat n => Applicative (BP s n a) where
--     pure x = BP (replicate known) (pure x)
--     (<*>) = \case
--         BP rf bf -> \case
--           BP rx bx -> case eqReplicate rf rx of
--                         Refl -> BP rf (bf <*> bx)

-- instance Known Nat n => Monad (BP s n a) where
--     return x = BP (replicate known) (pure x)
--     (>>=) = \case
--       BP r1 x -> \f -> BP r1 $ do
--         x' <- x
--         case f x' of
--           BP r2 y -> case eqReplicate r1 r2 of
--                        Refl -> y

-- data BPRef :: Type -> N -> Type -> Type where
--     BPR :: { bprRep   :: Replicate n a as
--            , bprBPRef :: BP.BPRef s as a
--            }
--         -> BPRef s n a

-- data BPRef :: Type -> N -> Type -> Type -> Type where
--     BPR :: { bprBPRef :: BP.BPRef s (Rep n a) b
--            }
--         -> BPRef s n a b

-- data Op :: N -> Type -> Type where
--     Op :: { opRep :: Replicate n a as
--           , opOp  :: BP.Op as a
--           }
--        -> Op n a

                   -- = BP.BP s (Replicate n a)
-- type BPRef s n a    = BP.BPRef s (Replicate n a) a
-- type Op n a         = BP.Op (Replicate n a) a

-- type family Replicate (n :: N) (a :: k) = (as :: [k]) | as -> n where
--     Replicate 'Z a     = '[]
--     Replicate ('S n) a = a ': Replicate n a

-- type BPRef s n a    = BP.BPRef s (Replicate n a)
