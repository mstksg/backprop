{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Type.Util (
    Replicate
  , unzipP
  , zipP
  , zipWithPM_
  , tagSum
  , indexP
  , vecToProd
  , prodToVec'
  , prodAlong
  , lengthProd
  , prodLength
  , vecLength
  , finIndex
  , replLen
  , replWit
  , traverse1_
  , itraverse1_
  , ifor1
  , ifor1_
  , for1
  , for1_
  ) where

import           Control.Applicative
import           Data.Bifunctor
-- import           Data.Kind
import           Data.Monoid hiding    (Sum)
import           Data.Type.Conjunction
import           Data.Type.Fin
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Sum
import           Data.Type.Vector
import           Lens.Micro
import           Type.Class.Higher
-- import           Type.Class.Known
import           Type.Class.Witness
-- import           Type.Family.List
import           Type.Family.Nat

-- | @'Replicate' n a@ is a list of @a@s repeated @n@ times.
--
-- >>> :kind! Replicate N3 Int
-- '[Int, Int, Int]
-- >>> :kind! Replicate N5 Double
-- '[Double, Double, Double, Double, Double]
type family Replicate (n :: N) (a :: k) = (as :: [k]) | as -> n where
    Replicate 'Z     a = '[]
    Replicate ('S n) a = a ': Replicate n a

vecToProd
    :: VecT n f a
    -> Prod f (Replicate n a)
vecToProd = \case
    ØV      -> Ø
    x :* xs -> x :< vecToProd xs

prodToVec'
    :: Nat n
    -> Prod f (Replicate n a)
    -> VecT n f a
prodToVec' = \case
    Z_   -> \case
      Ø       -> ØV
    S_ n -> \case
      x :< xs -> x :* prodToVec' n xs

prodAlong
    :: VecT n f b
    -> Prod f (Replicate n a)
    -> VecT n f a
prodAlong = \case
    ØV -> \case
      Ø       -> ØV
    _ :* v -> \case
      x :< xs -> x :* prodAlong v xs

finIndex
    :: Fin n
    -> Index (Replicate n a) a
finIndex = \case
    FZ   -> IZ
    FS f -> IS (finIndex f)

traverse1_
    :: (Applicative h, Traversable1 t)
    => (forall a. f a -> h ())
    -> t f b
    -> h ()
traverse1_ f = ($ pure ())
             . appEndo
             . getConst
             . foldMap1 (\y -> Const (Endo (f y *>)))

itraverse1_
    :: (Applicative h, IxFoldable1 i t)
    => (forall a. i b a -> f a -> h ())
    -> t f b
    -> h ()
itraverse1_ f = ($ pure ())
              . appEndo
              . getConst
              . ifoldMap1 (\i y -> Const (Endo (f i y *>)))

for1
    :: (Applicative h, Traversable1 t)
    => t f b
    -> (forall a. f a -> h (g a))
    -> h (t g b)
for1 x f = traverse1 f x

for1_
    :: (Applicative h, Traversable1 t)
    => t f b
    -> (forall a. f a -> h ())
    -> h ()
for1_ x f = traverse1_ f x

ifor1
    :: (Applicative h, IxTraversable1 i t)
    => t f b
    -> (forall a. i b a -> f a -> h (g a))
    -> h (t g b)
ifor1 x f = itraverse1 f x

ifor1_
    :: (Applicative h, IxFoldable1 i t)
    => t f b
    -> (forall a. i b a -> f a -> h ())
    -> h ()
ifor1_ x f = itraverse1_ f x

zipWithPM_
    :: forall h f g as. Applicative h
    => (forall a. f a -> g a -> h ())
    -> Prod f as
    -> Prod g as
    -> h ()
zipWithPM_ f = go
  where
    go :: forall bs. Prod f bs -> Prod g bs -> h ()
    go = \case
      Ø -> \case
        Ø -> pure ()
      x :< xs -> \case
        y :< ys -> f x y *> go xs ys


zipP
    :: Prod f as
    -> Prod g as
    -> Prod (f :&: g) as
zipP = \case
    Ø -> \case
      Ø       -> Ø
    x :< xs -> \case
      y :< ys -> x :&: y :< zipP xs ys
{-# INLINE zipP #-}

unzipP
    :: Prod (f :&: g) as
    -> (Prod f as, Prod g as)
unzipP = \case
    Ø               -> (Ø, Ø)
    (x :&: y) :< zs -> bimap (x :<) (y :<) (unzipP zs)

indexP :: Index as a -> Lens' (Prod g as) (g a)
indexP = \case
    IZ   -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> indexP i f xs

prodLength
    :: Prod f as
    -> Length as
prodLength = \case
    Ø       -> LZ
    _ :< xs -> LS (prodLength xs)

vecLength
    :: forall n f a. ()
    => VecT n f a
    -> Nat n
vecLength = \case
    ØV      -> Z_
    _ :* xs -> S_ (vecLength xs)

-- | Currently not used
tagSum
    :: Prod f as
    -> Sum g as
    -> Sum (f :&: g) as
tagSum = \case
    Ø       -> \case
    x :< xs -> \case
      InL y  -> InL (x :&: y)
      InR ys -> InR (tagSum xs ys)

replWit
    :: Nat n
    -> Wit (c a)
    -> Wit (Every c (Replicate n a))
replWit = \case
    Z_   -> \case
      Wit -> Wit
    S_ n -> \case
      c@Wit -> case replWit n c of
        Wit -> Wit

replLen
    :: forall n a. ()
    => Nat n
    -> Length (Replicate n a)
replLen = \case
    Z_   -> LZ
    S_ n -> LS (replLen @_ @a n)

lengthProd
    :: (forall a. f a)
    -> Length as
    -> Prod f as
lengthProd x = \case
    LZ   -> Ø
    LS l -> x :< lengthProd x l
