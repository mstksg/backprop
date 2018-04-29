{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Type.Util (
    Replicate
  , unzipP
  , zipP
  , zipWithPM_
  , zipWithPM3_
  , vecToProd
  , vecLen
  , lengthProd
  , listToVecDef
  , fillProd
  , zipVecList
  ) where

import           Data.Bifunctor
import           Data.Type.Conjunction
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Vector
import           Type.Class.Witness
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

vecLen
    :: VecT n f a
    -> Nat n
vecLen = \case
    ØV      -> Z_
    _ :* xs -> S_ (vecLen xs)

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

zipWithPM3_
    :: forall m f g h as. Applicative m
    => (forall a. f a -> g a -> h a -> m ())
    -> Prod f as
    -> Prod g as
    -> Prod h as
    -> m ()
zipWithPM3_ f = go
  where
    go :: forall bs. Prod f bs -> Prod g bs -> Prod h bs -> m ()
    go = \case
      Ø -> \case
        Ø -> \case
          Ø -> pure ()
      x :< xs -> \case
        y :< ys -> \case
          z :< zs -> f x y z *> go xs ys zs

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

lengthProd
    :: (forall a. f a)
    -> Length as
    -> Prod f as
lengthProd x = \case
    LZ   -> Ø
    LS l -> x :< lengthProd x l

listToVecDef
    :: forall f a n. ()
    => f a
    -> Nat n
    -> [f a]
    -> VecT n f a
listToVecDef d = go
  where
    go :: Nat m -> [f a] -> VecT m f a
    go = \case
      Z_   -> const ØV
      S_ n -> \case
        []   -> d :* vrep d \\ n
        x:xs -> x :* go n xs

fillProd
    :: forall f g as c. ()
    => (forall a. f a -> c -> g a)
    -> Prod f as
    -> [c]
    -> Maybe (Prod g as)
fillProd f = go
  where
    go :: Prod f bs -> [c] -> Maybe (Prod g bs)
    go = \case
      Ø -> \_ -> Just Ø
      x :< xs -> \case
        []   -> Nothing
        y:ys -> (f x y :<) <$> go xs ys

zipVecList
    :: forall a b c f g n. ()
    => (f a -> Maybe b -> g c)
    -> VecT n f a
    -> [b]
    -> VecT n g c
zipVecList f = go
  where
    go :: VecT m f a -> [b] -> VecT m g c
    go = \case
      ØV -> const ØV
      x :* xs -> \case
        []   -> f x Nothing  :* go xs []
        y:ys -> f x (Just y) :* go xs ys
