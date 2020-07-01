{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Type.Util (
    runzipWith
  , rzipWithM_
  , Replicate
  , VecT(.., (:+))
  , vmap
  , withVec
  , vecToRec
  , fillRec
  , zipVecList
  , splitRec
  , p1, p2, s1, s2
  ) where

import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           GHC.Generics
import           Lens.Micro

runzipWith
    :: forall f g h. ()
    => (forall x. f x -> (g x, h x))
    -> (forall xs. Rec f xs -> (Rec g xs, Rec h xs))
runzipWith f = go
  where
    go :: forall ys. Rec f ys -> (Rec g ys, Rec h ys)
    go = \case
      RNil    -> (RNil, RNil)
      x :& xs -> let (y , z ) = f x
                     (ys, zs) = go xs
                 in  (y :& ys, z :& zs)
{-# INLINE runzipWith #-}

data VecT :: Nat -> (k -> Type) -> k -> Type where
    VNil :: VecT 'Z f a
    (:*) :: !(f a) -> VecT n f a -> VecT ('S n) f a

pattern (:+) :: a -> VecT n Identity a -> VecT ('S n) Identity a
pattern x :+ xs = Identity x :* xs

vmap
    :: forall n f g a. ()
    => (f a -> g a) -> VecT n f a -> VecT n g a
vmap f = go
  where
    go :: VecT m f a -> VecT m g a
    go = \case
      VNil -> VNil
      x :* xs -> f x :* go xs
{-# INLINE vmap #-}

withVec
    :: [f a]
    -> (forall n. VecT n f a -> r)
    -> r
withVec = \case
    []   -> ($ VNil)
    x:xs -> \f -> withVec xs (f . (x :*))
{-# INLINE withVec #-}

type family Replicate (n :: Nat) (a :: k) = (as :: [k]) | as -> n where
    Replicate 'Z     a = '[]
    Replicate ('S n) a = a ': Replicate n a

vecToRec
    :: VecT n f a
    -> Rec f (Replicate n a)
vecToRec = \case
    VNil    -> RNil
    x :* xs -> x :& vecToRec xs
{-# INLINE vecToRec #-}

fillRec
    :: forall f g as c. ()
    => (forall a. f a -> c -> g a)
    -> Rec f as
    -> [c]
    -> Maybe (Rec g as)
fillRec f = go
  where
    go :: Rec f bs -> [c] -> Maybe (Rec g bs)
    go = \case
      RNil -> \_ -> Just RNil
      x :& xs -> \case
        []   -> Nothing
        y:ys -> (f x y :&) <$> go xs ys
{-# INLINE fillRec #-}

rzipWithM_
    :: forall h f g as. Applicative h
    => (forall a. f a -> g a -> h ())
    -> Rec f as
    -> Rec g as
    -> h ()
rzipWithM_ f = go
  where
    go :: forall bs. Rec f bs -> Rec g bs -> h ()
    go = \case
      RNil -> \case
        RNil -> pure ()
      x :& xs -> \case
        y :& ys -> f x y *> go xs ys
{-# INLINE rzipWithM_ #-}

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
      VNil -> const VNil
      x :* xs -> \case
        []   -> f x Nothing  :* go xs []
        y:ys -> f x (Just y) :* go xs ys
{-# INLINE zipVecList #-}

splitRec
    :: forall f as bs. RecApplicative as
    => Rec f (as ++ bs)
    -> (Rec f as, Rec f bs)
splitRec = go (rpure Proxy)
  where
    go :: Rec Proxy as' -> Rec f (as' ++ bs) -> (Rec f as', Rec f bs)
    go = \case
      RNil -> (RNil,)
      _ :& ps -> \case
        x :& xs -> first (x :&) $ go ps xs
{-# INLINE splitRec #-}

p1 :: Lens' ((f :*: g) a) (f a)
p1 f (x :*: y) = (:*: y) <$> f x
{-# INLINE p1 #-}

p2 :: Lens' ((f :*: g) a) (g a)
p2 f (x :*: y) = (x :*:) <$> f y
{-# INLINE p2 #-}

s1 :: Traversal' ((f :+: g) a) (f a)
s1 f (L1 x) = L1 <$> f x
s1 _ (R1 y) = pure (R1 y)
{-# INLINE s1 #-}

s2 :: Traversal' ((f :+: g) a) (g a)
s2 _ (L1 x) = pure (L1 x)
s2 f (R1 y) = R1 <$> f y
{-# INLINE s2 #-}
