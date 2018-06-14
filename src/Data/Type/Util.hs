{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Type.Util (
    rzipWith3
  , runzipWith
  , rzipWithM_
  , Replicate
  , Vec(..)
  , withVec
  , vecToRec
  , fillRec
  , rtraverse_
  -- , unzipP
  -- , zipP
  -- , zipWithPM_
  -- , zipWithPM3_
  -- , vecToProd
  -- , vecLen
  -- , lengthProd
  -- , listToVecDef
  -- , zipVecList
  -- , splitProd
  -- , traverse1_
  , p1, p2, s1, s2
  ) where

-- import           Data.Type.Conjunction hiding ((:*:))
-- import           Data.Type.Length
-- import           Data.Type.Nat
-- import           Data.Type.Product
-- import           Data.Type.Vector
-- import           Type.Class.Higher
-- import           Type.Class.Witness
-- import           Type.Family.List
-- import           Type.Family.Nat
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Kind
import           Data.Vinyl.Core
import           GHC.Generics
import           Lens.Micro

rzipWith3
    :: forall f g h j. ()
    => (forall x. f x -> g x -> h x -> j x)
    -> (forall xs. Rec f xs -> Rec g xs -> Rec h xs -> Rec j xs)
rzipWith3 f = go
  where
    go :: forall ys. Rec f ys -> Rec g ys -> Rec h ys -> Rec j ys
    go = \case
      RNil -> \case
        RNil -> \case
          RNil -> RNil
      x :& xs -> \case
        y :& ys -> \case
          z :& zs -> f x y z :& go xs ys zs

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

data N = Z | S N

data Vec :: N -> Type -> Type where
    VNil :: Vec 'Z a
    (:+) :: a -> Vec n a -> Vec ('S n) a

withVec
    :: [a]
    -> (forall n. Vec n a -> r)
    -> r
withVec = \case
    []   -> ($ VNil)
    x:xs -> \f -> withVec xs (f . (x :+))

type family Replicate (n :: N) (a :: k) = (as :: [k]) | as -> n where
    Replicate 'Z     a = '[]
    Replicate ('S n) a = a ': Replicate n a

vecToRec
    :: Vec n a
    -> Rec Identity (Replicate n a)
vecToRec = \case
    VNil    -> RNil
    x :+ xs -> Identity x :& vecToRec xs

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

rtraverse_
    :: forall f g. Applicative g
    => (forall x. f x -> g ())
    -> (forall xs. Rec f xs -> g ())
rtraverse_ f = go
  where
    go :: Rec f ys -> g ()
    go = \case
      RNil    -> pure ()
      x :& xs -> f x *> go xs

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


-- vecLen
--     :: VecT n f a
--     -> Nat n
-- vecLen = \case
--     ØV      -> Z_
--     _ :* xs -> S_ (vecLen xs)

-- zipWithPM_
--     :: forall h f g as. Applicative h
--     => (forall a. f a -> g a -> h ())
--     -> Prod f as
--     -> Prod g as
--     -> h ()
-- zipWithPM_ f = go
--   where
--     go :: forall bs. Prod f bs -> Prod g bs -> h ()
--     go = \case
--       Ø -> \case
--         Ø -> pure ()
--       x :< xs -> \case
--         y :< ys -> f x y *> go xs ys

-- zipWithPM3_
--     :: forall m f g h as. Applicative m
--     => (forall a. f a -> g a -> h a -> m ())
--     -> Prod f as
--     -> Prod g as
--     -> Prod h as
--     -> m ()
-- zipWithPM3_ f = go
--   where
--     go :: forall bs. Prod f bs -> Prod g bs -> Prod h bs -> m ()
--     go = \case
--       Ø -> \case
--         Ø -> \case
--           Ø -> pure ()
--       x :< xs -> \case
--         y :< ys -> \case
--           z :< zs -> f x y z *> go xs ys zs

-- zipP
--     :: Prod f as
--     -> Prod g as
--     -> Prod (f :&: g) as
-- zipP = \case
--     Ø -> \case
--       Ø       -> Ø
--     x :< xs -> \case
--       y :< ys -> x :&: y :< zipP xs ys
-- {-# INLINE zipP #-}

-- unzipP
--     :: Prod (f :&: g) as
--     -> (Prod f as, Prod g as)
-- unzipP = \case
--     Ø               -> (Ø, Ø)
--     (x :&: y) :< zs -> bimap (x :<) (y :<) (unzipP zs)

-- lengthProd
--     :: (forall a. f a)
--     -> Length as
--     -> Prod f as
-- lengthProd x = \case
--     LZ   -> Ø
--     LS l -> x :< lengthProd x l

-- listToVecDef
--     :: forall f a n. ()
--     => f a
--     -> Nat n
--     -> [f a]
--     -> VecT n f a
-- listToVecDef d = go
--   where
--     go :: Nat m -> [f a] -> VecT m f a
--     go = \case
--       Z_   -> const ØV
--       S_ n -> \case
--         []   -> d :* vrep d \\ n
--         x:xs -> x :* go n xs

-- zipVecList
--     :: forall a b c f g n. ()
--     => (f a -> Maybe b -> g c)
--     -> VecT n f a
--     -> [b]
--     -> VecT n g c
-- zipVecList f = go
--   where
--     go :: VecT m f a -> [b] -> VecT m g c
--     go = \case
--       ØV -> const ØV
--       x :* xs -> \case
--         []   -> f x Nothing  :* go xs []
--         y:ys -> f x (Just y) :* go xs ys

-- traverse1_
--     :: (Foldable1 t, Applicative g)
--     => (forall a. f a -> g ())
--     -> t f as
--     -> g ()
-- traverse1_ f = sequenceA_ . foldMap1 ((:[]) . f)

-- splitProd
--     :: Length as
--     -> Prod f (as ++ bs)
--     -> (Prod f as, Prod f bs)
-- splitProd = \case
--     LZ   -> (Ø,)
--     LS l -> \case
--       x :< xs -> first (x :<) $ splitProd l xs
-- {-# INLINE splitProd #-}

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
