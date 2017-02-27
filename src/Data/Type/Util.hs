{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Type.Util where

import           Control.Applicative
import           Data.Monoid
import           Data.Type.Conjunction
import           Data.Type.Fin
import           Data.Type.Index
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Vector
import           Lens.Micro
import           Type.Class.Higher
import           Type.Family.Nat

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

itraverse1_
    :: (Applicative h, IxTraversable1 i t)
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

zipP
    :: Prod f as
    -> Prod g as
    -> Prod (f :&: g) as
zipP = \case
    Ø -> \case
      Ø       -> Ø
    x :< xs -> \case
      y :< ys -> x :&: y :< zipP xs ys

indexP :: Index as a -> Lens' (Prod g as) (g a)
indexP = \case
    IZ   -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> indexP i f xs
