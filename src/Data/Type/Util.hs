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

module Data.Type.Util where

import           Control.Applicative
import           Data.Kind
import           Data.Monoid hiding (Sum)
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
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.List
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

ifor1
    :: (Applicative h, IxTraversable1 i t)
    => t f b
    -> (forall a. i b a -> f a -> h (g a))
    -> h (t g b)
ifor1 x f = itraverse1 f x

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

reIndex
    :: forall k (f :: k -> Type) (as :: [k]) (a :: k). ()
    => Index as a
    -> Index (f <$> as) (f a)
reIndex = undefined

prodLength
    :: Prod f as
    -> Length as
prodLength = \case
    Ø       -> LZ
    _ :< xs -> LS (prodLength xs)

withEvery
    :: forall c f as. (Known Length as, Every c as)
    => (forall a. c a => f a)
    -> Prod f as
withEvery = withEvery' @c known

withEvery'
    :: forall c f as. Every c as
    => Length as
    -> (forall a. c a => f a)
    -> Prod f as
withEvery' l x = map1 ((// x) . every @_ @c) (indices' l)

tagSum
    :: Prod f as
    -> Sum g as
    -> Sum (f :&: g) as
tagSum = \case
    Ø       -> \case
    x :< xs -> \case
      InL y  -> InL (x :&: y)
      InR ys -> InR (tagSum xs ys)
