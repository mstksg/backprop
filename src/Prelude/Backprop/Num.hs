{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_HADDOCK not-home     #-}

-- |
-- Module      : Prelude.Backprop.Num
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the exact same API as "Prelude.Backprop", except requiring
-- 'Num' instances for all types involved instead of 'Backprop' instances.
--
-- @since 0.2.0.0

module Prelude.Backprop.Num (
  -- * Foldable and Traversable
    sum
  , product
  , length
  , minimum
  , maximum
  , traverse
  , toList
  , mapAccumL
  , mapAccumR
  , foldr, foldl'
  -- * Functor and Applicative
  , fmap
  , (<$>)
  , pure
  , liftA2
  , liftA3
  -- * Misc
  , fromIntegral
  , realToFrac
  , E.coerce
  ) where

import           Numeric.Backprop.Num
import           Prelude                   (Num(..), Fractional(..), Ord(..), Functor, Foldable, Traversable, Applicative)
import qualified Numeric.Backprop.Explicit as E
import qualified Prelude                   as P
import qualified Prelude.Backprop.Explicit as E

-- | 'Prelude.Backprop.sum', but with 'Num' constraints instead of
-- 'Backprop' constraints.
sum :: (Foldable t, Functor t, Num (t a), Num a, Reifies s W)
    => BVar s (t a)
    -> BVar s a
sum = E.sum E.afNum E.zfNum
{-# INLINE sum #-}

-- | 'Prelude.Backprop.pure', but with 'Num' constraints instead of
-- 'Backprop' constraints.
pure
    :: (Foldable t, Applicative t, Num (t a), Num a, Reifies s W)
    => BVar s a
    -> BVar s (t a)
pure = E.pure E.afNum E.zfNum E.zfNum
{-# INLINE pure #-}

-- | 'Prelude.Backprop.product', but with 'Num' constraints instead of
-- 'Backprop' constraints.
product
    :: (Foldable t, Functor t, Num (t a), Fractional a, Reifies s W)
    => BVar s (t a)
    -> BVar s a
product = E.product E.afNum E.zfNum
{-# INLINE product #-}

-- | 'Prelude.Backprop.length', but with 'Num' constraints instead of
-- 'Backprop' constraints.
length
    :: (Foldable t, Num (t a), Num b, Reifies s W)
    => BVar s (t a)
    -> BVar s b
length = E.length E.afNum E.zfNum E.zfNum
{-# INLINE length #-}

-- | 'Prelude.Backprop.minimum', but with 'Num' constraints instead of
-- 'Backprop' constraints.
minimum
    :: (Foldable t, Functor t, Num a, Ord a, Num (t a), Reifies s W)
    => BVar s (t a)
    -> BVar s a
minimum = E.minimum E.afNum E.zfNum
{-# INLINE minimum #-}

-- | 'Prelude.Backprop.maximum', but with 'Num' constraints instead of
-- 'Backprop' constraints.
maximum
    :: (Foldable t, Functor t, Num a, Ord a, Num (t a), Reifies s W)
    => BVar s (t a)
    -> BVar s a
maximum = E.maximum E.afNum E.zfNum
{-# INLINE maximum #-}

-- | 'Prelude.Backprop.foldr', but with 'Num' constraints instead of
-- 'Backprop' constraints.
foldr
    :: (Traversable t, Num a, Reifies s W)
    => (BVar s a -> BVar s b -> BVar s b)
    -> BVar s b
    -> BVar s (t a)
    -> BVar s b
foldr = E.foldr E.afNum E.zfNum
{-# INLINE foldr #-}

-- | 'Prelude.Backprop.foldl'', but with 'Num' constraints instead of
-- 'Backprop' constraints.
foldl'
    :: (Traversable t, Num a, Reifies s W)
    => (BVar s b -> BVar s a -> BVar s b)
    -> BVar s b
    -> BVar s (t a)
    -> BVar s b
foldl' = E.foldl' E.afNum E.zfNum
{-# INLINE foldl' #-}

-- | 'Prelude.Backprop.fmap', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Prior to v0.2.3, required a 'Num' constraint on @f b@.
fmap
    :: (Traversable f, Num a, Num b, Reifies s W)
    => (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
fmap = E.fmap E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE fmap #-}

-- | Alias for 'fmap'.
(<$>)
    :: (Traversable f, Num a, Num b, Reifies s W)
    => (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
(<$>) = fmap
{-# INLINE (<$>) #-}

-- | 'Prelude.Backprop.traverse', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- See <https://hackage.haskell.org/package/vector-sized vector-sized> for
-- a fixed-length vector type with a very appropriate 'Num' instance!
--
-- Prior to v0.2.3, required a 'Num' constraint on @f (t b)@.
traverse
    :: (Traversable t, Applicative f, Foldable f, Num a, Num b, Num (t b), Reifies s W)
    => (BVar s a -> f (BVar s b))
    -> BVar s (t a)
    -> BVar s (f (t b))
traverse = E.traverse E.afNum E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE traverse #-}

-- | 'Prelude.Backprop.liftA2', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Prior to v0.2.3, required a 'Num' constraint on @f c@.
liftA2
    :: ( Traversable f
       , Applicative f
       , Num a, Num b, Num c
       , Reifies s W
       )
    => (BVar s a -> BVar s b -> BVar s c)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
liftA2 = E.liftA2 E.afNum E.afNum E.afNum E.zfNum E.zfNum E.zfNum
{-# INLINE liftA2 #-}

-- | 'Prelude.Backprop.liftA3', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Prior to v0.2.3, required a 'Num' constraint on @f d@.
liftA3
    :: ( Traversable f
       , Applicative f
       , Num a, Num b, Num c, Num d
       , Reifies s W
       )
    => (BVar s a -> BVar s b -> BVar s c -> BVar s d)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
    -> BVar s (f d)
liftA3 = E.liftA3 E.afNum E.afNum E.afNum E.afNum
                  E.zfNum E.zfNum E.zfNum E.zfNum
{-# INLINE liftA3 #-}

-- | 'Prelude.Backprop.fromIntegral', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- @since 0.2.1.0
fromIntegral
    :: (P.Integral a, P.Integral b, Reifies s W)
    => BVar s a
    -> BVar s b
fromIntegral = E.fromIntegral E.afNum E.zfNum
{-# INLINE fromIntegral #-}

-- | 'Prelude.Backprop.realToFrac', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- @since 0.2.1.0
realToFrac
    :: (Fractional a, P.Real a, Fractional b, P.Real b, Reifies s W)
    => BVar s a
    -> BVar s b
realToFrac = E.realToFrac E.afNum E.zfNum
{-# INLINE realToFrac #-}

-- | 'Prelude.Backprop.toList', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- @since 0.2.2.0
toList
    :: (Traversable t, Num a, Reifies s W)
    => BVar s (t a)
    -> [BVar s a]
toList = E.toList E.afNum E.zfNum
{-# INLINE toList #-}

-- | 'Prelude.Backprop.mapAccumL', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Prior to v0.2.3, required a 'Num' constraint on @t b@.
--
-- @since 0.2.2.0
mapAccumL
    :: (Traversable t, Num b, Num c, Reifies s W)
    => (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumL = E.mapAccumL E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE mapAccumL #-}

-- | 'Prelude.Backprop.mapAccumR', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Prior to v0.2.3, required a 'Num' constraint on @t b@.
--
-- @since 0.2.2.0
mapAccumR
    :: (Traversable t, Num b, Num c, Reifies s W)
    => (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumR = E.mapAccumR E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE mapAccumR #-}

