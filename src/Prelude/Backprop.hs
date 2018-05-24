{-# LANGUAGE FlexibleContexts    #-}

-- |
-- Module      : Prelude.Backprop
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Some lifted versions of common functions found in 'Prelude' (or /base/
-- in general).
--
-- This module is intended to be a catch-all one, so feel free to suggest
-- other functions or submit a PR if you think one would make sense.
--
-- See "Prelude.Backprop.Num" for a version with 'Num' constraints instead
-- of 'Backprop' constraints, and "Prelude.Backprop.Explicit" for a version
-- allowing you to provide 'zero', 'add', and 'one' explicitly.
--
-- @since 0.1.3.0
--

module Prelude.Backprop (
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

import           Numeric.Backprop
import           Prelude                   (Num(..), Fractional(..), Ord(..), Functor, Foldable, Traversable, Applicative)
import qualified Numeric.Backprop.Explicit as E
import qualified Prelude                   as P
import qualified Prelude.Backprop.Explicit as E

-- | Lifted 'P.sum'.  More efficient than going through 'toList'.
sum :: (Foldable t, Functor t, Backprop (t a), Backprop a, Num a, Reifies s W)
    => BVar s (t a)
    -> BVar s a
sum = E.sum E.addFunc E.zeroFunc
{-# INLINE sum #-}

-- | Lifted 'P.pure'.
pure
    :: (Foldable t, Applicative t, Backprop (t a), Backprop a, Reifies s W)
    => BVar s a
    -> BVar s (t a)
pure = E.pure E.addFunc E.zeroFunc E.zeroFunc
{-# INLINE pure #-}

-- | Lifted 'P.product'.  More efficient than going through 'toList'.
product
    :: (Foldable t, Functor t, Backprop (t a), Backprop a, Fractional a, Reifies s W)
    => BVar s (t a)
    -> BVar s a
product = E.product E.addFunc E.zeroFunc
{-# INLINE product #-}

-- | Lifted 'P.length'.  More efficient than going through 'toList'.
length
    :: (Foldable t, Backprop (t a), Backprop b, Num b, Reifies s W)
    => BVar s (t a)
    -> BVar s b
length = E.length E.addFunc E.zeroFunc E.zeroFunc
{-# INLINE length #-}

-- | Lifted 'P.minimum'.  Undefined for situations where 'P.minimum' would
-- be undefined.  More efficient than going through 'toList'.
minimum
    :: (Foldable t, Functor t, Backprop a, Ord a, Backprop (t a), Reifies s W)
    => BVar s (t a)
    -> BVar s a
minimum = E.minimum E.addFunc E.zeroFunc
{-# INLINE minimum #-}

-- | Lifted 'P.maximum'.  Undefined for situations where 'P.maximum' would
-- be undefined.  More efficient than going through 'toList'.
maximum
    :: (Foldable t, Functor t, Backprop a, Ord a, Backprop (t a), Reifies s W)
    => BVar s (t a)
    -> BVar s a
maximum = E.maximum E.addFunc E.zeroFunc
{-# INLINE maximum #-}

-- | Lifed 'P.foldr'.  Essentially just 'toList' composed with a normal
-- list 'P.foldr', and is only here for convenience.
--
-- @since 0.2.3.0
foldr
    :: (Traversable t, Backprop a, Reifies s W)
    => (BVar s a -> BVar s b -> BVar s b)
    -> BVar s b
    -> BVar s (t a)
    -> BVar s b
foldr = E.foldr E.addFunc E.zeroFunc
{-# INLINE foldr #-}

-- | Lifed 'P.foldl''.  Essentially just 'toList' composed with a normal
-- list 'P.foldl'', and is only here for convenience.
--
-- @since 0.2.3.0
foldl'
    :: (Traversable t, Backprop a, Reifies s W)
    => (BVar s b -> BVar s a -> BVar s b)
    -> BVar s b
    -> BVar s (t a)
    -> BVar s b
foldl' = E.foldl' E.addFunc E.zeroFunc
{-# INLINE foldl' #-}

-- | Lifted 'P.fmap'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Functor's.
--
-- Prior to v0.2.3, required a 'Backprop' constraint on @f b@.
fmap
    :: (Traversable f, Backprop a, Backprop b, Reifies s W)
    => (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
fmap = E.fmap E.addFunc E.addFunc E.zeroFunc E.zeroFunc
{-# INLINE fmap #-}

-- | Alias for 'fmap'.
(<$>)
    :: (Traversable f, Backprop a, Backprop b, Reifies s W)
    => (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
(<$>) = fmap
{-# INLINE (<$>) #-}

-- | Lifted 'P.traverse'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Functor's.
--
-- Prior to v0.2.3, required a 'Backprop' constraint on @f (t b)@.
traverse
    :: (Traversable t, Applicative f, Foldable f, Backprop a, Backprop b, Backprop (t b), Reifies s W)
    => (BVar s a -> f (BVar s b))
    -> BVar s (t a)
    -> BVar s (f (t b))
traverse = E.traverse E.addFunc E.addFunc E.addFunc
                      E.zeroFunc E.zeroFunc
{-# INLINE traverse #-}

-- | Lifted 'P.liftA2'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Applicative's.
--
-- Prior to v0.2.3, required a 'Backprop' constraint on @f c@.
liftA2
    :: ( Traversable f, Applicative f
       , Backprop a, Backprop b, Backprop c
       , Reifies s W
       )
    => (BVar s a -> BVar s b -> BVar s c)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
liftA2 = E.liftA2 E.addFunc E.addFunc E.addFunc
                  E.zeroFunc E.zeroFunc E.zeroFunc
{-# INLINE liftA2 #-}

-- | Lifted 'P.liftA3'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Applicative's.
--
-- Prior to v0.2.3, required a 'Backprop' constraint on @f d@.
liftA3
    :: ( Traversable f
       , Applicative f
       , Backprop a, Backprop b, Backprop c, Backprop d
       , Reifies s W
       )
    => (BVar s a -> BVar s b -> BVar s c -> BVar s d)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
    -> BVar s (f d)
liftA3 = E.liftA3 E.addFunc E.addFunc E.addFunc E.addFunc
                  E.zeroFunc E.zeroFunc E.zeroFunc E.zeroFunc
{-# INLINE liftA3 #-}

-- | Lifted conversion between two 'P.Integral' instances.
--
-- @since 0.2.1.0
fromIntegral
    :: (Backprop a, P.Integral a, Backprop b, P.Integral b, Reifies s W)
    => BVar s a
    -> BVar s b
fromIntegral = E.fromIntegral E.addFunc E.zeroFunc
{-# INLINE fromIntegral #-}

-- | Lifted conversion between two 'Fractional' and 'P.Real' instances.
--
-- @since 0.2.1.0
realToFrac
    :: (Backprop a, Fractional a, P.Real a, Backprop b, Fractional b, P.Real b, Reifies s W)
    => BVar s a
    -> BVar s b
realToFrac = E.realToFrac E.addFunc E.zeroFunc
{-# INLINE realToFrac #-}

-- | Lifted version of 'P.toList'.  Takes a 'BVar' of a 'Traversable' of
-- items and returns a list of 'BVar's for each item.
--
-- You can use this to implement "lifted" versions of 'Foldable' methods
-- like 'P.foldr', 'P.foldl'', etc.; however, 'sum', 'product', 'length',
-- 'minimum', and 'maximum' have more efficient implementations than simply
-- @'P.minimum' . 'toList'.@
--
-- @since 0.2.2.0
toList
    :: (Traversable t, Backprop a, Reifies s W)
    => BVar s (t a)
    -> [BVar s a]
toList = E.toList E.addFunc E.zeroFunc
{-# INLINE toList #-}

-- | Lifted version of 'P.mapAccumL'.
--
-- Prior to v0.2.3, required a 'Backprop' constraint on @t b@.
--
-- @since 0.2.2.0
mapAccumL
    :: (Traversable t, Backprop b, Backprop c, Reifies s W)
    => (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumL = E.mapAccumL E.addFunc E.addFunc E.zeroFunc E.zeroFunc
{-# INLINE mapAccumL #-}

-- | Lifted version of 'P.mapAccumR'.
--
-- Prior to v0.2.3, required a 'Backprop' constraint on @t b@.
--
-- @since 0.2.2.0
mapAccumR
    :: (Traversable t, Backprop b, Backprop c, Reifies s W)
    => (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumR = E.mapAccumR E.addFunc E.addFunc E.zeroFunc E.zeroFunc
{-# INLINE mapAccumR #-}

