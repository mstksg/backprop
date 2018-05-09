{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Prelude.Backprop.Explicit
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides "explicit" versions of all of the functions in
-- "Prelude.Backprop".  Instead of relying on a 'Backprop' instance, allows
-- you to manually provide 'zero', 'add', and 'one' on a per-value basis.
--
-- @since 0.2.0.0

module Prelude.Backprop.Explicit (
  -- * Foldable and Traversable
    sum
  , product
  , length
  , minimum
  , maximum
  , traverse
  , toList
  -- * Functor and Applicative
  , fmap
  , pure
  , liftA2
  , liftA3
  -- * Misc
  , fromIntegral
  , realToFrac
  , coerce
  ) where

import           Numeric.Backprop.Explicit
import           Prelude             (Num(..), Fractional(..), Eq(..), Ord(..), Functor, Foldable, Traversable, Applicative, (.), ($))
import qualified Control.Applicative as P
import qualified Data.Coerce         as C
import qualified Data.Foldable       as P
import qualified Prelude             as P

-- | Lifted 'P.sum'
sum :: forall t a s. (Foldable t, Functor t, Num a, Reifies s W)
    => AddFunc (t a)
    -> ZeroFunc a
    -> BVar s (t a)
    -> BVar s a
sum af zf = liftOp1 af zf . op1 $ \xs ->
    ( P.sum xs
    , (P.<$ xs)
    )
{-# INLINE sum #-}

-- | Lifted 'P.pure'.
pure
    :: forall t a s. (Foldable t, Applicative t, Reifies s W)
    => AddFunc a
    -> ZeroFunc a
    -> ZeroFunc (t a)
    -> BVar s a
    -> BVar s (t a)
pure af zfa zf = liftOp1 af zf . op1 $ \x ->
    ( P.pure x
    , P.foldl' (runAF af) (runZF zfa x)
    )
{-# INLINE pure #-}

-- | Lifted 'P.product'
product
    :: forall t a s. (Foldable t, Functor t, Fractional a, Reifies s W)
    => AddFunc (t a)
    -> ZeroFunc a
    -> BVar s (t a)
    -> BVar s a
product af zf = liftOp1 af zf . op1 $ \xs ->
    let p = P.product xs
    in ( p
       , \d -> (\x -> p * d / x) P.<$> xs
       )
{-# INLINE product #-}

-- | Lifted 'P.length'.
length
    :: forall t a b s. (Foldable t, Num b, Reifies s W)
    => AddFunc (t a)
    -> ZeroFunc (t a)
    -> ZeroFunc b
    -> BVar s (t a)
    -> BVar s b
length af zfa zf = liftOp1 af zf . op1 $ \xs ->
    ( P.fromIntegral (P.length xs)
    , P.const (runZF zfa xs)
    )
{-# INLINE length #-}

-- | Lifted 'P.minimum'.  Undefined for situations where 'P.minimum' would
-- be undefined.
minimum
    :: forall t a s. (Foldable t, Functor t, Ord a, Reifies s W)
    => AddFunc (t a)
    -> ZeroFunc a
    -> BVar s (t a)
    -> BVar s a
minimum af zf = liftOp1 af zf . op1 $ \xs ->
    let m = P.minimum xs
    in  ( m
        , \d -> (\x -> if x == m then d else runZF zf x) P.<$> xs
        )
{-# INLINE minimum #-}

-- | Lifted 'P.maximum'.  Undefined for situations where 'P.maximum' would
-- be undefined.
maximum
    :: forall t a s. (Foldable t, Functor t, Ord a, Reifies s W)
    => AddFunc (t a)
    -> ZeroFunc a
    -> BVar s (t a)
    -> BVar s a
maximum af zf = liftOp1 af zf . op1 $ \xs ->
    let m = P.maximum xs
    in  ( m
        , \d -> (\x -> if x == m then d else runZF zf x) P.<$> xs
        )
{-# INLINE maximum #-}

-- | Lifted 'P.fmap'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Functor's.
fmap
    :: forall f a b s. (Traversable f, Reifies s W)
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc a
    -> ZeroFunc b
    -> ZeroFunc (f b)
    -> (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
fmap afa afb zfa zfb zfbs f = collectVar afb zfb zfbs . P.fmap f . sequenceVar afa zfa
{-# INLINE fmap #-}

-- | Lifted 'P.traverse'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Functor's.
traverse
    :: forall t f a b s. (Traversable t, Applicative f, Foldable f, Reifies s W)
    => AddFunc a
    -> AddFunc b
    -> AddFunc (t b)
    -> ZeroFunc a
    -> ZeroFunc b
    -> ZeroFunc (t b)
    -> ZeroFunc (f (t b))
    -> (BVar s a -> f (BVar s b))
    -> BVar s (t a)
    -> BVar s (f (t b))
traverse afa afb aftb zfa zfb zftb zfftb f
        = collectVar aftb zftb zfftb
        . P.fmap (collectVar afb zfb zftb)
        . P.traverse f
        . sequenceVar afa zfa
{-# INLINE traverse #-}

-- | Lifted 'P.liftA2'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Applicative's.
liftA2
    :: forall f a b c s.
       ( Traversable f
       , Applicative f
       , Reifies s W
       )
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> ZeroFunc a
    -> ZeroFunc b
    -> ZeroFunc c
    -> ZeroFunc (f c)
    -> (BVar s a -> BVar s b -> BVar s c)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
liftA2 afa afb afc zfa zfb zfc zffc f x y
    = collectVar afc zfc zffc
    $ f P.<$> sequenceVar afa zfa x
        P.<*> sequenceVar afb zfb y
{-# INLINE liftA2 #-}

-- | Lifted 'P.liftA3'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Applicative's.
liftA3
    :: forall f a b c d s.
       ( Traversable f
       , Applicative f
       , Reifies s W
       )
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> AddFunc d
    -> ZeroFunc a
    -> ZeroFunc b
    -> ZeroFunc c
    -> ZeroFunc d
    -> ZeroFunc (f d)
    -> (BVar s a -> BVar s b -> BVar s c -> BVar s d)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
    -> BVar s (f d)
liftA3 afa afb afc afd zfa zfb zfc zfd zffd f x y z
    = collectVar afd zfd zffd
    $ f P.<$> sequenceVar afa zfa x
        P.<*> sequenceVar afb zfb y
        P.<*> sequenceVar afc zfc z
{-# INLINE liftA3 #-}

-- | Coerce items inside a 'BVar'.
coerce
    :: forall a b s. C.Coercible a b
    => BVar s a
    -> BVar s b
coerce = coerceVar
{-# INLINE coerce #-}

-- | Lifted conversion between two 'P.Integral' instances.
--
-- @since 0.2.1.0
fromIntegral
    :: (P.Integral a, P.Integral b, Reifies s W)
    => AddFunc a
    -> ZeroFunc b
    -> BVar s a
    -> BVar s b
fromIntegral af zf = liftOp1 af zf . op1 $ \x ->
    (P.fromIntegral x, P.fromIntegral)
{-# INLINE fromIntegral #-}

-- | Lifted conversion between two 'Fractional' and 'P.Real' instances.
--
-- @since 0.2.1.0
realToFrac
    :: (Fractional a, P.Real a, Fractional b, P.Real b, Reifies s W)
    => AddFunc a
    -> ZeroFunc b
    -> BVar s a
    -> BVar s b
realToFrac af zf = liftOp1 af zf . op1 $ \x ->
    (P.realToFrac x, P.realToFrac)
{-# INLINE realToFrac #-}

-- | Lifted version of 'P.toList'.  Takes a 'BVar' of a 'Traversable' of
-- items and returns a list of 'BVar's for each item.
--
-- @since 0.2.2.0
toList
    :: (Traversable t, Reifies s W)
    => AddFunc a
    -> ZeroFunc a
    -> BVar s (t a)
    -> [BVar s a]
toList af zf = toListOfVar af zf P.traverse
{-# INLINE toList #-}
