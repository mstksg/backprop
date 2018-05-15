{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_HADDOCK not-home     #-}

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
  , mapAccumL
  , mapAccumR
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

import           Data.Bifunctor
import           Numeric.Backprop.Explicit
import           Prelude                   (Num(..), Fractional(..), Eq(..), Ord(..), Functor, Foldable, Traversable, Applicative, (.), ($))
import qualified Control.Applicative       as P
import qualified Data.Coerce               as C
import qualified Data.Foldable             as P
import qualified Data.Traversable          as P
import qualified Prelude                   as P

-- | 'Prelude.Backprop.sum', but taking explicit 'add' and 'zero'.
sum :: (Foldable t, Functor t, Num a, Reifies s W)
    => AddFunc (t a)
    -> ZeroFunc a
    -> BVar s (t a)
    -> BVar s a
sum af zf = liftOp1 af zf . op1 $ \xs ->
    ( P.sum xs
    , (P.<$ xs)
    )
{-# INLINE sum #-}

-- | 'Prelude.Backprop.pure', but taking explicit 'add' and 'zero'.
pure
    :: (Foldable t, Applicative t, Reifies s W)
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

-- | 'Prelude.Backprop.product', but taking explicit 'add' and 'zero'.
product
    :: (Foldable t, Functor t, Fractional a, Reifies s W)
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

-- | 'Prelude.Backprop.length', but taking explicit 'add' and 'zero'.
length
    :: (Foldable t, Num b, Reifies s W)
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

-- | 'Prelude.Backprop.minimum', but taking explicit 'add' and 'zero'.
minimum
    :: (Foldable t, Functor t, Ord a, Reifies s W)
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

-- | 'Prelude.Backprop.maximum', but taking explicit 'add' and 'zero'.
maximum
    :: (Foldable t, Functor t, Ord a, Reifies s W)
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

-- | 'Prelude.Backprop.fmap', but taking explicit 'add' and 'zero'.
--
-- See documentation for 'Numeric.Backprop.Explicitl.collectVar' for
-- information the API change in v0.2.3 that removed the @'ZeroFunc' (f b)@
-- parameter.
fmap
    :: (Traversable f, Reifies s W)
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc a
    -> ZeroFunc b
    -> (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
fmap afa afb zfa zfb f = collectVar afb zfb . P.fmap f . sequenceVar afa zfa
{-# INLINE fmap #-}

-- | 'Prelude.Backprop.traverse', but taking explicit 'add' and 'zero'.
--
-- See documentation for 'Numeric.Backprop.Explicitl.collectVar' for
-- information the API change in v0.2.3 that removed the @'ZeroFunc' (t b)@
-- and @'ZeroFunc' (f (t b))@ parameters.
traverse
    :: (Traversable t, Applicative f, Foldable f, Reifies s W)
    => AddFunc a
    -> AddFunc b
    -> AddFunc (t b)
    -> ZeroFunc a
    -> ZeroFunc b
    -> (BVar s a -> f (BVar s b))
    -> BVar s (t a)
    -> BVar s (f (t b))
traverse afa afb aftb zfa zfb f
        = collectVar aftb zftb
        . P.fmap (collectVar afb zfb)
        . P.traverse f
        . sequenceVar afa zfa
  where
    zftb = ZF $ P.fmap (runZF zfb)
    {-# INLINE zftb #-}
{-# INLINE traverse #-}

-- | 'Prelude.Backprop.liftA2', but taking explicit 'add' and 'zero'.
--
-- See documentation for 'Numeric.Backprop.Explicitl.collectVar' for
-- information the API change in v0.2.3 that removed the @'ZeroFunc' (f c)@
-- parameter.
liftA2
    :: ( Traversable f
       , Applicative f
       , Reifies s W
       )
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> ZeroFunc a
    -> ZeroFunc b
    -> ZeroFunc c
    -> (BVar s a -> BVar s b -> BVar s c)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
liftA2 afa afb afc zfa zfb zfc f x y
    = collectVar afc zfc
    $ f P.<$> sequenceVar afa zfa x
        P.<*> sequenceVar afb zfb y
{-# INLINE liftA2 #-}

-- | 'Prelude.Backprop.liftA3', but taking explicit 'add' and 'zero'.
--
-- See documentation for 'Numeric.Backprop.Explicitl.collectVar' for
-- information the API change in v0.2.3 that removed the @'ZeroFunc' (f d)@
-- parameter.
liftA3
    :: ( Traversable f
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
    -> (BVar s a -> BVar s b -> BVar s c -> BVar s d)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
    -> BVar s (f d)
liftA3 afa afb afc afd zfa zfb zfc zfd f x y z
    = collectVar afd zfd
    $ f P.<$> sequenceVar afa zfa x
        P.<*> sequenceVar afb zfb y
        P.<*> sequenceVar afc zfc z
{-# INLINE liftA3 #-}

-- | Coerce items inside a 'BVar'.
coerce :: C.Coercible a b => BVar s a -> BVar s b
coerce = coerceVar
{-# INLINE coerce #-}

-- | 'Prelude.Backprop.fromIntegral', but taking explicit 'add' and 'zero'.
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

-- | 'Prelude.Backprop.realToFrac', but taking explicit 'add' and 'zero'.
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

-- | 'Prelude.Backprop.length', but taking explicit 'add' and 'zero'.
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

-- | 'Prelude.Backprop.mapAccumL', but taking explicit 'add' and 'zero'.
--
-- See documentation for 'Numeric.Backprop.Explicitl.collectVar' for
-- information the API change in v0.2.3 that removed the @'ZeroFunc' (t c)@
-- parameter.
--
-- @since 0.2.2.0
mapAccumL
    :: (Traversable t, Reifies s W)
    => AddFunc b
    -> AddFunc c
    -> ZeroFunc b
    -> ZeroFunc c
    -> (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumL afb afc zfb zfc f s =
        second (collectVar afc zfc)
      . P.mapAccumL f s
      . sequenceVar afb zfb
{-# INLINE mapAccumL #-}

-- | 'Prelude.Backprop.mapAccumR', but taking explicit 'add' and 'zero'.
--
-- See documentation for 'Numeric.Backprop.Explicitl.collectVar' for
-- information the API change in v0.2.3 that removed the @'ZeroFunc' (t c)@
-- parameter.
--
-- @since 0.2.2.0
mapAccumR
    :: (Traversable t, Reifies s W)
    => AddFunc b
    -> AddFunc c
    -> ZeroFunc b
    -> ZeroFunc c
    -> (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumR afb afc zfb zfc f s =
        second (collectVar afc zfc)
      . P.mapAccumR f s
      . sequenceVar afb zfb
{-# INLINE mapAccumR #-}
