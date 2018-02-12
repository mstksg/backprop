{-# LANGUAGE FlexibleContexts #-}

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
-- Intended to work with 'Functor' / 'Foldable' / 'Traversable' instances
-- with "fixed" number of items, i.e.
-- <https://hackage.haskell.org/package/vector-sized vector-sized> vectors.
-- There might be unintended consequences when using it with instances
-- where the number of items is not fixed.
--
-- This module is intended to be a catch-all one, so feel free to suggest
-- other functions or submit a PR if you think one would make sense.
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
  -- * Functor and Applicative
  , fmap
  , (<$>)
  , pure
  , liftA2
  , liftA3
  -- * Misc
  , coerce
  ) where

import           Numeric.Backprop
import           Prelude             (Num(..), Fractional(..), Eq(..), Ord(..), Functor, Foldable, Traversable, Applicative, (.), ($))
import qualified Control.Applicative as P
import qualified Data.Coerce         as C
import qualified Prelude             as P

-- | Lifted 'P.sum'
sum :: (Foldable t, Applicative t, Num (t a), Num a, Reifies s W)
    => BVar s (t a)
    -> BVar s a
sum = liftOp1 . op1 $ \xs ->
    ( P.sum xs
    , \d -> P.const d P.<$> xs
    )

-- | Lifted 'P.pure'.  Really intended only for 'Applicative' instances
-- with fixed number of items; untintended consequences might arise when
-- using it with containers with variable number of items.
pure
    :: (Foldable t, Applicative t, Num (t a), Num a, Reifies s W)
    => BVar s a
    -> BVar s (t a)
pure = liftOp1 . op1 $ \x ->
    ( P.pure x
    , P.sum
    )

-- | Lifted 'P.product'
product
    :: (Foldable t, Applicative t, Num (t a), Fractional a, Reifies s W)
    => BVar s (t a)
    -> BVar s a
product = liftOp1 . op1 $ \xs ->
    let p = P.product xs
    in ( p
       , \d -> (\x -> p * d / x) P.<$> xs
       )

-- | Lifted 'P.length'.  Really intended only for 'Foldable' instances
-- with fixed number of items; untintended consequences might arise when
-- using it with containers with variable number of items.
length
    :: (Foldable t, Num (t a), Num b, Reifies s W)
    => BVar s (t a)
    -> BVar s b
length = liftOp1 . op1 $ \xs ->
    ( P.fromIntegral (P.length xs)
    , P.const 0
    )

-- | Lifted 'P.minimum'.  Undefined for situations where 'P.minimum' would
-- be undefined.
minimum
    :: (Foldable t, Functor t, Num a, Ord a, Num (t a), Reifies s W)
    => BVar s (t a)
    -> BVar s a
minimum = liftOp1 . op1 $ \xs ->
    let m = P.minimum xs
    in  ( m
        , \d -> (\x -> if x == m then d else 0) P.<$> xs
        )

-- | Lifted 'P.maximum'.  Undefined for situations where 'P.maximum' would
-- be undefined.
maximum
    :: (Foldable t, Functor t, Num a, Ord a, Num (t a), Reifies s W)
    => BVar s (t a)
    -> BVar s a
maximum = liftOp1 . op1 $ \xs ->
    let m = P.maximum xs
    in  ( m
        , \d -> (\x -> if x == m then d else 0) P.<$> xs
        )

-- | Lifted 'P.fmap'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Functor's.
--
-- Really intended only for 'Functor' instances with fixed number of items;
-- untintended consequences might arise when using it with containers with
-- variable number of items.
fmap
    :: (Traversable f, Num a, Num b, Num (f b), Reifies s W)
    => (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
fmap f = collectVar . P.fmap f . sequenceVar

-- | Alias for 'fmap'.
(<$>)
    :: (Traversable f, Num a, Num b, Num (f b), Reifies s W)
    => (BVar s a -> BVar s b)
    -> BVar s (f a)
    -> BVar s (f b)
(<$>) = fmap

-- | Lifted 'P.traverse'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Functor's.
--
-- Really intended only for 'Traversable' and 'Applicative' instances with
-- fixed number of items; untintended consequences might arise when using
-- it with containers with variable number of items.
traverse
    :: (Traversable t, Applicative f, Foldable f, Num a, Num b, Num (f (t b)), Num (t b), Reifies s W)
    => (BVar s a -> f (BVar s b))
    -> BVar s (t a)
    -> BVar s (f (t b))
traverse f = collectVar
           . P.fmap collectVar
           . P.traverse f
           . sequenceVar

-- | Lifted 'P.liftA2'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Applicative's.
--
-- Really intended only for 'Traversable' and 'Applicative' instances with
-- fixed number of items; untintended consequences might arise when using
-- it with containers with variable number of items.
liftA2
    :: ( Traversable f
       , Applicative f
       , Num a, Num b, Num c, Num (f c)
       , Reifies s W
       )
    => (BVar s a -> BVar s b -> BVar s c)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
liftA2 f x y = collectVar $ f P.<$> sequenceVar x
                              P.<*> sequenceVar y

-- | Lifted 'P.liftA3'.  Lifts backpropagatable functions to be
-- backpropagatable functions on 'Traversable' 'Applicative's.
--
-- Really intended only for 'Traversable' and 'Applicative' instances with
-- fixed number of items; untintended consequences might arise when using
-- it with containers with variable number of items.
liftA3
    :: ( Traversable f
       , Applicative f
       , Num a, Num b, Num c, Num d, Num (f d)
       , Reifies s W
       )
    => (BVar s a -> BVar s b -> BVar s c -> BVar s d)
    -> BVar s (f a)
    -> BVar s (f b)
    -> BVar s (f c)
    -> BVar s (f d)
liftA3 f x y z = collectVar $ f P.<$> sequenceVar x
                                P.<*> sequenceVar y
                                P.<*> sequenceVar z

-- | Coerce items inside a 'BVar'.
coerce
    :: (C.Coercible a b, Num a, Num b, Reifies s W)
    => BVar s a
    -> BVar s b
coerce = liftOp1 $ opIso C.coerce C.coerce
