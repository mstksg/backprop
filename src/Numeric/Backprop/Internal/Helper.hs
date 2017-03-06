{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Numeric.Backprop.Internal.Helper
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides general helper types like 'Summer' and 'Unity' that both
-- "Numeric.Backprop.Op" and "Numeric.Backprop.Internal" use.

module Numeric.Backprop.Internal.Helper (
  -- * Summer
    Summer(..), summers, summers'
  -- * Unity
  , Unity(..), unities, unities'
  ) where

import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Type.Class.Known

-- | Instructions on how to "sum" a list of values of a given type.
-- Basically used as an explicit witness for a 'Num' instance.
--
-- For most types, the only meaningful value of type @'Summer' a@ is
-- @'Summer' 'sum'@.  However, using 'Summer' lets us use 'BP' with types
-- that are /not/ instances of 'Num'.  Any type can be used, as long as you
-- provide a way to "sum" it!
--
-- For most of the functions in this library, you can completely ignore
-- this, as they will be generated automatically.  You only need to work
-- with this directly if you want to use custom types that /aren't/
-- instances of 'Num' with this library.
--
-- If 'Num a' is satisfied, one can create the canonical 'Summer' using
-- @'known' :: 'Num' a => 'Summer' a@.
newtype Summer a = Summer { runSummer :: [a] -> a }

-- | A canonical "unity" (the multiplicative identity) for a given type.
-- Basically used as an explicit witness for a 'Num' instance.
--
-- For most types, the only meaningful value of type @'Unity' a@ is
-- @'Unity' 1'@.  However, using 'Unity' lets us use 'BP' with types
-- that are /not/ instances of 'Num'.  Any type can be used, as long as you
-- provide a way to get a multiplicative identity in it!
--
-- For most of the functions in this library, you can completely ignore
-- this, as they will be generated automatically.  You only need to work
-- with this directly if you want to use custom types that /aren't/
-- instances of 'Num' with this library.
--
-- If 'Num a' is satisfied, one can create the canonical 'Unity' using
-- @'known' :: 'Num' a => 'Unity' a@.
newtype Unity  a = Unity  { getUnity  :: a        }
    deriving (Functor, Show)

-- | If @a@ is an instance of 'Num', then the canonical @'Summer' a@ is
-- @'Summer' 'sum'@.
instance Num a => Known Summer a where
    type KnownC Summer a = Num a
    known = Summer sum

-- | If @a@ is an instance of 'Num', then the canonical @'Unity' a@ is
-- @'Unity' 1@.
instance Num a => Known Unity a where
    type KnownC Unity a = Num a
    known = Unity 1

-- | If all the types in @as@ are instances of 'Num', generate a @'Prod'
-- 'Summer' as@, or a tuple of 'Summer's for every type in @as@.
summers
    :: (Every Num as, Known Length as)
    => Prod Summer as
summers = summers' known

-- | Like 'summers', but requiring an explicit witness for the number of
-- types in the list @as@.
summers'
    :: Every Num as
    => Length as
    -> Prod Summer as
summers' l = withEvery' @Num l known

-- | If all the types in @as@ are instances of 'Num', generate a @'Prod'
-- 'Unity' as@, or a tuple of 'Unity's for every type in @as@.
unities
    :: (Every Num as, Known Length as)
    => Prod Unity as
unities = unities' known

-- | Like 'unities', but requiring an explicit witness for the number of
-- types in the list @as@.
unities'
    :: Every Num as
    => Length as
    -> Prod Unity as
unities' l = withEvery' @Num l known

