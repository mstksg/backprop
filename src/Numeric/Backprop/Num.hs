{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_HADDOCK not-home  #-}

-- |
-- Module      : Numeric.Backprop.Num
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the exact same API as "Numeric.Backprop", except requiring
-- 'Num' instances for all types involved instead of 'Backprop' instances.
--
-- This was the original API of the library (for version 0.1).
--
-- 'Num' is strictly more powerful than 'Backprop', and is a stronger
-- constraint on types than is necessary for proper backpropagating.  In
-- particular, 'fromInteger' is a problem for many types, preventing useful
-- backpropagation for lists, variable-length vectors (like "Data.Vector")
-- and variable-size matrices from linear algebra libraries like /hmatrix/
-- and /accelerate/.
--
-- However, this module might be useful in situations where you are working
-- with external types with 'Num' instances, and you want to avoid writing
-- orphan instances for external types.
--
-- If you have external types that are not 'Num' instances, consider
-- instead "Numeric.Backprop.External".
--
-- If you need a 'Num' instance for tuples, you can use the canonical 2-
-- and 3-tuples for the library in "Numeric.Backprop.Tuple".  If you need
-- one for larger tuples, consider making a custom product type instead
-- (making Num instances with something like
-- <https://hackage.haskell.org/package/one-liner-instances
-- one-liner-instances>).  You can also use the orphan instances in the
-- <https://hackage.haskell.org/package/NumInstances NumInstances> package
-- (in particular, "Data.NumInstances.Tuple") if you are writing an
-- application and do not have to worry about orphan instances.
--
-- See "Numeric.Backprop" for fuller documentation on using these
-- functions.
--
-- @since 0.2.0.0

module Numeric.Backprop.Num (
    -- * Types
    BVar, W
    -- * Running
  , backprop, E.evalBP, gradBP, backpropWith
    -- ** Multiple inputs
  , E.evalBP0
  , backprop2, E.evalBP2, gradBP2, backpropWith2
  , backpropN, E.evalBPN, gradBPN, backpropWithN, Every
    -- * Manipulating 'BVar'
  , E.constVar, E.auto, E.coerceVar
  , (^^.), (.~~), (%~~), (^^?), (^^..), (^^?!)
  , viewVar, setVar, overVar
  , sequenceVar, collectVar
  , previewVar, toListOfVar
    -- ** With Isomorphisms
  , isoVar, isoVar2, isoVar3, isoVarN
    -- ** With 'Op's
  , liftOp
  , liftOp1, liftOp2, liftOp3
    -- * 'Op'
  , Op(..)
    -- ** Creation
  , op0, opConst, idOp
  , opConst'
    -- *** Giving gradients directly
  , op1, op2, op3
    -- *** From Isomorphisms
  , opCoerce, opTup, opIso, opIsoN, opLens
    -- *** No gradients
  , noGrad1, noGrad
    -- * Utility
    -- ** Inductive tuples/heterogeneous lists
  , Prod(..), pattern (:>), only, head'
  , Tuple, pattern (::<), only_
  , I(..)
    -- ** Misc
  , Reifies
  ) where

import           Data.Bifunctor
import           Data.Maybe
import           Data.Reflection
import           Data.Type.Index
import           Data.Type.Length
import           Lens.Micro
import           Numeric.Backprop.Explicit (BVar, W)
import           Numeric.Backprop.Op
import           Type.Class.Known
import qualified Numeric.Backprop.Explicit as E

-- | 'Numeric.Backprop.backpropN', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- The @'Every' 'Num' as@ in the constraint says that every value in the
-- type-level list @as@ must have a 'Num' instance.  This means you can
-- use, say, @'[Double, Float, Int]@, but not @'[Double, Bool, String]@.
--
-- If you stick to /concerete/, monomorphic usage of this (with specific
-- types, typed into source code, known at compile-time), then @'Every'
-- 'Num' as@ should be fulfilled automatically.
--
backpropN
    :: (Every Num as, Known Length as, Num b)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, Tuple as)
backpropN f = second ($ E.ofNum) . E.backpropN E.zfNums f
{-# INLINE backpropN #-}

-- | 'Numeric.Backprop.backpropWithN', but with 'Num' constraints instead
-- of 'Backprop' constraints.
--
-- See 'backpropN' for information on the 'Every' constraint.
--
-- Note that argument order changed in v0.2.3.
--
-- @since 0.2.0.0
backpropWithN
    :: (Every Num as, Known Length as)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, (b -> b) -> Tuple as) -- ^ Takes function giving gradient of final result given the output of function
backpropWithN = E.backpropWithN E.zfNums
{-# INLINE backpropWithN #-}

-- | 'Numeric.Backprop.backprop', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- See module documentation for "Numeric.Backprop.Num" for information on
-- using this with tuples.
backprop
    :: (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop f = second ($ E.ofNum) . E.backprop E.zfNum f
{-# INLINE backprop #-}

-- | 'Numeric.Backprop.backpropWith', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- See module documentation for "Numeric.Backprop.Num" for information on
-- using this with tuples.
--
-- Note that argument order changed in v0.2.3.
--
-- @since 0.2.0.0
backpropWith
    :: Num a
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, (b -> b) -> a) -- ^ Takes function giving gradient of final result given the output of function
backpropWith = E.backpropWith E.zfNum
{-# INLINE backpropWith #-}

-- | 'Numeric.Backprop.gradBP', but with 'Num' constraints instead of
-- 'Backprop' constraints.
gradBP
    :: (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> a
gradBP = E.gradBP E.zfNum E.ofNum
{-# INLINE gradBP #-}

-- | 'Numeric.Backprop.gradBPN', but with 'Num' constraints instead of
-- 'Backprop' constraints.
gradBPN
    :: (Every Num as, Known Length as, Num b)
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> Tuple as
gradBPN = E.gradBPN E.zfNums E.ofNum
{-# INLINE gradBPN #-}

-- | 'Numeric.Backprop.backprop2', but with 'Num' constraints instead of
-- 'Backprop' constraints.
backprop2
    :: (Num a, Num b, Num c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c, (a, b))
backprop2 f x = second ($ E.ofNum) . E.backprop2 E.zfNum E.zfNum f x
{-# INLINE backprop2 #-}

-- | 'Numeric.Backprop.backpropWith2', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Note that argument order changed in v0.2.3.
--
-- @since 0.2.0.0
backpropWith2
    :: (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c, (c -> c) -> (a, b)) -- ^ Takes function giving gradient of final result given the output of function
backpropWith2 = E.backpropWith2 E.zfNum E.zfNum
{-# INLINE backpropWith2 #-}

-- | 'Numeric.Backprop.gradBP2', but with 'Num' constraints instead of
-- 'Backprop' constraints.
gradBP2
    :: (Num a, Num b, Num c)
    => (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (a, b)
gradBP2 = E.gradBP2 E.zfNum E.zfNum E.ofNum
{-# INLINE gradBP2 #-}

-- | 'Numeric.Backprop.^^.', but with 'Num' constraints instead of
-- 'Backprop' constraints.
(^^.)
    :: forall b a s. (Num a, Reifies s W)
    => BVar s b
    -> Lens' b a
    -> BVar s a
x ^^. l = viewVar l x
infixl 8 ^^.
{-# INLINE (^^.) #-}

-- | 'Numeric.Backprop.viewVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
viewVar
    :: forall b a s. (Num a, Reifies s W)
    => Lens' b a
    -> BVar s b
    -> BVar s a
viewVar = E.viewVar E.afNum E.zfNum
{-# INLINE viewVar #-}


-- | 'Numeric.Backprop..~~', but with 'Num' constraints instead of
-- 'Backprop' constraints.
(.~~)
    :: (Num a, Num b, Reifies s W)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
l .~~ x = setVar l x
infixl 8 .~~
{-# INLINE (.~~) #-}

-- | 'Numeric.Backprop.setVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
setVar
    :: forall a b s. (Num a, Num b, Reifies s W)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
setVar = E.setVar E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE setVar #-}

-- | 'Numeric.Backprop.%~~', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- @since 0.2.4.0
--
(%~~)
    :: (Num a, Num b, Reifies s W)
    => Lens' b a
    -> (BVar s a -> BVar s a)
    -> BVar s b
    -> BVar s b
l %~~ f = overVar l f
infixr 4 %~~
{-# INLINE (%~~) #-}

-- | 'Numeric.Backprop.overVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- @since 0.2.4.0
overVar
    :: (Num a, Num b, Reifies s W)
    => Lens' b a
    -> (BVar s a -> BVar s a)
    -> BVar s b
    -> BVar s b
overVar = E.overVar E.afNum E.afNum E.zfNum E.zfNum
{-# INLINE overVar #-}

-- | 'Numeric.Backprop.^^?', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Note that many automatically-generated prisms by the /lens/ package use
-- tuples, which cannot work this this by default (because tuples do not
-- have a 'Num' instance).
--
-- If you are writing an application or don't have to worry about orphan
-- instances, you can pull in the orphan instances from
-- <https://hackage.haskell.org/package/NumInstances NumInstances>.
-- Alternatively, you can chain those prisms with conversions to the
-- anonymous canonical strict tuple types in "Numeric.Backprop.Tuple",
-- which do have 'Num' instances.
--
-- @
-- myPrism                   :: 'Prism'' c (a, b)
-- myPrism . 'iso' 'tupT2' 't2Tup' :: 'Prism'' c ('T2' a b)
-- @
(^^?)
    :: forall b a s. (Num a, Reifies s W)
    => BVar s b
    -> Traversal' b a
    -> Maybe (BVar s a)
v ^^? t = previewVar t v
{-# INLINE (^^?) #-}

-- | 'Numeric.Backprop.^^?!', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Like 'Numeric.Backprop.^^?!', is *UNSAFE*.
--
-- @since 0.2.1.0
(^^?!)
    :: forall b a s. (Num a, Reifies s W)
    => BVar s b
    -> Traversal' b a
    -> BVar s a
v ^^?! t = fromMaybe (error e) (previewVar t v)
  where
    e = "Numeric.Backprop.Num.^^?!: Empty traversal"
{-# INLINE (^^?!) #-}

-- | 'Numeric.Backprop.previewVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- See documentation for '^^?' for more information and important notes.
previewVar
    :: forall b a s. (Num a, Reifies s W)
    => Traversal' b a
    -> BVar s b
    -> Maybe (BVar s a)
previewVar = E.previewVar E.afNum E.zfNum
{-# INLINE previewVar #-}

-- | 'Numeric.Backprop.^^..', but with 'Num' constraints instead of
-- 'Backprop' constraints.
(^^..)
    :: forall b a s. (Num a, Reifies s W)
    => BVar s b
    -> Traversal' b a
    -> [BVar s a]
v ^^.. t = toListOfVar t v
{-# INLINE (^^..) #-}

-- | 'Numeric.Backprop.toListOfVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
toListOfVar
    :: forall b a s. (Num a, Reifies s W)
    => Traversal' b a
    -> BVar s b
    -> [BVar s a]
toListOfVar = E.toListOfVar E.afNum E.zfNum
{-# INLINE toListOfVar #-}

-- | 'Numeric.Backprop.sequenceVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
sequenceVar
    :: (Traversable t, Num a, Reifies s W)
    => BVar s (t a)
    -> t (BVar s a)
sequenceVar = E.sequenceVar E.afNum E.zfNum
{-# INLINE sequenceVar #-}

-- | 'Numeric.Backprop.collectVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
--
-- Prior to v0.2.3, required a 'Num' constraint on @t a@.
collectVar
    :: (Foldable t, Functor t, Num a, Reifies s W)
    => t (BVar s a)
    -> BVar s (t a)
collectVar = E.collectVar E.afNum E.zfNum
{-# INLINE collectVar #-}

-- | 'Numeric.Backprop.liftOp', but with 'Num' constraints instead of
-- 'Backprop' constraints.
liftOp
    :: (Every Num as, Known Length as, Num b, Reifies s W)
    => Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOp = E.liftOp E.afNums E.zfNum
{-# INLINE liftOp #-}

-- | 'Numeric.Backprop.liftOp1', but with 'Num' constraints instead of
-- 'Backprop' constraints.
liftOp1
    :: (Num a, Num b, Reifies s W)
    => Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 = E.liftOp1 E.afNum E.zfNum
{-# INLINE liftOp1 #-}

-- | 'Numeric.Backprop.liftOp2', but with 'Num' constraints instead of
-- 'Backprop' constraints.
liftOp2
    :: (Num a, Num b, Num c, Reifies s W)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 = E.liftOp2 E.afNum E.afNum E.zfNum
{-# INLINE liftOp2 #-}

-- | 'Numeric.Backprop.liftOp3', but with 'Num' constraints instead of
-- 'Backprop' constraints.
liftOp3
    :: (Num a, Num b, Num c, Num d, Reifies s W)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 = E.liftOp3 E.afNum E.afNum E.afNum E.zfNum
{-# INLINE liftOp3 #-}

-- | 'Numeric.Backprop.isoVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
isoVar
    :: (Num a, Num b, Reifies s W)
    => (a -> b)
    -> (b -> a)
    -> BVar s a
    -> BVar s b
isoVar = E.isoVar E.afNum E.zfNum
{-# INLINE isoVar #-}

-- | 'Numeric.Backprop.isoVar', but with 'Num' constraints instead of
-- 'Backprop' constraints.
isoVar2
    :: (Num a, Num b, Num c, Reifies s W)
    => (a -> b -> c)
    -> (c -> (a, b))
    -> BVar s a
    -> BVar s b
    -> BVar s c
isoVar2 = E.isoVar2 E.afNum E.afNum E.zfNum
{-# INLINE isoVar2 #-}

-- | 'Numeric.Backprop.isoVar3', but with 'Num' constraints instead of
-- 'Backprop' constraints.
isoVar3
    :: (Num a, Num b, Num c, Num d, Reifies s W)
    => (a -> b -> c -> d)
    -> (d -> (a, b, c))
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
isoVar3 = E.isoVar3 E.afNum E.afNum E.afNum E.zfNum
{-# INLINE isoVar3 #-}

-- | 'Numeric.Backprop.isoVarN', but with 'Num' constraints instead of
-- 'Backprop' constraints.
isoVarN
    :: (Every Num as, Known Length as, Num b, Reifies s W)
    => (Tuple as -> b)
    -> (b -> Tuple as)
    -> Prod (BVar s) as
    -> BVar s b
isoVarN = E.isoVarN E.afNums E.zfNum
{-# INLINE isoVarN #-}
