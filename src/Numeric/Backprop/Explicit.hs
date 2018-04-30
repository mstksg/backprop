{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Numeric.Backprop.Explicit
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides "explicit" versions of all of the functions in
-- "Numeric.Backprop".  Instead of relying on a 'Backprop' instance, allows
-- you to manually provide 'zero', 'add', and 'one' on a per-value basis.
--
-- It is recommended you use 'Numeric.Backprop' or 'Numeric.Backprop.Num'
-- instead, unless your type has no 'Num' instance, or you else you want to
-- avoid defining orphan 'Backprop' instances for external types.
--
-- See "Numeric.Backprop" for fuller documentation on using these
-- functions.
--
-- @since 0.2.0.0

module Numeric.Backprop.Explicit (
    -- * Types
    BVar, W, Backprop(..)
    -- * Explicit 'zero', 'add', and 'one'
  , ZeroFunc(..), zfNum, zfNums, zeroFunc, zeroFuncs
  , AddFunc(..), afNum, afNums, addFunc, addFuncs
  , OneFunc(..), ofNum, ofNums, oneFunc, oneFuncs
    -- * Running
  , backprop, evalBP, gradBP, backpropWith
    -- ** Multiple inputs
  , backprop2, evalBP2, gradBP2, backpropWith2
  , backpropN, evalBPN, gradBPN, backpropWithN, Every
    -- * Manipulating 'BVar'
  , constVar, auto, coerceVar
  -- , (^^.), (.~~), (^^?), (^^..)
  , viewVar, setVar
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
import           Data.Reflection
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Numeric.Backprop.Class
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Op
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
zfNums :: (Every Num as, Known Length as) => Prod ZeroFunc as
zfNums = map1 (\i -> zfNum \\ every @_ @Num i) indices

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
afNums :: (Every Num as, Known Length as) => Prod AddFunc as
afNums = map1 (\i -> afNum \\ every @_ @Num i) indices

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
ofNums :: (Every Num as, Known Length as) => Prod OneFunc as
ofNums = map1 (\i -> ofNum \\ every @_ @Num i) indices

-- | The canonical 'ZeroFunc' for instances of 'Backprop'.
zeroFunc :: Backprop a => ZeroFunc a
zeroFunc = ZF zero
{-# INLINE zeroFunc #-}

-- | The canonical 'AddFunc' for instances of 'Backprop'.
addFunc :: Backprop a => AddFunc a
addFunc = AF add
{-# INLINE addFunc #-}

-- | The canonical 'OneFunc' for instances of 'Backprop'.
oneFunc :: Backprop a => OneFunc a
oneFunc = OF one
{-# INLINE oneFunc #-}

-- | Generate an 'ZeroFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
zeroFuncs :: (Every Backprop as, Known Length as) => Prod ZeroFunc as
zeroFuncs = map1 (\i -> zeroFunc \\ every @_ @Backprop i) indices

-- | Generate an 'AddFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
addFuncs :: (Every Backprop as, Known Length as) => Prod AddFunc as
addFuncs = map1 (\i -> addFunc \\ every @_ @Backprop i) indices

-- | Generate an 'OneFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
oneFuncs :: (Every Backprop as, Known Length as) => Prod OneFunc as
oneFuncs = map1 (\i -> oneFunc \\ every @_ @Backprop i) indices

-- | Shorter alias for 'constVar', inspired by the /ad/ library.
auto :: a -> BVar s a
auto = constVar
{-# INLINE auto #-}

-- | 'Numeric.Backprop.backpropWithN', but with explicit 'zero'.
backpropWithN
    :: Prod ZeroFunc as
    -> (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b -> b)                 -- ^ Gradient of final result with respect to output of function
    -> (b, Tuple as)
backpropWithN zfs f xs g = backpropN zfs (OF g) f xs
{-# INLINE backpropWithN #-}

-- | 'Numeric.Backprop.backprop', but with explicit 'zero' and 'one'.
backprop
    :: ZeroFunc a
    -> OneFunc b
    -> (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop zfa ofb f = second (getI . head')
                   . backpropN (zfa :< Ø) ofb (f . head')
                   . only_
{-# INLINE backprop #-}

-- | 'Numeric.Backprop.backpropWith', but with explicit 'zero'.
backpropWith
    :: ZeroFunc a
    -> (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b -> b)                 -- ^ Gradient of final result with respect to output of function
    -> (b, a)
backpropWith zfa f x g = backprop zfa (OF g) f x
{-# INLINE backpropWith #-}

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents.
--
-- Benchmarks show that this should have virtually no overhead over
-- directly writing a @a -> b@. 'BVar' is, in this situation, a zero-cost
-- abstraction, performance-wise.
--
-- See documentation of 'Numeric.Backprop.backprop' for more information.
evalBP :: (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> b
evalBP f = evalBPN (f . head') . only_
{-# INLINE evalBP #-}

-- | 'Numeric.Backprop.gradBP', but with explicit 'zero' and 'one'.
gradBP
    :: ZeroFunc a
    -> OneFunc b
    -> (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> a
gradBP zfa ofb f = snd . backprop zfa ofb f
{-# INLINE gradBP #-}

-- | 'Numeric.Backprop.gradBP', Nbut with explicit 'zero' and 'one'.
gradBPN
    :: Prod ZeroFunc as
    -> OneFunc b
    -> (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> Tuple as
gradBPN zfas ofb f = snd . backpropN zfas ofb f
{-# INLINE gradBPN #-}

-- | 'Numeric.Backprop.backprop2', but with explicit 'zero' and 'one'.
backprop2
    :: ZeroFunc a
    -> ZeroFunc b
    -> OneFunc c
    -> (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c, (a, b))
backprop2 zfa zfb ofc f x y = second (\(dx ::< dy ::< Ø) -> (dx, dy)) $
    backpropN (zfa :< zfb :< Ø) ofc
        (\(x' :< y' :< Ø) -> f x' y')
        (x ::< y ::< Ø)
{-# INLINE backprop2 #-}

-- | 'Numeric.Backprop.backpropWith2', but with explicit 'zero'.
backpropWith2
    :: ZeroFunc a
    -> ZeroFunc b
    -> (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c -> c)                 -- ^ Gradient of final result with respect to output of function
    -> (c, (a, b))
backpropWith2 zfa zfb f x y g = backprop2 zfa zfb (OF g) f x y
{-# INLINE backpropWith2 #-}

-- | 'evalBP' for a two-argument function.  See
-- 'Numeric.Backprop.backprop2' for notes.
evalBP2
    :: (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> c
evalBP2 f x y = evalBPN (\(x' :< y' :< Ø) -> f x' y') (x ::< y ::< Ø)
{-# INLINE evalBP2 #-}

-- | 'gradBP' for a two-argument function.  See
-- 'Numeric.Backprop.backprop2' for notes.
gradBP2
    :: ZeroFunc a
    -> ZeroFunc b
    -> OneFunc c
    -> (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (a, b)
gradBP2 zfa zfb ofc f x = snd . backprop2 zfa zfb ofc f x
{-# INLINE gradBP2 #-}

-- | 'Numeric.Backprop.isoVar' with explicit 'add' and 'zero'.
isoVar
    :: Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> (a -> b)
    -> (b -> a)
    -> BVar s a
    -> BVar s b
isoVar af z f g = liftOp1 af z (opIso f g)
{-# INLINE isoVar #-}

-- | 'Numeric.Backprop.isoVar2' with explicit 'add' and 'zero'.
isoVar2
    :: Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc c
    -> (a -> b -> c)
    -> (c -> (a, b))
    -> BVar s a
    -> BVar s b
    -> BVar s c
isoVar2 afa afb z f g = liftOp2 afa afb z (opIso2 f g)
{-# INLINE isoVar2 #-}

-- | 'Numeric.Backprop.isoVar3' with explicit 'add' and 'zero'.
isoVar3
    :: Reifies s W
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> ZeroFunc d
    -> (a -> b -> c -> d)
    -> (d -> (a, b, c))
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
isoVar3 afa afb afc z f g = liftOp3 afa afb afc z (opIso3 f g)
{-# INLINE isoVar3 #-}

-- | 'Numeric.Backprop.isoVarN' with explicit 'add' and 'zero'.
isoVarN
    :: Reifies s W
    => Prod AddFunc as
    -> ZeroFunc b
    -> (Tuple as -> b)
    -> (b -> Tuple as)
    -> Prod (BVar s) as
    -> BVar s b
isoVarN afs z f g = liftOp afs z (opIsoN f g)
{-# INLINE isoVarN #-}
