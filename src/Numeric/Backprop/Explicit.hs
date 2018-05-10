{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

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
-- avoid defining orphan 'Backprop' instances for external types.  Can also
-- be useful if mixing and matching styles.
--
-- See "Numeric.Backprop" for fuller documentation on using these
-- functions.
--
-- @since 0.2.0.0

module Numeric.Backprop.Explicit (
    -- * Types
    BVar, W, Backprop(..), ABP(..), NumBP(..)
    -- * Explicit 'zero', 'add', and 'one'
  , ZeroFunc(..), zfNum, zfNums, zeroFunc, zeroFuncs, zfFunctor
  , AddFunc(..), afNum, afNums, addFunc, addFuncs
  , OneFunc(..), ofNum, ofNums, oneFunc, oneFuncs, ofFunctor
    -- * Running
  , backprop, evalBP, gradBP, backpropWith
    -- ** Multiple inputs
  , backprop2, evalBP2, gradBP2, backpropWith2
  , backpropN, evalBPN, gradBPN, backpropWithN, Every
    -- * Manipulating 'BVar'
  , constVar, auto, coerceVar
  , viewVar, setVar
  , sequenceVar, collectVar
  , previewVar, toListOfVar
    -- ** With Isomorphisms
  , isoVar, isoVar2, isoVar3, isoVarN
    -- ** With 'Op's
  , liftOp
  , liftOp1, liftOp2, liftOp3
    -- ** Generics
  , splitBV
  , joinBV
  , BVGroup
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
import           Data.Type.Util
import           GHC.Generics              as G
import           Lens.Micro
import           Numeric.Backprop.Class
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Op
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.List
import           Unsafe.Coerce

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
--
-- @since 0.2.0.0
zfNums :: (Every Num as, Known Length as) => Prod ZeroFunc as
zfNums = map1 (\i -> zfNum \\ every @_ @Num i) indices

-- | 'zeroFunc' for instances of 'Functor'
--
-- @since 0.2.1.0
zfFunctor :: (Backprop a, Functor f) => ZeroFunc (f a)
zfFunctor = ZF zeroFunctor
{-# INLINE zfFunctor #-}

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
--
-- @since 0.2.0.0
afNums :: (Every Num as, Known Length as) => Prod AddFunc as
afNums = map1 (\i -> afNum \\ every @_ @Num i) indices

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
--
-- @since 0.2.0.0
ofNums :: (Every Num as, Known Length as) => Prod OneFunc as
ofNums = map1 (\i -> ofNum \\ every @_ @Num i) indices

-- | 'OneFunc' for instances of 'Functor'
--
-- @since 0.2.1.0
ofFunctor :: (Backprop a, Functor f) => OneFunc (f a)
ofFunctor = OF oneFunctor
{-# INLINE ofFunctor #-}

-- | Generate an 'ZeroFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
--
-- @since 0.2.0.0
zeroFuncs :: (Every Backprop as, Known Length as) => Prod ZeroFunc as
zeroFuncs = map1 (\i -> zeroFunc \\ every @_ @Backprop i) indices

-- | Generate an 'AddFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
--
-- @since 0.2.0.0
addFuncs :: (Every Backprop as, Known Length as) => Prod AddFunc as
addFuncs = map1 (\i -> addFunc \\ every @_ @Backprop i) indices

-- | Generate an 'OneFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
--
-- @since 0.2.0.0
oneFuncs :: (Every Backprop as, Known Length as) => Prod OneFunc as
oneFuncs = map1 (\i -> oneFunc \\ every @_ @Backprop i) indices

-- | Shorter alias for 'constVar', inspired by the /ad/ library.
--
-- @since 0.2.0.0
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

-- | Helper class for generically "splitting" and "joining" 'BVar's into
-- constructors.  See 'splitBV' and 'joinBV'.
--
-- See "Numeric.Backprop#hkd" for a tutorial on how to use this.
--
-- Instances should be available for types made with one constructor whose
-- fields are all instances of 'Backprop', with a 'Generic' instance.
--
-- @since 0.2.2.0
class BVGroup s as i o | o -> i, i -> as where
    -- | Helper method for generically "splitting" 'BVar's out of
    -- constructors inside a 'BVar'.  See 'splitBV'.
    gsplitBV :: Prod AddFunc as -> Prod ZeroFunc as -> BVar s (i ()) -> o ()
    -- | Helper method for generically "joining" 'BVar's inside
    -- a constructor into a 'BVar'.  See 'joinBV'.
    gjoinBV  :: Prod AddFunc as -> Prod ZeroFunc as -> o () -> BVar s (i ())

instance BVGroup s '[] (K1 i a) (K1 i (BVar s a)) where
    gsplitBV _ _ = K1 . coerceVar
    {-# INLINE gsplitBV #-}
    gjoinBV  _ _ = coerceVar . unK1
    {-# INLINE gjoinBV #-}

instance BVGroup s as i o
        => BVGroup s as (M1 p c i) (M1 p c o) where
    gsplitBV afs zfs = M1 . gsplitBV afs zfs . coerceVar @_ @(i ())
    {-# INLINE gsplitBV #-}
    gjoinBV afs zfs = coerceVar @(i ()) . gjoinBV afs zfs . unM1
    {-# INLINE gjoinBV #-}

instance BVGroup s '[] V1 V1 where
    gsplitBV _ _ = unsafeCoerce
    {-# INLINE gsplitBV #-}
    gjoinBV _ _ = \case
    {-# INLINE gjoinBV #-}

instance BVGroup s '[] U1 U1 where
    gsplitBV _ _ _ = U1
    {-# INLINE gsplitBV #-}
    gjoinBV _ _ _ = constVar U1
    {-# INLINE gjoinBV #-}

instance ( Reifies s W
         , BVGroup s as i1 o1
         , BVGroup s bs i2 o2
         , cs ~ (as ++ bs)
         , Known Length as
         ) => BVGroup s (i1 () ': i2 () ': cs) (i1 :*: i2) (o1 :*: o2) where
    gsplitBV (afa :< afb :< afs) (zfa :< zfb :< zfs) xy = x :*: y
      where
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
        x = gsplitBV afas zfas . viewVar afa zfa p1 $ xy
        y = gsplitBV afbs zfbs . viewVar afb zfb p2 $ xy
    {-# INLINE gsplitBV #-}
    gjoinBV (afa :< afb :< afs) (zfa :< zfb :< zfs) (x :*: y)
        = liftOp2 afa afb zfab (opIso2 (:*:) unP)
            (gjoinBV afas zfas x)
            (gjoinBV afbs zfbs y)
      where
        zfab = ZF $ \(xx :*: yy) -> runZF zfa xx :*: runZF zfb yy
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
        unP (xx :*: yy) = (xx, yy)
    {-# INLINE gjoinBV #-}

-- | This instance is possible but it is not clear when it would be useful
instance ( Reifies s W
         , BVGroup s as i1 o1
         , BVGroup s bs i2 o2
         , cs ~ (as ++ bs)
         , Known Length as
         ) => BVGroup s (i1 () ': i2 () ': cs) (i1 :+: i2) (o1 :+: o2) where
    gsplitBV (afa :< afb :< afs) (zfa :< zfb :< zfs) xy =
        case previewVar afa zfa s1 xy of
          Just x -> L1 $ gsplitBV afas zfas x
          Nothing -> case previewVar afb zfb s2 xy of
            Just y -> R1 $ gsplitBV afbs zfbs y
            Nothing -> error "Numeric.Backprop.gsplitBV: Internal error occurred"
      where
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
    {-# INLINE gsplitBV #-}
    gjoinBV (afa :< afb :< afs) (zfa :< zfb :< zfs) = \case
        L1 x -> liftOp1 afa zf (op1 (\xx -> (L1 xx, \case L1 d -> d; R1 _ -> runZF zfa xx)))
                    (gjoinBV afas zfas x)
        R1 y -> liftOp1 afb zf (op1 (\yy -> (R1 yy, \case L1 _ -> runZF zfb yy; R1 d -> d)))
                    (gjoinBV afbs zfbs y)
      where
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
        zf = ZF $ \case
            L1 xx -> L1 $ runZF zfa xx
            R1 yy -> R1 $ runZF zfb yy
    {-# INLINE gjoinBV #-}

-- | 'Numeric.Backprop.splitBV' with explicit 'add' and 'zero'.
--
-- @since 0.2.2.0
splitBV
    :: forall z f as s.
       ( Generic (z f)
       , Generic (z (BVar s))
       , BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
       , Reifies s W
       )
    => AddFunc (Rep (z f) ())
    -> Prod AddFunc as
    -> ZeroFunc (Rep (z f) ())
    -> Prod ZeroFunc as
    -> BVar s (z f)             -- ^ 'BVar' of value
    -> z (BVar s)               -- ^ 'BVar's of fields
splitBV af afs zf zfs =
        G.to
      . gsplitBV afs zfs
      . viewVar af zf (lens (from @(z f) @()) (const G.to))
{-# INLINE splitBV #-}

-- | 'Numeric.Backprop.joinBV' with explicit 'add' and 'zero'.
--
-- @since 0.2.2.0
joinBV
    :: forall z f as s.
       ( Generic (z f)
       , Generic (z (BVar s))
       , BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
       , Reifies s W
       )
    => AddFunc (z f)
    -> Prod AddFunc as
    -> ZeroFunc (z f)
    -> Prod ZeroFunc as
    -> z (BVar s)           -- ^ 'BVar's of fields
    -> BVar s (z f)         -- ^ 'BVar' of combined value
joinBV af afs zf zfs =
        viewVar af zf (lens G.to (const from))
      . gjoinBV afs zfs
      . from @(z (BVar s)) @()
{-# INLINE joinBV #-}
