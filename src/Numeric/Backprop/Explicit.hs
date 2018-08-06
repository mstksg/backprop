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
{-# OPTIONS_HADDOCK not-home        #-}

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
-- It is recommended you use "Numeric.Backprop" or "Numeric.Backprop.Num"
-- instead, unless your type has no 'Num' instance, or you else you want to
-- avoid defining orphan 'Backprop' instances for external types.  Can also
-- be useful if mixing and matching styles.
--
-- See "Numeric.Backprop" for fuller documentation on using these
-- functions.
--
-- WARNING: API of this module can be considered only "semi-stable"; while
-- the API of "Numeric.Backprop" and "Numeric.Backprop.Num" are kept
-- consistent, some argument order changes might happen in this module to
-- reflect changes in underlying implementation.
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
  , evalBP0
  , backprop2, evalBP2, gradBP2, backpropWith2
  , backpropN, evalBPN, gradBPN, backpropWithN, RPureConstrained
    -- * Manipulating 'BVar'
  , constVar, auto, coerceVar
  , viewVar, setVar, overVar
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
  , bpOp
    -- *** Giving gradients directly
  , op1, op2, op3
    -- *** From Isomorphisms
  , opCoerce, opTup, opIso, opIsoN, opLens
    -- *** No gradients
  , noGrad1, noGrad
    -- * Utility
  , Rec(..), Reifies
  ) where

import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Reflection
import           Data.Type.Util
import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           GHC.Generics              as G
import           Lens.Micro
import           Numeric.Backprop.Class
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Op
import           Unsafe.Coerce

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
--
-- @since 0.2.0.0
zfNums :: RPureConstrained Num as => Rec ZeroFunc as
zfNums = rpureConstrained @Num zfNum

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
afNums :: RPureConstrained Num as => Rec AddFunc as
afNums = rpureConstrained @Num afNum

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
--
-- @since 0.2.0.0
ofNums :: RPureConstrained Num as => Rec OneFunc as
ofNums = rpureConstrained @Num ofNum

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
zeroFuncs :: RPureConstrained Backprop as => Rec ZeroFunc as
zeroFuncs = rpureConstrained @Backprop zeroFunc

-- | Generate an 'AddFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
--
-- @since 0.2.0.0
addFuncs :: RPureConstrained Backprop as => Rec AddFunc as
addFuncs = rpureConstrained @Backprop addFunc

-- | Generate an 'OneFunc' for every type in a type-level list, if every
-- type has an instance of 'Backprop'.
--
-- @since 0.2.0.0
oneFuncs :: RPureConstrained Backprop as => Rec OneFunc as
oneFuncs = rpureConstrained @Backprop oneFunc

-- | Shorter alias for 'constVar', inspired by the /ad/ library.
--
-- @since 0.2.0.0
auto :: a -> BVar s a
auto = constVar
{-# INLINE auto #-}

-- | 'Numeric.Backprop.backpropN', but with explicit 'zero' and 'one'.
backpropN
    :: forall as b. ()
    => Rec ZeroFunc as
    -> OneFunc b
    -> (forall s. Reifies s W => Rec (BVar s) as -> BVar s b)
    -> Rec Identity as
    -> (b, Rec Identity as)
backpropN zfs ob f xs = case backpropWithN zfs f xs of
    (y, g) -> (y, g (runOF ob y))
{-# INLINE backpropN #-}

-- | 'Numeric.Backprop.backprop', but with explicit 'zero' and 'one'.
backprop
    :: ZeroFunc a
    -> OneFunc b
    -> (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop zfa ofb f = second (\case Identity x :& RNil -> x)
                   . backpropN (zfa :& RNil) ofb (f . (\case x :& RNil -> x))
                   . (:& RNil)
                   . Identity
{-# INLINE backprop #-}

-- | 'Numeric.Backprop.backpropWith', but with explicit 'zero'.
--
-- Note that argument order changed in v0.2.4.
backpropWith
    :: ZeroFunc a
    -> (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, b -> a)
backpropWith zfa f = second ((\case Identity x :& RNil -> x) .)
                   . backpropWithN (zfa :& RNil) (f . (\case x :& RNil -> x))
                   . (:& RNil)
                   . Identity
{-# INLINE backpropWith #-}

-- | 'evalBP' but with no arguments.  Useful when everything is just given
-- through 'constVar'.
evalBP0 :: (forall s. Reifies s W => BVar s a) -> a
evalBP0 x = evalBPN (const x) RNil
{-# INLINE evalBP0 #-}

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents.
--
-- Benchmarks show that this should have virtually no overhead over
-- directly writing a @a -> b@. 'BVar' is, in this situation, a zero-cost
-- abstraction, performance-wise.
--
-- See documentation of 'Numeric.Backprop.backprop' for more information.
evalBP :: (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> b
evalBP f = evalBPN (f . (\case x :& RNil -> x)) . (:& RNil)  . Identity
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
    :: Rec ZeroFunc as
    -> OneFunc b
    -> (forall s. Reifies s W => Rec (BVar s) as -> BVar s b)
    -> Rec Identity as
    -> Rec Identity as
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
backprop2 zfa zfb ofc f x y = second (\(Identity dx :& Identity dy :& RNil) -> (dx, dy)) $
    backpropN (zfa :& zfb :& RNil) ofc
        (\(x' :& y' :& RNil) -> f x' y')
        (Identity x :& Identity y :& RNil)
{-# INLINE backprop2 #-}

-- | 'Numeric.Backprop.backpropWith2', but with explicit 'zero'.
--
-- Note that argument order changed in v0.2.4.
--
-- @since 0.2.0.0
backpropWith2
    :: ZeroFunc a
    -> ZeroFunc b
    -> (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> (c, c -> (a, b))
backpropWith2 zfa zfb f x y = second ((\(Identity dx :& Identity dy :& RNil) -> (dx, dy)) .) $
    backpropWithN (zfa :& zfb :& RNil)
        (\(x' :& y' :& RNil) -> f x' y')
        (Identity x :& Identity y :& RNil)
{-# INLINE backpropWith2 #-}

-- | 'evalBP' for a two-argument function.  See
-- 'Numeric.Backprop.backprop2' for notes.
evalBP2
    :: (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c)
    -> a
    -> b
    -> c
evalBP2 f x y = evalBPN (\(x' :& y' :& RNil) -> f x' y') $ Identity x
                                                        :& Identity y
                                                        :& RNil
{-# INLINE evalBP2 #-}

-- | 'Numeric.Backprop.gradBP2' with explicit 'zero' and 'one'.
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

-- | 'Numeric.Backprop.bpOp' with explicit 'zero'.
bpOp
    :: Rec ZeroFunc as
    -> (forall s. Reifies s W => Rec (BVar s) as -> BVar s b)
    -> Op as b
bpOp zfs f = Op (backpropWithN zfs f)
{-# INLINE bpOp #-}

-- | 'Numeric.Backprop.overVar' with explicit 'add' and 'zero'.
--
-- @since 0.2.4.0
overVar
    :: Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc a
    -> ZeroFunc b
    -> Lens' b a
    -> (BVar s a -> BVar s a)
    -> BVar s b
    -> BVar s b
overVar afa afb zfa zfb l f x = setVar afa afb zfa l (f (viewVar afa zfb l x)) x
{-# INLINE overVar #-}

-- | 'Numeric.Backprop.isoVar' with explicit 'add' and 'zero'.
isoVar
    :: Reifies s W
    => AddFunc a
    -> (a -> b)
    -> (b -> a)
    -> BVar s a
    -> BVar s b
isoVar af f g = liftOp1 af (opIso f g)
{-# INLINE isoVar #-}

-- | 'Numeric.Backprop.isoVar2' with explicit 'add' and 'zero'.
isoVar2
    :: Reifies s W
    => AddFunc a
    -> AddFunc b
    -> (a -> b -> c)
    -> (c -> (a, b))
    -> BVar s a
    -> BVar s b
    -> BVar s c
isoVar2 afa afb f g = liftOp2 afa afb (opIso2 f g)
{-# INLINE isoVar2 #-}

-- | 'Numeric.Backprop.isoVar3' with explicit 'add' and 'zero'.
isoVar3
    :: Reifies s W
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> (a -> b -> c -> d)
    -> (d -> (a, b, c))
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
isoVar3 afa afb afc f g = liftOp3 afa afb afc (opIso3 f g)
{-# INLINE isoVar3 #-}

-- | 'Numeric.Backprop.isoVarN' with explicit 'add' and 'zero'.
isoVarN
    :: Reifies s W
    => Rec AddFunc as
    -> (Rec Identity as -> b)
    -> (b -> Rec Identity as)
    -> Rec (BVar s) as
    -> BVar s b
isoVarN afs f g = liftOp afs (opIsoN f g)
{-# INLINE isoVarN #-}

-- | Helper class for generically "splitting" and "joining" 'BVar's into
-- constructors.  See 'Numeric.Backprop.splitBV' and
-- 'Numeric.Backprop.joinBV'.
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
    gsplitBV :: Rec AddFunc as -> Rec ZeroFunc as -> BVar s (i ()) -> o ()
    -- | Helper method for generically "joining" 'BVar's inside
    -- a constructor into a 'BVar'.  See 'joinBV'.
    gjoinBV  :: Rec AddFunc as -> Rec ZeroFunc as -> o () -> BVar s (i ())

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
         , RecApplicative as
         ) => BVGroup s (i1 () ': i2 () ': cs) (i1 :*: i2) (o1 :*: o2) where
    gsplitBV (afa :& afb :& afs) (zfa :& zfb :& zfs) xy = x :*: y
      where
        (afas, afbs) = splitRec afs
        (zfas, zfbs) = splitRec zfs
        zfab = ZF $ \(xx :*: yy) -> runZF zfa xx :*: runZF zfb yy
        x = gsplitBV afas zfas . viewVar afa zfab p1 $ xy
        y = gsplitBV afbs zfbs . viewVar afb zfab p2 $ xy
    {-# INLINE gsplitBV #-}
    gjoinBV (afa :& afb :& afs) (_ :& _ :& zfs) (x :*: y)
        = isoVar2 afa afb (:*:) unP
            (gjoinBV afas zfas x)
            (gjoinBV afbs zfbs y)
      where
        (afas, afbs) = splitRec afs
        (zfas, zfbs) = splitRec zfs
        unP (xx :*: yy) = (xx, yy)
    {-# INLINE gjoinBV #-}

-- | This instance is possible but it is not clear when it would be useful
instance ( Reifies s W
         , BVGroup s as i1 o1
         , BVGroup s bs i2 o2
         , cs ~ (as ++ bs)
         , RecApplicative as
         ) => BVGroup s (i1 () ': i2 () ': cs) (i1 :+: i2) (o1 :+: o2) where
    gsplitBV (afa :& afb :& afs) (zfa :& zfb :& zfs) xy =
        case previewVar afa zf s1 xy of
          Just x -> L1 $ gsplitBV afas zfas x
          Nothing -> case previewVar afb zf s2 xy of
            Just y -> R1 $ gsplitBV afbs zfbs y
            Nothing -> error "Numeric.Backprop.gsplitBV: Internal error occurred"
      where
        zf = ZF $ \case
            L1 xx -> L1 $ runZF zfa xx
            R1 yy -> R1 $ runZF zfb yy
        (afas, afbs) = splitRec afs
        (zfas, zfbs) = splitRec zfs
    {-# INLINE gsplitBV #-}
    gjoinBV (afa :& afb :& afs) (zfa :& zfb :& zfs) = \case
        L1 x -> liftOp1 afa (op1 (\xx -> (L1 xx, \case L1 d -> d; R1 _ -> runZF zfa xx)))
                    (gjoinBV afas zfas x)
        R1 y -> liftOp1 afb (op1 (\yy -> (R1 yy, \case L1 _ -> runZF zfb yy; R1 d -> d)))
                    (gjoinBV afbs zfbs y)
      where
        (afas, afbs) = splitRec afs
        (zfas, zfbs) = splitRec zfs
    {-# INLINE gjoinBV #-}

-- | 'Numeric.Backprop.splitBV' with explicit 'add' and 'zero'.
--
-- @since 0.2.2.0
splitBV
    :: forall z f s as.
       ( Generic (z f)
       , Generic (z (BVar s))
       , BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
       , Reifies s W
       )
    => AddFunc (Rep (z f) ())
    -> Rec AddFunc as
    -> ZeroFunc (z f)
    -> Rec ZeroFunc as
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
    :: forall z f s as.
       ( Generic (z f)
       , Generic (z (BVar s))
       , BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
       , Reifies s W
       )
    => AddFunc (z f)
    -> Rec AddFunc as
    -> ZeroFunc (Rep (z f) ())
    -> Rec ZeroFunc as
    -> z (BVar s)           -- ^ 'BVar's of fields
    -> BVar s (z f)         -- ^ 'BVar' of combined value
joinBV af afs zf zfs =
        viewVar af zf (lens G.to (const from))
      . gjoinBV afs zfs
      . from @(z (BVar s)) @()
{-# INLINE joinBV #-}
