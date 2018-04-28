{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Numeric.Backprop.Class (
    Backprop(..)
  , zeroNum, addNum, oneNum
  , zeroVec, addVec, oneVec
  , zeroFunctor, addIsList, oneFunctor
  , ZeroFunc(..), zeroFunc, zeroFuncs, zfNum, zfNums
  , AddFunc(..), addFunc, addFuncs, afNum, afNums
  , OneFunc(..), oneFunc, oneFuncs, ofNum, ofNums
  ) where

import           Data.List.NonEmpty       (NonEmpty(..))
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product hiding (toList)
import           GHC.Exts
import           GHC.Generics
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Data.Vector              as V
import qualified Data.Vector.Generic      as VG
import qualified Data.Vector.Primitive    as VP
import qualified Data.Vector.Storable     as VS
import qualified Data.Vector.Unboxed      as VU

-- | "Zero out" all components of a value.  For scalar values, this should
-- just be @'const' 0@.  For vectors and matrices, this should set all
-- components to zero, the additive identity.
--
-- Each type should ideally only have one 'ZeroFunc'.  This coherence
-- constraint is given by the typeclass 'Zero'.
newtype ZeroFunc a = ZF { runZF :: a -> a }

-- | Add together two values of a type.  To combine contributions of
-- gradients, so should ideally be information-preserving.  For any other
-- valid 'ZeroFunc', should ideally obey:
--
-- @
-- \af zf x y -> 'runAF' af x ('runZF' zf y) == x
--            && 'runAF' af ('runZF' zf x) y == y
-- @
--
-- Each type should ideally only have one 'AddFunc'.  This coherence
-- constraint is given by the typeclass 'Add'.
newtype AddFunc  a = AF { runAF :: a -> a -> a }

-- | "One" all components of a value.  For scalar values, this should
-- just be @'const' 1@.  For vectors and matrices, this should set all
-- components to one, the multiplicative identity.
--
-- Each type should ideally only have one 'ZeroFunc'.  This coherence
-- constraint is given by the typeclass 'One'.
newtype OneFunc  a = OF { runOF :: a -> a }

-- | If a type has a 'Num' instance, this is the canonical 'ZeroFunc'.
zfNum :: Num a => ZeroFunc a
zfNum = ZF (const 0)
{-# INLINE zfNum #-}

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
zfNums :: (Every Num as, Known Length as) => Prod ZeroFunc as
zfNums = map1 (\i -> zfNum \\ every @_ @Num i) indices

-- | If a type has a 'Num' instance, this is the canonical 'AddFunc'.
afNum :: Num a => AddFunc a
afNum = AF (+)
{-# INLINE afNum #-}

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
afNums :: (Every Num as, Known Length as) => Prod AddFunc as
afNums = map1 (\i -> afNum \\ every @_ @Num i) indices

-- | If a type has a 'Num' instance, this is the canonical 'OneFunc'.
ofNum :: Num a => OneFunc a
ofNum = OF (const 1)
{-# INLINE ofNum #-}

-- | 'ZeroFunc's for every item in a type level list based on their
-- 'Num' instances
ofNums :: (Every Num as, Known Length as) => Prod OneFunc as
ofNums = map1 (\i -> ofNum \\ every @_ @Num i) indices

class Backprop a where
    zero :: a -> a
    add  :: a -> a -> a
    one  :: a -> a

    default zero :: (Generic a, GZero (Rep a)) => a -> a
    zero = to . gzero . from
    default add :: (Generic a, GAdd (Rep a)) => a -> a -> a
    add x y = to $ gadd (from x) (from y)
    default one :: (Generic a, GOne (Rep a)) => a -> a
    one = to . gone . from

zeroNum :: Num a => a -> a
zeroNum _ = 0
{-# INLINE zeroNum #-}

addNum :: Num a => a -> a -> a
addNum = (+)
{-# INLINE addNum #-}

oneNum :: Num a => a -> a
oneNum _ = 1
{-# INLINE oneNum #-}

zeroFunc :: Backprop a => ZeroFunc a
zeroFunc = ZF zero
{-# INLINE zeroFunc #-}

addFunc :: Backprop a => AddFunc a
addFunc = AF add
{-# INLINE addFunc #-}

oneFunc :: Backprop a => OneFunc a
oneFunc = OF one
{-# INLINE oneFunc #-}

addFuncs :: (Every Backprop as, Known Length as) => Prod AddFunc as
addFuncs = map1 (\i -> addFunc \\ every @_ @Backprop i) indices

zeroFuncs :: (Every Backprop as, Known Length as) => Prod ZeroFunc as
zeroFuncs = map1 (\i -> zeroFunc \\ every @_ @Backprop i) indices

oneFuncs :: (Every Backprop as, Known Length as) => Prod OneFunc as
oneFuncs = map1 (\i -> oneFunc \\ every @_ @Backprop i) indices

zeroVec :: (VG.Vector v a, Backprop a) => v a -> v a
zeroVec = VG.map zero

addVec :: (VG.Vector v a, Backprop a) => v a -> v a -> v a
addVec x y = case compare lX lY of
    LT -> let (y1,y2) = VG.splitAt (lY - lX) y
          in  VG.zipWith add x y1 VG.++ y2
    EQ -> VG.zipWith add x y
    GT -> let (x1,x2) = VG.splitAt (lX - lY) x
          in  VG.zipWith add x1 y VG.++ x2
  where
    lX = VG.length x
    lY = VG.length y

oneVec :: (VG.Vector v a, Backprop a) => v a -> v a
oneVec = VG.map one

zeroFunctor :: (Functor f, Backprop a) => f a -> f a
zeroFunctor = fmap zero

addIsList :: (IsList a, Backprop (Item a)) => a -> a -> a
addIsList x y = fromList $ go (toList x) (toList y)
  where
    go = \case
      [] -> id
      o@(x':xs) -> \case
        []    -> o
        y':ys -> add x' y' : go xs ys

oneFunctor :: (Functor f, Backprop a) => f a -> f a
oneFunctor = fmap one






class GZero f where
    gzero :: f t -> f t

instance Backprop a => GZero (K1 i a) where
    gzero (K1 x) = K1 (zero x)

instance (GZero f, GZero g) => GZero (f :*: g) where
    gzero (x :*: y) = gzero x :*: gzero y

instance (GZero f, GZero g) => GZero (f :+: g) where
    gzero (L1 x) = L1 (gzero x)
    gzero (R1 x) = R1 (gzero x)

instance GZero V1 where
    gzero = \case

instance GZero U1 where
    gzero _ = U1

instance GZero f => GZero (M1 i c f) where
    gzero (M1 x) = M1 (gzero x)

instance GZero f => GZero (f :.: g) where
    gzero (Comp1 x) = Comp1 (gzero x)


class GAdd f where
    gadd :: f t -> f t -> f t

instance Backprop a => GAdd (K1 i a) where
    gadd (K1 x) (K1 y) = K1 (add x y)

instance (GAdd f, GAdd g) => GAdd (f :*: g) where
    gadd (x1 :*: y1) (x2 :*: y2) = gadd x1 x2 :*: gadd y1 y2

instance GAdd V1 where
    gadd = \case

instance GAdd U1 where
    gadd _ _ = U1

instance GAdd f => GAdd (M1 i c f) where
    gadd (M1 x) (M1 y) = M1 (gadd x y)

instance GAdd f => GAdd (f :.: g) where
    gadd (Comp1 x) (Comp1 y) = Comp1 (gadd x y)


class GOne f where
    gone :: f t -> f t

instance Backprop a => GOne (K1 i a) where
    gone (K1 x) = K1 (one x)

instance (GOne f, GOne g) => GOne (f :*: g) where
    gone (x :*: y) = gone x :*: gone y

instance (GOne f, GOne g) => GOne (f :+: g) where
    gone (L1 x) = L1 (gone x)
    gone (R1 x) = R1 (gone x)

instance GOne V1 where
    gone = \case

instance GOne U1 where
    gone _ = U1

instance GOne f => GOne (M1 i c f) where
    gone (M1 x) = M1 (gone x)

instance GOne f => GOne (f :.: g) where
    gone (Comp1 x) = Comp1 (gone x)

instance Backprop Float where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance Backprop Double where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance Backprop a => Backprop (V.Vector a) where
    zero = zeroVec
    add  = addVec
    one  = oneVec

instance (VU.Unbox a, Backprop a) => Backprop (VU.Vector a) where
    zero = zeroVec
    add  = addVec
    one  = oneVec

instance (VS.Storable a, Backprop a) => Backprop (VS.Vector a) where
    zero = zeroVec
    add  = addVec
    one  = oneVec

instance (VP.Prim a, Backprop a) => Backprop (VP.Vector a) where
    zero = zeroVec
    add  = addVec
    one  = oneVec

instance Backprop a => Backprop [a] where
    zero = zeroFunctor
    add  = addIsList
    one  = oneFunctor

instance Backprop a => Backprop (NonEmpty a) where
    zero = zeroFunctor
    add  = addIsList
    one  = oneFunctor

instance (Backprop a, Backprop b) => Backprop (a, b) where
    zero (x, y) = (zero x, zero y)
    add (x1, y1) (x2, y2) = (add x1 x2, add y1 y2)
    one (x, y) = (one x, one y)

instance (Backprop a, Backprop b, Backprop c) => Backprop (a, b, c) where
    zero (x, y, z) = (zero x, zero y, zero z)
    add (x1, y1, z1) (x2, y2, z2) = (add x1 x2, add y1 y2, add z1 z2)
    one (x, y, z) = (one x, one y, one z)

instance (Backprop a, Backprop b, Backprop c, Backprop d) => Backprop (a, b, c, d) where
    zero (x, y, z, w) = (zero x, zero y, zero z, zero w)
    add (x1, y1, z1, w1) (x2, y2, z2, w2) = (add x1 x2, add y1 y2, add z1 z2, add w1 w2)
    one (x, y, z, w) = (one x, one y, one z, one w)
