{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Numeric.Backprop.Class (
  -- * Backpropagatable types
    Backprop(..)
  -- * Derived methods
  , zeroNum, addNum, oneNum
  , zeroVec, addVec, oneVec
  , zeroFunctor, addIsList, addAsList, oneFunctor
  , genericZero, genericAdd, genericOne
  -- * Usable by /backprop/
  , ZeroFunc(..), zeroFunc, zeroFuncs, zfNum, zfNums
  , AddFunc(..), addFunc, addFuncs, afNum, afNums
  , OneFunc(..), oneFunc, oneFuncs, ofNum, ofNums
  -- * Generics
  , GZero(..), GAdd(..), GOne(..)
  ) where

import           Data.Complex
import           Data.List.NonEmpty       (NonEmpty(..))
import           Data.Maybe
import           Data.Ratio
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
-- Should be idempotent: Applying the function twice is the same as
-- applying it just once.
--
-- Each type should ideally only have one 'ZeroFunc'.  This coherence
-- constraint is given by the typeclass 'Zero'.
newtype ZeroFunc a = ZF { runZF :: a -> a }

-- | Add together two values of a type.  To combine contributions of
-- gradients, so should ideally be information-preserving.
--
-- See laws for 'Backprop' for the laws this should be expected to
-- preserve.  Namely, it should be commutative and associative, with an
-- identity for a valid 'ZeroFunc'.
--
-- Each type should ideally only have one 'AddFunc'.  This coherence
-- constraint is given by the typeclass 'Add'.
newtype AddFunc  a = AF { runAF :: a -> a -> a }

-- | "One" all components of a value.  For scalar values, this should
-- just be @'const' 1@.  For vectors and matrices, this should set all
-- components to one, the multiplicative identity.
--
-- Should be idempotent: Applying the function twice is the same as
-- applying it just once.
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

-- | Class of values that can be backpropagated in general.
--
-- For instances of 'Num', these methods can be given by 'zeroNum',
-- 'addNum', and 'oneNum'.  There are also generic options given in
-- "Numeric.Backprop.Class" for functors, 'IsList' instances, and 'Generic'
-- instances.
--
-- If you leave the body of an instance declaration blank, GHC Generics
-- will be used to derive instances if the type has a single constructor
-- and each field is an instance of 'Backprop'.
--
-- To ensure that backpropagation works in a sound way, should obey the
-- laws:
--
-- [/identity/]
--
--   * @'add' x ('zero' y) = x@
--
--   * @'add' ('zero' x) y = y@
--
-- [/commutativity/]
--
--   * @'add' x y = 'add' y x@
--
-- [/associativity/]
--
--   * @'add' x ('add' y z) = 'add' ('add' x y) z@
--
-- [/idempotence/]
-- 
--   * @'zero' '.' 'zero' = 'zero'@
--
--   * @'one' '.' 'one' = 'one'@
--
-- Note that not all values in the backpropagation process needs all of
-- these methods: Only the "final result" needs 'one', for example.  These
-- are all grouped under one typeclass for convenience in defining
-- instances, and also to talk about sensible laws.  For fine-grained
-- control, use the "explicit" versions of library functions (for example,
-- in "Numeric.Backprop.Explicit") instead of 'Backprop' based ones.
--
class Backprop a where
    -- | "Zero out" all components of a value.  For scalar values, this
    -- should just be @'const' 0@.  For vectors and matrices, this should
    -- set all components to zero, the additive identity.
    zero :: a -> a
    -- | Add together two values of a type.  To combine contributions of
    -- gradients, so should ideally be information-preserving.
    add  :: a -> a -> a
    -- | "One" all components of a value.  For scalar values, this should
    -- just be @'const' 1@.  For vectors and matrices, this should set all
    -- components to one, the multiplicative identity.
    one  :: a -> a

    default zero :: (Generic a, GZero (Rep a)) => a -> a
    zero = genericZero
    {-# INLINE zero #-}
    default add :: (Generic a, GAdd (Rep a)) => a -> a -> a
    add = genericAdd
    {-# INLINE add #-}
    default one :: (Generic a, GOne (Rep a)) => a -> a
    one = genericOne
    {-# INLINE one #-}

genericZero :: (Generic a, GZero (Rep a)) => a -> a
genericZero = to . gzero . from
{-# INLINE genericZero #-}

genericAdd :: (Generic a, GAdd (Rep a)) => a -> a -> a
genericAdd x y = to $ gadd (from x) (from y)
{-# INLINE genericAdd #-}

genericOne :: (Generic a, GOne (Rep a)) => a -> a
genericOne = to . gone . from
{-# INLINE genericOne #-}

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
{-# INLINE zeroVec #-}

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
{-# INLINE oneVec #-}

zeroFunctor :: (Functor f, Backprop a) => f a -> f a
zeroFunctor = fmap zero
{-# INLINE zeroFunctor #-}

addIsList :: (IsList a, Backprop (Item a)) => a -> a -> a
addIsList = addAsList toList fromList
{-# INLINE addIsList #-}

addAsList :: Backprop b => (a -> [b]) -> ([b] -> a) -> a -> a -> a
addAsList f g x y = g $ go (f x) (f y)
  where
    go = \case
      [] -> id
      o@(x':xs) -> \case
        []    -> o
        y':ys -> add x' y' : go xs ys

oneFunctor :: (Functor f, Backprop a) => f a -> f a
oneFunctor = fmap one
{-# INLINE oneFunctor #-}






class GZero f where
    gzero :: f t -> f t

instance Backprop a => GZero (K1 i a) where
    gzero (K1 x) = K1 (zero x)
    {-# INLINE gzero #-}

instance (GZero f, GZero g) => GZero (f :*: g) where
    gzero (x :*: y) = gzero x :*: gzero y
    {-# INLINE gzero #-}

instance (GZero f, GZero g) => GZero (f :+: g) where
    gzero (L1 x) = L1 (gzero x)
    gzero (R1 x) = R1 (gzero x)
    {-# INLINE gzero #-}

instance GZero V1 where
    gzero = \case
    {-# INLINE gzero #-}

instance GZero U1 where
    gzero _ = U1
    {-# INLINE gzero #-}

instance GZero f => GZero (M1 i c f) where
    gzero (M1 x) = M1 (gzero x)
    {-# INLINE gzero #-}

instance GZero f => GZero (f :.: g) where
    gzero (Comp1 x) = Comp1 (gzero x)
    {-# INLINE gzero #-}


class GAdd f where
    gadd :: f t -> f t -> f t

instance Backprop a => GAdd (K1 i a) where
    gadd (K1 x) (K1 y) = K1 (add x y)
    {-# INLINE gadd #-}

instance (GAdd f, GAdd g) => GAdd (f :*: g) where
    gadd (x1 :*: y1) (x2 :*: y2) = gadd x1 x2 :*: gadd y1 y2
    {-# INLINE gadd #-}

instance GAdd V1 where
    gadd = \case
    {-# INLINE gadd #-}

instance GAdd U1 where
    gadd _ _ = U1
    {-# INLINE gadd #-}

instance GAdd f => GAdd (M1 i c f) where
    gadd (M1 x) (M1 y) = M1 (gadd x y)
    {-# INLINE gadd #-}

instance GAdd f => GAdd (f :.: g) where
    gadd (Comp1 x) (Comp1 y) = Comp1 (gadd x y)
    {-# INLINE gadd #-}


class GOne f where
    gone :: f t -> f t

instance Backprop a => GOne (K1 i a) where
    gone (K1 x) = K1 (one x)
    {-# INLINE gone #-}

instance (GOne f, GOne g) => GOne (f :*: g) where
    gone (x :*: y) = gone x :*: gone y
    {-# INLINE gone #-}

instance (GOne f, GOne g) => GOne (f :+: g) where
    gone (L1 x) = L1 (gone x)
    gone (R1 x) = R1 (gone x)
    {-# INLINE gone #-}

instance GOne V1 where
    gone = \case
    {-# INLINE gone #-}

instance GOne U1 where
    gone _ = U1
    {-# INLINE gone #-}

instance GOne f => GOne (M1 i c f) where
    gone (M1 x) = M1 (gone x)
    {-# INLINE gone #-}

instance GOne f => GOne (f :.: g) where
    gone (Comp1 x) = Comp1 (gone x)
    {-# INLINE gone #-}

instance Backprop Int where
    zero = zeroNum
    {-# INLINE zero #-}
    add  = addNum
    {-# INLINE add #-}
    one  = oneNum
    {-# INLINE one #-}

instance Backprop Integer where
    zero = zeroNum
    {-# INLINE zero #-}
    add  = addNum
    {-# INLINE add #-}
    one  = oneNum
    {-# INLINE one #-}

instance Integral a => Backprop (Ratio a) where
    zero = zeroNum
    {-# INLINE zero #-}
    add  = addNum
    {-# INLINE add #-}
    one  = oneNum
    {-# INLINE one #-}

instance RealFloat a => Backprop (Complex a) where
    zero = zeroNum
    {-# INLINE zero #-}
    add  = addNum
    {-# INLINE add #-}
    one  = oneNum
    {-# INLINE one #-}

instance Backprop Float where
    zero = zeroNum
    {-# INLINE zero #-}
    add  = addNum
    {-# INLINE add #-}
    one  = oneNum
    {-# INLINE one #-}

instance Backprop Double where
    zero = zeroNum
    {-# INLINE zero #-}
    add  = addNum
    {-# INLINE add #-}
    one  = oneNum
    {-# INLINE one #-}

instance Backprop a => Backprop (V.Vector a) where
    zero = zeroVec
    {-# INLINE zero #-}
    add  = addVec
    {-# INLINE add #-}
    one  = oneVec
    {-# INLINE one #-}

instance (VU.Unbox a, Backprop a) => Backprop (VU.Vector a) where
    zero = zeroVec
    {-# INLINE zero #-}
    add  = addVec
    {-# INLINE add #-}
    one  = oneVec
    {-# INLINE one #-}

instance (VS.Storable a, Backprop a) => Backprop (VS.Vector a) where
    zero = zeroVec
    {-# INLINE zero #-}
    add  = addVec
    {-# INLINE add #-}
    one  = oneVec
    {-# INLINE one #-}

instance (VP.Prim a, Backprop a) => Backprop (VP.Vector a) where
    zero = zeroVec
    {-# INLINE zero #-}
    add  = addVec
    {-# INLINE add #-}
    one  = oneVec
    {-# INLINE one #-}

instance Backprop a => Backprop [a] where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = addIsList
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

instance Backprop a => Backprop (NonEmpty a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = addIsList
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

instance Backprop a => Backprop (Maybe a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = addAsList maybeToList listToMaybe
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

instance (Backprop a, Backprop b) => Backprop (a, b) where
    zero (x, y) = (zero x, zero y)
    {-# INLINE zero #-}
    add (x1, y1) (x2, y2) = (add x1 x2, add y1 y2)
    {-# INLINE add #-}
    one (x, y) = (one x, one y)
    {-# INLINE one #-}

instance (Backprop a, Backprop b, Backprop c) => Backprop (a, b, c) where
    zero (x, y, z) = (zero x, zero y, zero z)
    {-# INLINE zero #-}
    add (x1, y1, z1) (x2, y2, z2) = (add x1 x2, add y1 y2, add z1 z2)
    {-# INLINE add #-}
    one (x, y, z) = (one x, one y, one z)
    {-# INLINE one #-}

instance (Backprop a, Backprop b, Backprop c, Backprop d) => Backprop (a, b, c, d) where
    zero (x, y, z, w) = (zero x, zero y, zero z, zero w)
    {-# INLINE zero #-}
    add (x1, y1, z1, w1) (x2, y2, z2, w2) = (add x1 x2, add y1 y2, add z1 z2, add w1 w2)
    {-# INLINE add #-}
    one (x, y, z, w) = (one x, one y, one z, one w)
    {-# INLINE one #-}
