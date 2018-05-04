{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      : Numeric.Backprop.Class
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the 'Backprop' typeclass, a class for values that can be used
-- for backpropagation.
--
-- This class replaces the old (version 0.1) API relying on 'Num'.
--
-- @since 0.2.0.0

module Numeric.Backprop.Class (
  -- * Backpropagatable types
    Backprop(..)
  -- * Derived methods
  , zeroNum, addNum, oneNum
  , zeroVec, addVec, oneVec
  , zeroFunctor, addIsList, addAsList, oneFunctor
  , genericZero, genericAdd, genericOne
  -- * Newtype
  , ABP(..), NumBP(..)
  -- * Generics
  , GZero(..), GAdd(..), GOne(..)
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Coerce
import           Data.Complex
import           Data.Data
import           Data.Foldable hiding        (toList)
import           Data.Functor.Identity
import           Data.List.NonEmpty          (NonEmpty(..))
import           Data.Ratio
import           Data.Type.Combinator hiding ((:.:), Comp1)
import           Data.Type.Option
import           Data.Type.Product hiding    (toList)
import           Data.Void
import           GHC.Exts
import           GHC.Generics
import           Numeric.Natural
import           Type.Family.List
import qualified Data.IntMap                 as IM
import qualified Data.Map                    as M
import qualified Data.Sequence               as Seq
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Primitive       as VP
import qualified Data.Vector.Storable        as VS
import qualified Data.Vector.Unboxed         as VU
import qualified Type.Family.Maybe           as M

-- | Class of values that can be backpropagated in general.
--
-- For instances of 'Num', these methods can be given by 'zeroNum',
-- 'addNum', and 'oneNum'.  There are also generic options given in
-- "Numeric.Backprop.Class" for functors, 'IsList' instances, and 'Generic'
-- instances.
--
-- @
-- instance 'Backprop' 'Double' where
--     'zero' = 'zeroNum'
--     'add' = 'addNum'
--     'one' = 'oneNum'
-- @
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
-- Also implies preservation of information, making @'zipWith' ('+')@ an
-- illegal implementation for lists and vectors.
--
-- This is only expected to be true up to potential "extra zeroes" in @x@
-- and @y@ in the result.
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
-- This typeclass replaces the reliance on 'Num' of the previous API
-- (v0.1).  'Num' is strictly more powerful than 'Backprop', and is
-- a stronger constraint on types than is necessary for proper
-- backpropagating.  In particular, 'fromInteger' is a problem for many
-- types, preventing useful backpropagation for lists, variable-length
-- vectors (like "Data.Vector") and variable-size matrices from linear
-- algebra libraries like /hmatrix/ and /accelerate/.
--
-- @since 0.2.0.0
class Backprop a where
    -- | "Zero out" all components of a value.  For scalar values, this
    -- should just be @'const' 0@.  For vectors and matrices, this should
    -- set all components to zero, the additive identity.
    --
    -- Should be idempotent:
    --
    --   * @'zero' '.' 'zero' = 'zero'@
    --
    -- Should be as /lazy/ as possible.  This behavior is observed for
    -- all instances provided by this library.
    --
    -- See 'zeroNum' for a pre-built definition for instances of 'Num' and
    -- 'zeroFunctor' for a definition for instances of 'Functor'.  If left
    -- blank, will automatically be 'genericZero', a pre-built definition
    -- for instances of 'GHC.Generic' whose fields are all themselves
    -- instances of 'Backprop'.
    zero :: a -> a
    -- | Add together two values of a type.  To combine contributions of
    -- gradients, so should be information-preserving:
    --
    --   * @'add' x ('zero' y) = x@
    --
    --   * @'add' ('zero' x) y = y@
    --
    -- Should be as /strict/ as possible.  This behavior is observed for
    -- all instances provided by this library.
    --
    -- See 'addNum' for a pre-built definition for instances of 'Num' and
    -- 'addFunctor' for a definition for instances of 'Functor'.  If left
    -- blank, will automatically be 'genericAdd', a pre-built definition
    -- for instances of 'GHC.Generic' with one constructor whose fields are
    -- all themselves instances of 'Backprop'.
    add  :: a -> a -> a
    -- | "One" all components of a value.  For scalar values, this should
    -- just be @'const' 1@.  For vectors and matrices, this should set all
    -- components to one, the multiplicative identity.
    --
    -- Should be idempotent:
    --
    --   * @'one' '.' 'one' = 'one'@
    --
    -- Should be as /lazy/ as possible.  This behavior is observed for
    -- all instances provided by this library.
    --
    -- See 'oneNum' for a pre-built definition for instances of 'Num' and
    -- 'oneFunctor' for a definition for instances of 'Functor'.  If left
    -- blank, will automatically be 'genericOne', a pre-built definition
    -- for instances of 'GHC.Generic' whose fields are all themselves
    -- instances of 'Backprop'.
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

-- | 'zero' using GHC Generics; works if all fields are instances of
-- 'Backprop'.
genericZero :: (Generic a, GZero (Rep a)) => a -> a
genericZero = to . gzero . from
{-# INLINE genericZero #-}

-- | 'add' using GHC Generics; works if all fields are instances of
-- 'Backprop', but only for values with single constructors.
genericAdd :: (Generic a, GAdd (Rep a)) => a -> a -> a
genericAdd x y = to $ gadd (from x) (from y)
{-# INLINE genericAdd #-}

-- | 'one' using GHC Generics; works if all fields are instaces of
-- 'Backprop'.
genericOne :: (Generic a, GOne (Rep a)) => a -> a
genericOne = to . gone . from
{-# INLINE genericOne #-}

-- | 'zero' for instances of 'Num'.
--
-- Is lazy in its argument.
zeroNum :: Num a => a -> a
zeroNum _ = 0
{-# INLINE zeroNum #-}

-- | 'add' for instances of 'Num'.
addNum :: Num a => a -> a -> a
addNum = (+)
{-# INLINE addNum #-}

-- | 'one' for instances of 'Num'.
--
-- Is lazy in its argument.
oneNum :: Num a => a -> a
oneNum _ = 1
{-# INLINE oneNum #-}

-- | 'zero' for instances of 'VG.Vector'.
zeroVec :: (VG.Vector v a, Backprop a) => v a -> v a
zeroVec = VG.map zero
{-# INLINE zeroVec #-}

-- | 'add' for instances of 'VG.Vector'.  Automatically pads the end of the
-- shorter vector with zeroes.
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

-- | 'one' for instances of 'VG.Vector'.
oneVec :: (VG.Vector v a, Backprop a) => v a -> v a
oneVec = VG.map one
{-# INLINE oneVec #-}

-- | 'zero' for 'Functor' instances.
zeroFunctor :: (Functor f, Backprop a) => f a -> f a
zeroFunctor = fmap zero
{-# INLINE zeroFunctor #-}

-- | 'add' for instances of 'IsList'.  Automatically pads the end of the
-- "shorter" value with zeroes.
addIsList :: (IsList a, Backprop (Item a)) => a -> a -> a
addIsList = addAsList toList fromList
{-# INLINE addIsList #-}

-- | 'add' for types that are isomorphic to a list.
-- Automatically pads the end of the "shorter" value with zeroes.
addAsList
    :: Backprop b
    => (a -> [b])       -- ^ convert to list (should form isomorphism)
    -> ([b] -> a)       -- ^ convert from list (should form isomorphism)
    -> a
    -> a
    -> a
addAsList f g x y = g $ go (f x) (f y)
  where
    go = \case
      [] -> id
      o@(x':xs) -> \case
        []    -> o
        y':ys -> add x' y' : go xs ys

-- | 'one' for instances of 'Functor'.
oneFunctor :: (Functor f, Backprop a) => f a -> f a
oneFunctor = fmap one
{-# INLINE oneFunctor #-}

-- | A newtype wrapper over an instance of 'Num' that gives a free
-- 'Backprop' instance.
--
-- Useful for things like /DerivingVia/, or for avoiding orphan instances.
--
-- @since 0.2.1.0
newtype NumBP a = NumBP { runNumBP :: a }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable, Num, Fractional, Floating)

instance NFData a => NFData (NumBP a)

instance Applicative NumBP where
    pure    = NumBP
    f <*> x = NumBP $ (runNumBP f) (runNumBP x)

instance Monad NumBP where
    return = NumBP
    x >>= f = f (runNumBP x)

instance Num a => Backprop (NumBP a) where
    zero = coerce (zeroNum :: a -> a)
    add = coerce (addNum :: a -> a -> a)
    one = coerce (oneNum :: a -> a)

-- | A newtype wrapper over an @f a@ for @'Applicative' f@ that gives
-- a free 'Backprop' instance (as well as 'Num' etc. instances).
--
-- Useful for performing backpropagation over functions that require some
-- monadic context (like 'IO') to perform.
--
-- @since 0.2.1.0
newtype ABP f a = ABP { runABP :: f a }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance NFData (f a) => NFData (ABP f a)

instance Applicative f => Applicative (ABP f) where
    pure = ABP . pure
    f <*> x = ABP $ ($) <$> runABP f <*> runABP x

instance Monad m => Monad (ABP m) where
    return = ABP . return
    x >>= f = ABP $ do
      x' <- runABP x
      runABP $ f x'

instance (Applicative f, Backprop a) => Backprop (ABP f a) where
    zero = fmap zero
    {-# INLINE zero #-}
    add  = liftA2 add
    {-# INLINE add #-}
    one  = fmap one
    {-# INLINE one #-}

instance (Applicative f, Num a) => Num (ABP f a) where
    (+) = liftA2 (+)
    {-# INLINE (+) #-}
    (-) = liftA2 (-)
    {-# INLINE (-) #-}
    (*) = liftA2 (*)
    {-# INLINE (*) #-}
    negate = fmap negate
    {-# INLINE negate #-}
    abs = fmap abs
    {-# INLINE abs #-}
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

instance (Applicative f, Fractional a) => Fractional (ABP f a) where
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    recip = fmap recip
    {-# INLINE recip #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

instance (Applicative f, Floating a) => Floating (ABP f a) where
    pi  = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    log = fmap log
    {-# INLINE log #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    cos = fmap cos
    {-# INLINE cos #-}
    tan = fmap tan
    {-# INLINE tan #-}
    asin = fmap asin
    {-# INLINE asin #-}
    acos = fmap acos
    {-# INLINE acos #-}
    atan = fmap atan
    {-# INLINE atan #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}


-- | Helper class for automatically deriving 'zero' using GHC Generics.
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
    gzero = \case {}
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


-- | Helper class for automatically deriving 'add' using GHC Generics.
class GAdd f where
    gadd :: f t -> f t -> f t

instance Backprop a => GAdd (K1 i a) where
    gadd (K1 x) (K1 y) = K1 (add x y)
    {-# INLINE gadd #-}

instance (GAdd f, GAdd g) => GAdd (f :*: g) where
    gadd (x1 :*: y1) (x2 :*: y2) = x3 :*: y3
      where
        !x3 = gadd x1 x2
        !y3 = gadd y1 y2
    {-# INLINE gadd #-}

instance GAdd V1 where
    gadd = \case {}
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


-- | Helper class for automatically deriving 'one' using GHC Generics.
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
    gone = \case {}
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

-- | @since 0.2.1.0
instance Backprop Natural where
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

-- | 'add' assumes the shorter list has trailing zeroes, and the result has
-- the length of the longest input.
instance Backprop a => Backprop [a] where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = addIsList
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

-- | 'add' assumes the shorter list has trailing zeroes, and the result has
-- the length of the longest input.
instance Backprop a => Backprop (NonEmpty a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = addIsList
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

-- | 'add' assumes the shorter sequence has trailing zeroes, and the result
-- has the length of the longest input.
instance Backprop a => Backprop (Seq.Seq a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = addIsList
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

-- | 'Nothing' is treated the same as @'Just' 0@.  However, 'zero', 'add',
-- and 'one' preserve 'Nothing' if all inputs are also 'Nothing'.
instance Backprop a => Backprop (Maybe a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add x y = asum [ add <$> x <*> y
                   , x
                   , y
                   ]
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

-- | 'add' is strict, but 'zero' and 'one' are lazy in their arguments.
instance Backprop () where
    zero _ = ()
    add () () = ()
    one _ = ()

-- | 'add' is strict
instance (Backprop a, Backprop b) => Backprop (a, b) where
    zero (x, y) = (zero x, zero y)
    {-# INLINE zero #-}
    add (x1, y1) (x2, y2) = (x3, y3)
      where
        !x3 = add x1 x2
        !y3 = add y1 y2
    {-# INLINE add #-}
    one (x, y) = (one x, one y)
    {-# INLINE one #-}

-- | 'add' is strict
instance (Backprop a, Backprop b, Backprop c) => Backprop (a, b, c) where
    zero (x, y, z) = (zero x, zero y, zero z)
    {-# INLINE zero #-}
    add (x1, y1, z1) (x2, y2, z2) = (x3, y3, z3)
      where
        !x3 = add x1 x2
        !y3 = add y1 y2
        !z3 = add z1 z2
    {-# INLINE add #-}
    one (x, y, z) = (one x, one y, one z)
    {-# INLINE one #-}

-- | 'add' is strict
instance (Backprop a, Backprop b, Backprop c, Backprop d) => Backprop (a, b, c, d) where
    zero (x, y, z, w) = (zero x, zero y, zero z, zero w)
    {-# INLINE zero #-}
    add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x3, y3, z3, w3)
      where
        !x3 = add x1 x2
        !y3 = add y1 y2
        !z3 = add z1 z2
        !w3 = add w1 w2
    {-# INLINE add #-}
    one (x, y, z, w) = (one x, one y, one z, one w)
    {-# INLINE one #-}

-- | 'add' is strict
instance (Backprop a, Backprop b, Backprop c, Backprop d, Backprop e) => Backprop (a, b, c, d, e) where
    zero (x, y, z, w, v) = (zero x, zero y, zero z, zero w, zero v)
    {-# INLINE zero #-}
    add (x1, y1, z1, w1, v1) (x2, y2, z2, w2, v2) = (x3, y3, z3, w3, v3)
      where
        !x3 = add x1 x2
        !y3 = add y1 y2
        !z3 = add z1 z2
        !w3 = add w1 w2
        !v3 = add v1 v2
    {-# INLINE add #-}
    one (x, y, z, w, v) = (one x, one y, one z, one w, one v)
    {-# INLINE one #-}

instance Backprop a => Backprop (Identity a) where
    zero (Identity x) = Identity (zero x)
    {-# INLINE zero #-}
    add (Identity x) (Identity y) = Identity (add x y)
    {-# INLINE add #-}
    one (Identity x) = Identity (one x)
    {-# INLINE one #-}

instance Backprop a => Backprop (I a) where
    zero (I x) = I (zero x)
    {-# INLINE zero #-}
    add (I x) (I y) = I (add x y)
    {-# INLINE add #-}
    one (I x) = I (one x)
    {-# INLINE one #-}

-- | 'add' is strict, but 'zero' and 'one' are lazy in their arguments.
instance Backprop (Proxy a) where
    zero _ = Proxy
    {-# INLINE zero #-}
    add Proxy Proxy = Proxy
    {-# INLINE add #-}
    one _ = Proxy
    {-# INLINE one #-}

instance Backprop Void where
    zero = \case {}
    {-# INLINE zero #-}
    add = \case {}
    {-# INLINE add #-}
    one = \case {}
    {-# INLINE one #-}

-- | 'zero' and 'one' replace all current values, and 'add' merges keys
-- from both maps, adding in the case of double-occurrences.
instance (Backprop a, Ord k) => Backprop (M.Map k a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = M.unionWith add
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

-- | 'zero' and 'one' replace all current values, and 'add' merges keys
-- from both maps, adding in the case of double-occurrences.
instance (Backprop a) => Backprop (IM.IntMap a) where
    zero = zeroFunctor
    {-# INLINE zero #-}
    add  = IM.unionWith add
    {-# INLINE add #-}
    one  = oneFunctor
    {-# INLINE one #-}

instance ListC (Backprop <$> (f <$> as)) => Backprop (Prod f as) where
    zero = \case
      Ø -> Ø
      x :< xs -> zero x :< zero xs
    {-# INLINE zero #-}
    add = \case
      Ø -> \case
        Ø -> Ø
      x :< xs -> \case
        y :< ys -> add x y :< add xs ys
    {-# INLINE add #-}
    one = \case
      Ø       -> Ø
      x :< xs -> one x :< one xs
    {-# INLINE one #-}

instance M.MaybeC (Backprop M.<$> (f M.<$> a)) => Backprop (Option f a) where
    zero = \case
      Nothing_ -> Nothing_
      Just_ x  -> Just_ (zero x)
    {-# INLINE zero #-}
    add = \case
      Nothing_ -> \case
        Nothing_ -> Nothing_
      Just_ x -> \case
        Just_ y -> Just_ (add x y)
    {-# INLINE add #-}
    one = \case
      Nothing_ -> Nothing_
      Just_ x  -> Just_ (one x)
    {-# INLINE one #-}

