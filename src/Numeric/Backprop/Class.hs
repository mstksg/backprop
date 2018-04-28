{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}

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
    one  :: a -> a
    add  :: a -> a -> a

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

-- instance Zero Float
-- instance Zero Double

-- instance Add Float
-- instance Add Double

-- instance One Float
-- instance One Double

-- instance Zero a => Zero (V.Vector a) where
--     zero = zeroVec
-- instance (VU.Unbox a, Zero a) => Zero (VU.Vector a) where
--     zero = zeroVec
-- instance (VS.Storable a, Zero a) => Zero (VS.Vector a) where
--     zero = zeroVec
-- instance (VP.Prim a, Zero a) => Zero (VP.Vector a) where
--     zero = zeroVec

-- instance Add a => Add (V.Vector a) where
--     add = addVec
-- instance (VU.Unbox a, Add a) => Add (VU.Vector a) where
--     add = addVec
-- instance (VS.Storable a, Add a) => Add (VS.Vector a) where
--     add = addVec
-- instance (VP.Prim a, Add a) => Add (VP.Vector a) where
--     add = addVec

-- instance One a => One (V.Vector a) where
--     one = oneVec
-- instance (VU.Unbox a, One a) => One (VU.Vector a) where
--     one = oneVec
-- instance (VS.Storable a, One a) => One (VS.Vector a) where
--     one = oneVec
-- instance (VP.Prim a, One a) => One (VP.Vector a) where
--     one = oneVec

-- instance Zero a => Zero [a] where
--     zero = zeroFunctor
-- instance Zero a => Zero (NonEmpty a) where
--     zero = zeroFunctor

-- instance Add a => Add [a] where
--     add = addIsList
-- instance Add a => Add (NonEmpty a) where
--     add = addIsList

-- instance One a => One [a] where
--     one = oneFunctor
-- instance One a => One (NonEmpty a) where
--     one = oneFunctor

-- instance (Zero a, Zero b) => Zero (a, b) where
--     zero (x, y) = (zero x, zero y)
-- instance (Zero a, Zero b, Zero c) => Zero (a, b, c) where
--     zero (x, y, z) = (zero x, zero y, zero z)

-- instance (Add a, Add b) => Add (a, b) where
--     add (x1, y1) (x2, y2) = (add x1 x2, add y1 y2)
-- instance (Add a, Add b, Add c) => Add (a, b, c) where
--     add (x1, y1, z1) (x2, y2, z2) = (add x1 x2, add y1 y2, add z1 z2)

-- instance (One a, One b) => One (a, b) where
--     one (x, y) = (one x, one y)
-- instance (One a, One b, One c) => One (a, b, c) where
--     one (x, y, z) = (one x, one y, one z)
