{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}

module Numeric.Backprop.Class (
    Zero(..), ZeroFunc(..), zeroFunc, zeroFuncs
  , Add(..), AddFunc(..), addFunc, addFuncs
  , One(..), OneFunc(..), oneFunc, oneFuncs
  , zeroVec, addVec, oneVec
  ) where

import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Primitive     as VP
import qualified Data.Vector.Storable      as VS
import qualified Data.Vector.Unboxed       as VU

class Zero a where
    zero :: a -> a

    default zero :: Num a => a -> a
    zero _ = 0

zeroFunc :: Zero a => ZeroFunc a
zeroFunc = ZF zero

zeroFuncs :: (Every Zero as, Known Length as) => Prod ZeroFunc as
zeroFuncs = map1 (\i -> zeroFunc \\ every @_ @Zero i) indices

class Add a where
    add :: a -> a -> a

    default add :: Num a => a -> a -> a
    add = (+)

addFunc :: Add a => AddFunc a
addFunc = AF add

addFuncs :: (Every Add as, Known Length as) => Prod AddFunc as
addFuncs = map1 (\i -> addFunc \\ every @_ @Add i) indices

class One a where
    one :: a -> a

    default one :: Num a => a -> a
    one _ = 1

oneFunc :: One a => OneFunc a
oneFunc = OF one

oneFuncs :: (Every One as, Known Length as) => Prod OneFunc as
oneFuncs = map1 (\i -> oneFunc \\ every @_ @One i) indices

instance Zero Float
instance Zero Double

instance Add Float
instance Add Double

instance One Float
instance One Double

zeroVec :: (VG.Vector v a, Zero a) => v a -> v a
zeroVec = VG.map zero

addVec :: (VG.Vector v a, Num a) => v a -> v a -> v a
addVec x y = case compare lX lY of
    LT -> VG.zipWith (+) (x VG.++ VG.replicate (lY - lX) 0) y
    EQ -> VG.zipWith (+) x y
    GT -> VG.zipWith (+) x (y VG.++ VG.replicate (lX - lY) 0)
  where
    lX = VG.length x
    lY = VG.length y

oneVec :: (VG.Vector v a, One a) => v a -> v a
oneVec = VG.map one

instance Zero a => Zero (V.Vector a) where
    zero = zeroVec
instance (VU.Unbox a, Zero a) => Zero (VU.Vector a) where
    zero = zeroVec
instance (VS.Storable a, Zero a) => Zero (VS.Vector a) where
    zero = zeroVec
instance (VP.Prim a, Zero a) => Zero (VP.Vector a) where
    zero = zeroVec

instance Num a => Add (V.Vector a) where
    add = addVec
instance (VU.Unbox a, Num a) => Add (VU.Vector a) where
    add = addVec
instance (VS.Storable a, Num a) => Add (VS.Vector a) where
    add = addVec
instance (VP.Prim a, Num a) => Add (VP.Vector a) where
    add = addVec

instance One a => One (V.Vector a) where
    one = oneVec
instance (VU.Unbox a, One a) => One (VU.Vector a) where
    one = oneVec
instance (VS.Storable a, One a) => One (VS.Vector a) where
    one = oneVec
instance (VP.Prim a, One a) => One (VP.Vector a) where
    one = oneVec
