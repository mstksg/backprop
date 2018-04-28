{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}

module Numeric.Backprop.Class (
    Zero(..), ZeroFunc(..), zeroFunc, zeroFuncs
  , Add(..), AddFunc(..), addFunc, addFuncs
  , One(..), OneFunc(..), oneFunc, oneFuncs
  ) where

import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

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

