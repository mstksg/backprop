{-# LANGUAGE DefaultSignatures #-}

module Numeric.Backprop.Class (
    Zero(..)
  , Scale(..)
  , ScaleFunc(..)
  , scaleFunc
  ) where

import           Numeric.Backprop.Internal

class Zero a where
    zero :: a -> a

    default zero :: Num a => a -> a
    zero _ = 0

class Scale a where
    scale :: a -> a -> a

    default scale :: Num a => a -> a -> a
    scale = (+)

scaleFunc :: Scale a => ScaleFunc a
scaleFunc = SF scale
