{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Numeric.Backprop.Tuple (
    T2(..)
  , T3(..)
  , T4(..)
  ) where

import           Control.DeepSeq
import           Lens.Micro.Internal
import           GHC.Generics (Generic)

data T2 a b     = T2 !a !b
  deriving (Generic, Show, Read, Eq, Ord)
data T3 a b c   = T3 !a !b !c
  deriving (Generic, Show, Read, Eq, Ord)
data T4 a b c d = T4 !a !b !c !d
  deriving (Generic, Show, Read, Eq, Ord)

instance (NFData a, NFData b                    ) => NFData (T2 a b)
instance (NFData a, NFData b, NFData c          ) => NFData (T3 a b c)
instance (NFData a, NFData b, NFData c, NFData d) => NFData (T4 a b c d)

instance (Num a, Num b) => Num (T2 a b) where
    T2 x1 y1 + T2 x2 y2 = T2 (x1 + x2) (y1 + y2)
    T2 x1 y1 - T2 x2 y2 = T2 (x1 - x2) (y1 - y2)
    T2 x1 y1 * T2 x2 y2 = T2 (x1 * x2) (y1 * y2)
    negate (T2 x y)     = T2 (negate x) (negate y)
    abs    (T2 x y)     = T2 (abs x   ) (abs y   )
    signum (T2 x y)     = T2 (signum x) (signum y)
    fromInteger x       = T2 (fromInteger x) (fromInteger x)

instance (Num a, Num b, Num c) => Num (T3 a b c) where
    T3 x1 y1 z1 + T3 x2 y2 z2 = T3 (x1 + x2) (y1 + y2) (z1 + z2)
    T3 x1 y1 z1 - T3 x2 y2 z2 = T3 (x1 - x2) (y1 - y2) (z1 - z2)
    T3 x1 y1 z1 * T3 x2 y2 z2 = T3 (x1 * x2) (y1 * y2) (z1 * z2)
    negate (T3 x y z)   = T3 (negate x) (negate y) (negate z)
    abs    (T3 x y z)   = T3 (abs x   ) (abs y   ) (abs z   )
    signum (T3 x y z)   = T3 (signum x) (signum y) (signum z)
    fromInteger x       = T3 (fromInteger x) (fromInteger x) (fromInteger x)

instance (Num a, Num b, Num c, Num d) => Num (T4 a b c d) where
    T4 x1 y1 z1 w1 + T4 x2 y2 z2 w2 = T4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
    T4 x1 y1 z1 w1 - T4 x2 y2 z2 w2 = T4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
    T4 x1 y1 z1 w1 * T4 x2 y2 z2 w2 = T4 (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
    negate (T4 x y z w) = T4 (negate x) (negate y) (negate z) (negate w)
    abs    (T4 x y z w) = T4 (abs x   ) (abs y   ) (abs z   ) (abs w   )
    signum (T4 x y z w) = T4 (signum x) (signum y) (signum z) (signum w)
    fromInteger x       = T4 (fromInteger x) (fromInteger x) (fromInteger x) (fromInteger x)

instance Field1 (T2 a b) (T2 a' b) a a' where
    _1 f (T2 x y) = (`T2` y) <$> f x

instance Field2 (T2 a b) (T2 a b') b b' where
    _2 f (T2 x y) = T2 x <$> f y

instance Field1 (T3 a b c) (T3 a' b c) a a' where
    _1 f (T3 x y z) = (\x' -> T3 x' y z) <$> f x

instance Field2 (T3 a b c) (T3 a b' c) b b' where
    _2 f (T3 x y z) = (\y' -> T3 x y' z) <$> f y

instance Field3 (T3 a b c) (T3 a b c') c c' where
    _3 f (T3 x y z) = T3 x y <$> f z
