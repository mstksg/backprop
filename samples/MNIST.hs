{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Foldable
import           Data.IDX
import           Data.Kind
import           Data.Maybe
import           GHC.Generics                        (Generic)
import           GHC.TypeLits
import           Numeric.Backprop
import           Numeric.LinearAlgebra.Static hiding (dot)
import           System.Random.MWC
import qualified Data.Binary                         as B
import qualified Generics.SOP                        as SOP

data Layer i o =
    Layer { _lWeights :: !(L o i)
          , _lBiases  :: !(R o)
          }
  deriving (Show, Generic)

instance SOP.Generic (Layer i o)

data Network i h1 h2 o =
    Net { _nLayer1 :: !(Layer i  h1)
        , _nLayer2 :: !(Layer h1 h2)
        , _nLayer3 :: !(Layer h2 o)
        }
  deriving (Show, Generic)

instance SOP.Generic (Network i h1 h2 o)

matVec
    :: (KnownNat m, KnownNat n)
    => Op '[ L m n, R n ] (R m)
matVec = op2' $ \m v -> ( m #> v
                        , \(fromMaybe 1 -> g) ->
                             (g `outer` v, tr m #> g)
                        )

dot :: KnownNat n
    => Op '[ R n, R n ] Double
dot = op2' $ \x y -> ( x <.> y
                     , \case Nothing -> (y, x)
                             Just g  -> (konst g * y, x * konst g)
                     )

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

runLayer
    :: (KnownNat i, KnownNat o)
    => BPOp s '[ R i, Layer i o ] (R o)
runLayer = withInps $ \(x :< l :< Ø) -> do
    w :< b :< Ø <- gTuple #<~ l
    y <- matVec  ~$ (w :< x :< Ø)
    op1 logistic ~$ (y + b :< Ø)

runNetwork
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => BPOp s '[ R i, Network i h1 h2 o ] (R o)
runNetwork = withInps $ \(x :< n :< Ø) -> do
    l1 :< l2 :< l3 :< Ø <- gTuple #<~ n
    y <- runLayer -$ (x :< l1 :< Ø)
    z <- runLayer -$ (y :< l2 :< Ø)
    runLayer -$ (z :< l3 :< Ø)

errOp
    :: KnownNat m
    => R m
    -> BVar s rs (R m)
    -> BPOp s rs Double
errOp targ r = do
    err <- op2 (-) ~$ (r :< t :< Ø)
    dot ~$ (err :< err :< Ø)
  where
    t = constVar targ

trainStep
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainStep r x t n = case gradBPOp o (x ::< n ::< Ø) of
    _ :< I gN :< Ø ->
        n - (realToFrac r * gN)
  where
    o :: BPOp s '[ R i, Network i h1 h2 o ] Double
    o = do
      y <- runNetwork
      errOp t y

trainEpoch
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> [(R i, R o)]
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainEpoch r = flip $ foldl' (\n (x,y) -> trainStep r x y n)

main :: IO ()
main = putStrLn "hey"

-- data MSet a = MS { msImages :: a
--                  , msLabels :: a
--                  }

-- mnist :: [MSet FilePath]
-- mnist = [ MS "data/t10k-images-idx3-ubyte"  "data/t10k-labels-idx1-ubyte"
--         , MS "data/train-images-idx3-ubyte" "data/train-labels-idx1-ubyte"
--         ]

-- main :: IO ()
-- main = do
--     forM_ mnist $ \(MS fpI fpL) -> do
--       i <- decodeIDXFile fpI
--       l <- decodeIDXLabelsFile fpL
--       mapM_ (print . idxType) i


instance KnownNat n => Variate (R n) where
    uniform g = randomVector <$> uniform g <*> pure Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

instance (KnownNat m, KnownNat n) => Variate (L m n) where
    uniform g = uniformSample <$> uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

instance (KnownNat n, KnownNat m) => Variate (Layer n m) where
    uniform g = subtract 1 . (* 2) <$> (Layer <$> uniform g <*> uniform g)
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

instance (KnownNat i, KnownNat o) => Num (Layer i o) where
    Layer w1 b1 + Layer w2 b2 = Layer (w1 + w2) (b1 + b2)
    Layer w1 b1 - Layer w2 b2 = Layer (w1 - w2) (b1 - b2)
    Layer w1 b1 * Layer w2 b2 = Layer (w1 * w2) (b1 * b2)
    abs    (Layer w b)        = Layer (abs w) (abs b)
    signum (Layer w b)        = Layer (signum w) (signum b)
    negate (Layer w b)        = Layer (negate w) (negate b)
    fromInteger x             = Layer (fromInteger x) (fromInteger x)

instance (KnownNat i, KnownNat o) => Fractional (Layer i o) where
    Layer w1 b1 / Layer w2 b2 = Layer (w1 / w2) (b1 / b2)
    recip (Layer w b)         = Layer (recip w) (recip b)
    fromRational x            = Layer (fromRational x) (fromRational x)

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Num (Network i h1 h2 o) where
    Net a b c + Net d e f = Net (a + d) (b + e) (c + f)
    Net a b c - Net d e f = Net (a - d) (b - e) (c - f)
    Net a b c * Net d e f = Net (a * d) (b * e) (c * f)
    negate (Net a b c)    = Net (negate a) (negate b) (negate c)
    signum (Net a b c)    = Net (signum a) (signum b) (signum c)
    abs    (Net a b c)    = Net (abs    a) (abs    b) (abs    c)
    fromInteger x         = Net (fromInteger x) (fromInteger x) (fromInteger x)

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Fractional (Network i h1 h2 o) where
    Net a b c / Net d e f = Net (a / d) (b / e) (c / f)
    recip (Net a b c)     = Net (recip a) (recip b) (recip c)
    fromRational x        = Net (fromRational x) (fromRational x) (fromRational x)
