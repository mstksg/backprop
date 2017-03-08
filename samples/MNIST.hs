{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bitraversable
import           Data.Foldable
import           Data.IDX
import           Data.List.Split
import           Data.Maybe
import           Data.Proxy
import           Data.Time.Clock
import           Data.Traversable
import           Data.Tuple
import           GHC.Generics                        (Generic)
import           GHC.TypeLits
import           Numeric.Backprop
import           Numeric.LinearAlgebra.Static hiding (dot)
import           Text.Printf
import qualified Data.Vector                         as V
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Unboxed                 as VU
import qualified Generics.SOP                        as SOP
import qualified Numeric.LinearAlgebra               as LA
import qualified System.Random.MWC                   as MWC
import qualified System.Random.MWC.Distributions     as MWC

data Layer i o =
    Layer { _lWeights :: !(L o i)
          , _lBiases  :: !(R o)
          }
  deriving (Show, Generic)

instance SOP.Generic (Layer i o)
instance NFData (Layer i o)

data Network i h1 h2 o =
    Net { _nLayer1 :: !(Layer i  h1)
        , _nLayer2 :: !(Layer h1 h2)
        , _nLayer3 :: !(Layer h2 o)
        }
  deriving (Show, Generic)

instance SOP.Generic (Network i h1 h2 o)
instance NFData (Network i h1 h2 o)

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

rkonst
    :: forall n. KnownNat n
    => Op '[ Double ] (R n)
rkonst = op1' $ \x -> (konst x, maybe (fromIntegral (natVal @n Proxy))
                                      (LA.sumElements . extract)
                      )

rsum
    :: KnownNat n
    => Op '[ R n ] Double
rsum = op1' $ \x -> (LA.sumElements (extract x), maybe 1 konst)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

softmax :: KnownNat i => BPOp s '[ R i ] (R i)
softmax = withInps $ \(x :< Ø) -> do
    x' <- bindVar $ exp x
    s  <- rsum   ~$ (x' :< Ø)
    k  <- rkonst ~$ (s  :< Ø)
    return $ x' / k

runLayer
    :: (KnownNat i, KnownNat o)
    => BPOp s '[ R i, Layer i o ] (R o)
runLayer = withInps $ \(x :< l :< Ø) -> do
    w :< b :< Ø <- gTuple #<~ l
    y <- matVec  ~$ (w :< x :< Ø)
    return $ y + b

runNetwork
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => BPOp s '[ R i, Network i h1 h2 o ] (R o)
runNetwork = withInps $ \(x :< n :< Ø) -> do
    l1 :< l2 :< l3 :< Ø <- gTuple #<~ n
    y <- runLayer -$ (x          :< l1 :< Ø)
    z <- runLayer -$ (logistic y :< l2 :< Ø)
    r <- runLayer -$ (logistic z :< l3 :< Ø)
    softmax       -$ (r          :< Ø)

errOp
    :: KnownNat m
    => R m
    -> BVar s rs (R m)
    -> BPOp s rs Double
errOp targ r = do
    err <- bindVar $ r - t
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
trainStep r !x !t !n = case gradBPOp o (x ::< n ::< Ø) of
    _ :< I gN :< Ø ->
        n - (realToFrac r * gN)
  where
    o :: BPOp s '[ R i, Network i h1 h2 o ] Double
    o = do
      y <- runNetwork
      errOp t y

trainList
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> [(R i, R o)]
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainList r = flip $ foldl' (\n (x,y) -> trainStep r x y n)

testNet
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => [(R i, R o)]
    -> Network i h1 h2 o
    -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test :: R i -> R o -> Double
    test x (extract->t)
        | LA.maxIndex t == LA.maxIndex (extract r) = 1
        | otherwise                                = 0
      where
        r :: R o
        r = evalBPOp runNetwork (x ::< n ::< Ø)

main :: IO ()
main = MWC.withSystemRandom $ \g -> do
    Just train <- loadMNIST "data/train-images-idx3-ubyte" "data/train-labels-idx1-ubyte"
    Just test  <- loadMNIST "data/t10k-images-idx3-ubyte"  "data/t10k-labels-idx1-ubyte"
    putStrLn "Loaded data."
    net0 :: Network 784 300 100 9 <- MWC.uniformR (-1, 1) g
    flip evalStateT net0 . forever $ do
      train' <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList train) g

      liftIO $ putStrLn "Training epoch..."
      forM_ (chunksOf 5000 train') $ \chnk -> StateT $ \n0 -> do
        test' <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList train) g

        t0 <- getCurrentTime
        n' <- evaluate . force $ trainList 0.05 chnk n0
        t1 <- getCurrentTime

        printf "Trained on 5000 points in %s.\n" (show (t1 `diffUTCTime` t0))

        let score = testNet (take 5000 test) n'
        printf "Error: %.3f%%\n" ((1 - score) * 100)

        return ((), n')

loadMNIST
    :: FilePath
    -> FilePath
    -> IO (Maybe [(R 784, R 9)])
loadMNIST fpI fpL = runMaybeT $ do
    i <- MaybeT          $ decodeIDXFile       fpI
    l <- MaybeT          $ decodeIDXLabelsFile fpL
    d <- MaybeT . return $ labeledIntData l i
    r <- MaybeT . return $ for d (bitraverse mkImage mkLabel . swap)
    liftIO . evaluate $ force r
  where
    mkImage :: VU.Vector Int -> Maybe (R 784)
    mkImage = create . VG.convert . VG.map (\i -> fromIntegral i / 255)
    mkLabel :: Int -> Maybe (R 9)
    mkLabel n = create $ LA.build 9 (\i -> if i == fromIntegral n then 1 else 0)



instance KnownNat n => MWC.Variate (R n) where
    uniform g = randomVector <$> MWC.uniform g <*> pure Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (L m n) where
    uniform g = uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat i, KnownNat o) => MWC.Variate (Layer i o) where
    uniform g = Layer <$> MWC.uniform g <*> MWC.uniform g
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => MWC.Variate (Network i h1 h2 o) where
    uniform g = Net <$> MWC.uniform g <*> MWC.uniform g <*> MWC.uniform g
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

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
