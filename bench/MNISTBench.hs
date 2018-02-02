{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.DeepSeq
import           Control.Exception
import           Control.Lens hiding                 ((:<), (<.>))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Criterion.Main
import           Criterion.Types
import           Data.Bitraversable
import           Data.IDX
import           Data.Reflection
import           Data.Time
import           Data.Traversable
import           Data.Tuple
import           GHC.Generics                        (Generic)
import           GHC.TypeLits
import           Numeric.Backprop
import           Numeric.LinearAlgebra.Static
import           System.Directory
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Unboxed                 as VU
import qualified Generics.SOP                        as SOP
import qualified Numeric.LinearAlgebra               as HM
import qualified System.Random.MWC                   as MWC

data Layer i o =
    Layer { _lWeights :: !(L o i)
          , _lBiases  :: !(R o)
          }
  deriving (Show, Generic)

instance SOP.Generic (Layer i o)
instance NFData (Layer i o)

makeLenses ''Layer

data Network i h1 h2 o =
    Net { _nLayer1 :: !(Layer i  h1)
        , _nLayer2 :: !(Layer h1 h2)
        , _nLayer3 :: !(Layer h2 o)
        }
  deriving (Show, Generic)

instance SOP.Generic (Network i h1 h2 o)
instance NFData (Network i h1 h2 o)

makeLenses ''Network

infixr 8 #>!
(#>!)
    :: (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (L m n)
    -> BVar s (R n)
    -> BVar s (R m)
(#>!) = liftOp2 . op2 $ \m v ->
  ( m #> v, \g -> (g `outer` v, tr m #> g) )


infixr 8 <.>!
(<.>!)
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s (R n)
    -> BVar s Double
(<.>!) = liftOp2 . op2 $ \x y ->
  ( x <.> y, \g -> (konst g * y, x * konst g)
  )

konst'
    :: (KnownNat n, Reifies s W)
    => BVar s Double
    -> BVar s (R n)
konst' = liftOp1 . op1 $ \c -> (konst c, HM.sumElements . extract)

sumElements :: KnownNat n => R n -> Double
sumElements = HM.sumElements . extract

sumElements'
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s Double
sumElements' = liftOp1 . op1 $ \x -> (sumElements x, konst)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
{-# INLINE logistic #-}

runLayer
    :: (KnownNat i, KnownNat o, Reifies s W)
    => BVar s (Layer i o)
    -> BVar s (R i)
    -> BVar s (R o)
runLayer l x = (l ^^. lWeights) #>! x + (l ^^. lBiases)
{-# INLINE runLayer #-}

softMax :: (KnownNat n, Reifies s W) => BVar s (R n) -> BVar s (R n)
softMax x = konst' (1 / sumElements' x) * exp x
{-# INLINE softMax #-}

runNetwork
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => BVar s (Network i h1 h2 o)
    -> BVar s (R i)
    -> BVar s (R o)
runNetwork n = softMax
             . runLayer (n ^^. nLayer3)
             . logistic
             . runLayer (n ^^. nLayer2)
             . logistic
             . runLayer (n ^^. nLayer1)
{-# INLINE runNetwork #-}

crossEntropy :: (KnownNat n, Reifies s W) => R n -> BVar s (R n) -> BVar s Double
crossEntropy t r = negate $ log r <.>! constVar t
{-# INLINE crossEntropy #-}

netErr
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => R o
    -> BVar s (Network i h1 h2 o)
    -> BVar s (R i)
    -> BVar s Double
netErr t n = crossEntropy t . runNetwork n
{-# INLINE netErr #-}

trainStep
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainStep r !x !t !n = case gradBP (uncurryVar (netErr t)) (n, x) of
    (gN, _) -> n - (realToFrac r * gN)
{-# INLINE trainStep #-}

runLayerManual
    :: (KnownNat i, KnownNat o)
    => Layer i o
    -> R i
    -> R o
runLayerManual l x = (l ^. lWeights) #> x + (l ^. lBiases)
{-# INLINE runLayerManual #-}

softMaxManual :: KnownNat n => R n -> R n
softMaxManual x = konst (1 / sumElements x) * exp x
{-# INLINE softMaxManual #-}

runNetManual
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Network i h1 h2 o
    -> R i
    -> R o
runNetManual n = softMaxManual
               . runLayerManual (n ^. nLayer3)
               . logistic
               . runLayerManual (n ^. nLayer2)
               . logistic
               . runLayerManual (n ^. nLayer1)
{-# INLINE runNetManual #-}

gradNetManual
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
gradNetManual x t (Net (Layer w1 b1) (Layer w2 b2) (Layer w3 b3)) =
    let y1 = w1 #> x
        z1 = y1 + b1
        x2 = logistic z1
        y2 = w2 #> x2
        z2 = y2 + b2
        x3 = logistic z2
        y3 = w3 #> x3
        z3 = y3 + b3
        o0 = exp z3
        o1 = HM.sumElements (extract o0)
        o2 = o0 / konst o1
        -- o3 = - (log o2 <.> t)
        dEdO3 = 1
        dEdO2 = dEdO3 * (- t / o2)
        dEdO1 = - (dEdO2 <.> o0) / (o1 ** 2)
        dEdO0 = konst dEdO1 + dEdO2 / konst o1
        dEdZ3 = dEdO0 * o0
        dEdY3 = dEdZ3
        dEdX3 = tr w3 #> dEdY3
        dEdZ2 = dEdX3 * (x3 * (1 - x3))
        dEdY2 = dEdZ2
        dEdX2 = tr w2 #> dEdY2
        dEdZ1 = dEdX2 * (x2 * (1 - x2))
        dEdY1 = dEdZ1
        dEdB3 = dEdZ3
        dEdW3 = dEdY3 `outer` x3
        dEdB2 = dEdZ2
        dEdW2 = dEdY2 `outer` x2
        dEdB1 = dEdZ1
        dEdW1 = dEdY1 `outer` x
    in  Net (Layer dEdW1 dEdB1) (Layer dEdW2 dEdB2) (Layer dEdW3 dEdB3)
{-# INLINE gradNetManual #-}

trainStepManual
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainStepManual r !x !t !n =
    let gN = gradNetManual x t n
    in  n - (realToFrac r * gN)

main :: IO ()
main = MWC.withSystemRandom $ \g -> do
    Just test  <- loadMNIST "data/t10k-images-idx3-ubyte"  "data/t10k-labels-idx1-ubyte"
    putStrLn "Loaded data."
    net0 <- MWC.uniformR @(Network 784 300 100 9) (-0.5, 0.5) g
    createDirectoryIfMissing True "bench-results"
    t <- getZonedTime
    let test0   = head test
        tstr    = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
    defaultMainWith defaultConfig
          { reportFile = Just $ "bench-results/mnist-bench_" ++ tstr ++ ".html"
          , timeLimit  = 10
          } [
        bgroup "gradient" [
            let testManual x y = gradNetManual x y net0
            in  bench "manual" $ nf (uncurry testManual) test0
          , let testBP     x y = fst . gradBP (uncurryVar (netErr y)) $ (net0, x)
            in  bench "bp"     $ nf (uncurry testBP) test0
          ]
      , bgroup "descent" [
            let testManual x y = trainStepManual 0.02 x y net0
            in  bench "manual" $ nf (uncurry testManual) test0
          , let testBP     x y = trainStep 0.02 x y net0
            in  bench "bp"     $ nf (uncurry testBP) test0
          ]
      , bgroup "run" [
            let testManual     = runNetManual net0
            in  bench "manual" $ nf testManual (fst test0)
          , let testBP     x   = evalBP (uncurryVar runNetwork) (net0, x)
            in  bench "bp"     $ nf testBP (fst test0)
          ]
      ]

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
    mkLabel n = create $ HM.build 9 (\i -> if round i == n then 1 else 0)

instance (KnownNat i, KnownNat o) => Num (Layer i o) where
    Layer w1 b1 + Layer w2 b2 = Layer (w1 + w2) (b1 + b2)
    Layer w1 b1 - Layer w2 b2 = Layer (w1 - w2) (b1 - b2)
    Layer w1 b1 * Layer w2 b2 = Layer (w1 * w2) (b1 * b2)
    abs    (Layer w b)        = Layer (abs    w) (abs    b)
    signum (Layer w b)        = Layer (signum w) (signum b)
    negate (Layer w b)        = Layer (negate w) (negate b)
    fromInteger x             = Layer (fromInteger x) (fromInteger x)

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Num (Network i h1 h2 o) where
    Net a b c + Net d e f = Net (a + d) (b + e) (c + f)
    Net a b c - Net d e f = Net (a - d) (b - e) (c - f)
    Net a b c * Net d e f = Net (a * d) (b * e) (c * f)
    abs    (Net a b c)    = Net (abs    a) (abs    b) (abs    c)
    signum (Net a b c)    = Net (signum a) (signum b) (signum c)
    negate (Net a b c)    = Net (negate a) (negate b) (negate c)
    fromInteger x         = Net (fromInteger x) (fromInteger x) (fromInteger x)

instance (KnownNat i, KnownNat o) => Fractional (Layer i o) where
    Layer w1 b1 / Layer w2 b2 = Layer (w1 / w2) (b1 / b2)
    recip (Layer w b)         = Layer (recip w) (recip b)
    fromRational x            = Layer (fromRational x) (fromRational x)

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Fractional (Network i h1 h2 o) where
    Net a b c / Net d e f = Net (a / d) (b / e) (c / f)
    recip (Net a b c)     = Net (recip a) (recip b) (recip c)
    fromRational x        = Net (fromRational x) (fromRational x) (fromRational x)

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

instance (Num a, Num b) => Num (a, b) where
    (x1,y1) + (x2,y2) = (x1 + x2, y1 + y2)
    (x1,y1) * (x2,y2) = (x1 * x2, y1 * y2)
    (x1,y1) - (x2,y2) = (x1 - x2, y1 - y2)
    abs (x, y)        = (abs x, abs y)
    signum (x, y)     = (signum x, signum y)
    fromInteger x     = (fromInteger x, fromInteger x)

-- softMaxCrossEntropy
--     :: KnownNat n
--     => R n
--     -> BPOpI s '[ R n ] Double
-- softMaxCrossEntropy targ (r :< Ø) =  realToFrac tsum * log (vsum .$ (r :< Ø))
--                                        - (dot .$ (r :< t :< Ø))
--   where
--     tsum = HM.sumElements . extract $ targ
--     t    = constVar targ

