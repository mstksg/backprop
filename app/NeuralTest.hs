{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Functor
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Product
import           GHC.Generics                        (Generic)
import           Numeric.Backprop
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Numeric.LinearAlgebra.Static hiding (dot)
import           System.Random.MWC
import qualified Generics.SOP                        as SOP

-- Basic matrix/vector operations, as Ops

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

-- Containers holding our network parameters

data Layer :: Nat -> Nat -> Type where
    Layer :: { _lWeights :: L m n
             , _lBiases  :: R m
             }
          -> Layer n m
      deriving (Show, Generic)

data Network :: Nat -> [Nat] -> Nat -> Type where
    NØ   :: !(Layer a b) -> Network a '[] b
    (:&) :: !(Layer a b) -> Network b bs c -> Network a (b ': bs) c

-- Generic instance so we can use gTuple iso
instance SOP.Generic (Layer n m)

-- Iso's for the Network constructors
netExternal :: Iso' (Network a '[] b) (Tuple '[Layer a b])
netExternal = iso (\case NØ x     -> x ::< Ø)
                  (\case I x :< Ø -> NØ x   )

netInternal :: Iso' (Network a (b ': bs) c) (Tuple '[Layer a b, Network b bs c])
netInternal = iso (\case x :& xs          -> x ::< xs ::< Ø)
                  (\case I x :< I xs :< Ø -> x :& xs       )

-- "Running" a network on a given input
netOp
    :: forall s a bs c. (KnownNat a, KnownNat c)
    => Sing bs
    -> BPOp s '[ R a, Network a bs c ] (R c)
netOp sbs = go sbs
  where
    go :: forall d es. KnownNat d
        => Sing es
        -> BPOp s '[ R d, Network d es c ] (R c)
    go = \case
      SNil -> withInps $ \(x :< n :< Ø) -> do
        l :< Ø       <- netExternal #<~ n
        layerOp ~$ x :< l :< Ø
      SNat `SCons` ses -> withInps $ \(x :< n :< Ø) -> withSingI ses $ do
        l :< n' :< Ø <- netInternal #<~ n
        z <- layerOp ~$ x :< l :< Ø
        go ses       ~$ z :< n' :< Ø
    layerOp
        :: forall d e. (KnownNat d, KnownNat e)
        => BPOp s '[ R d, Layer d e ] (R e)
    layerOp = withInps $ \(x :< l :< Ø) -> do
        w :< b :< Ø <- gTuple #<~ l
        y           <- opRef2 w x matVec
        y'          <- opRef2 y b (op2 (+))
        opRef1 y' (op1 logistic)
      where
        logistic x = 1 / (1 + exp (-x))


-- calculate error based on target
err
    :: KnownNat m
    => R m
    -> BPRef s rs (R m)
    -> BPOp s rs Double
err targ r = do
    d <- opRef2 r t $ op2 (-)
    opRef2 d d      $ dot
  where
    t = constRef targ

-- gradient descent step
train
    :: (KnownNat a, SingI bs, KnownNat c)
    => Double
    -> R a
    -> R c
    -> Network a bs c
    -> Network a bs c
train r x t n = case backprop (err t =<< netOp sing) (x ::< n ::< Ø) of
    (_, _ :< I g :< Ø) -> n - (realToFrac r * g)

main :: IO ()
main = withSystemRandom $ \g -> do
    n <- uniform @(Network 4 '[3,2] 1) g
    void $ traverseNetwork sing (\l -> l <$ print l) n













-- non-bp-related boilerplate and instances for working with data types

instance KnownNat n => Variate (R n) where
    uniform g = randomVector <$> uniform g <*> pure Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

instance (KnownNat m, KnownNat n) => Variate (L m n) where
    uniform g = uniformSample <$> uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

instance (KnownNat n, KnownNat m) => Variate (Layer n m) where
    uniform g = subtract 1 . (* 2) <$> (Layer <$> uniform g <*> uniform g)
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

instance (KnownNat m, KnownNat n) => Num (Layer n m) where
    Layer w1 b1 + Layer w2 b2 = Layer (w1 + w2) (b1 + b2)
    Layer w1 b1 - Layer w2 b2 = Layer (w1 - w2) (b1 - b2)
    Layer w1 b1 * Layer w2 b2 = Layer (w1 * w2) (b1 * b2)
    abs    (Layer w b) = Layer (abs w) (abs b)
    signum (Layer w b) = Layer (signum w) (signum b)
    negate (Layer w b) = Layer (negate w) (negate b)
    fromInteger x = Layer (fromInteger x) (fromInteger x)

instance (KnownNat m, KnownNat n) => Fractional (Layer n m) where
    Layer w1 b1 / Layer w2 b2 = Layer (w1 / w2) (b1 / b2)
    recip (Layer w b) = Layer (recip w) (recip b)
    fromRational x = Layer (fromRational x) (fromRational x)

instance (KnownNat a, SingI bs, KnownNat c) => Variate (Network a bs c) where
    uniform g = genNet sing (uniform g)
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> uniform g

genNet
    :: forall f a bs c. (Applicative f, KnownNat a, KnownNat c)
    => Sing bs
    -> (forall d e. (KnownNat d, KnownNat e) => f (Layer d e))
    -> f (Network a bs c)
genNet sbs f = go sbs
  where
    go :: forall d es. KnownNat d => Sing es -> f (Network d es c)
    go = \case
      SNil             -> NØ <$> f
      SNat `SCons` ses -> (:&) <$> f <*> go ses

mapNetwork0
    :: forall a bs c. (KnownNat a, KnownNat c)
    => Sing bs
    -> (forall d e. (KnownNat d, KnownNat e) => Layer d e)
    -> Network a bs c
mapNetwork0 sbs f = getI $ genNet sbs (I f)

traverseNetwork
    :: forall a bs c f. (KnownNat a, KnownNat c, Applicative f)
    => Sing bs
    -> (forall d e. (KnownNat d, KnownNat e) => Layer d e -> f (Layer d e))
    -> Network a bs c
    -> f (Network a bs c)
traverseNetwork sbs f = go sbs
  where
    go :: forall d es. KnownNat d => Sing es -> Network d es c -> f (Network d es c)
    go = \case
      SNil -> \case
        NØ x -> NØ <$> f x
      SNat `SCons` ses -> \case
        x :& xs -> (:&) <$> f x <*> go ses xs

mapNetwork1
    :: forall a bs c. (KnownNat a, KnownNat c)
    => Sing bs
    -> (forall d e. (KnownNat d, KnownNat e) => Layer d e -> Layer d e)
    -> Network a bs c
    -> Network a bs c
mapNetwork1 sbs f = getI . traverseNetwork sbs (I . f)

mapNetwork2
    :: forall a bs c. (KnownNat a, KnownNat c)
    => Sing bs
    -> (forall d e. (KnownNat d, KnownNat e) => Layer d e -> Layer d e -> Layer d e)
    -> Network a bs c
    -> Network a bs c
    -> Network a bs c
mapNetwork2 sbs f = go sbs
  where
    go :: forall d es. KnownNat d => Sing es -> Network d es c -> Network d es c -> Network d es c
    go = \case
      SNil -> \case
        NØ x -> \case
          NØ y -> NØ (f x y)
      SNat `SCons` ses -> \case
        x :& xs -> \case
          y :& ys -> f x y :& go ses xs ys

instance (KnownNat a, SingI bs, KnownNat c) => Num (Network a bs c) where
    (+)           = mapNetwork2 sing (+)
    (-)           = mapNetwork2 sing (-)
    (*)           = mapNetwork2 sing (*)
    negate        = mapNetwork1 sing negate
    abs           = mapNetwork1 sing abs
    signum        = mapNetwork1 sing signum
    fromInteger x = mapNetwork0 sing (fromInteger x)

instance (KnownNat a, SingI bs, KnownNat c) => Fractional (Network a bs c) where
    (/)            = mapNetwork2 sing (/)
    recip          = mapNetwork1 sing recip
    fromRational x = mapNetwork0 sing (fromRational x)
