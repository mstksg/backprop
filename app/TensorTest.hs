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

import           Control.Monad.Primitive
import           Data.Functor
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           GHC.Generics                        (Generic)
import           Numeric.Backprop
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Numeric.LinearAlgebra.Static hiding (dot)
import           System.Random.MWC
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness hiding           (outer)
import           Type.Family.List
import qualified Generics.SOP                        as SOP

data Tensor :: [Nat] -> Type where
    TS :: { unTS :: Double } -> Tensor '[]
    TV :: { unTV :: R n    } -> Tensor '[n]
    TM :: { unTM :: L m n  } -> Tensor '[m,n]

deriving instance ListC (KnownNat <$> ns) => Show (Tensor ns)

instance SingI ns => Variate (Tensor ns) where
    uniform g = case sing :: Sing ns of
      SNil                             ->
          TS <$> uniform g
      SNat `SCons` SNil                ->
          TV <$> (randomVector <$> uniform g <*> pure Uniform)
      SNat `SCons` (SNat `SCons` SNil) ->
          TM <$> (uniformSample <$> uniform g <*> pure 0 <*> pure 1)
      _`SCons`(_`SCons`(_`SCons`_))    ->
          error "unimplemented"

matVec
    :: (KnownNat m, KnownNat n)
    => Op '[ Tensor '[m,n] , Tensor '[n] ] (Tensor '[m])
matVec = op2' $ \case
    TM m -> \case
      TV v -> ( TV (m #> v)
              , \(maybe 1 unTV -> g) ->
                   (TM (g `outer` v), TV (tr m #> g))
              )

dot :: KnownNat n
    => Op '[ Tensor '[n] , Tensor '[n] ] (Tensor '[])
dot = op2' $ \case
    TV x -> \case
      TV y -> ( TS (x <.> y)
              , \case Nothing      ->
                        (TV y            , TV x            )
                      Just (TS g)  ->
                        (TV (konst g * y), TV (x * konst g))
              )

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

data Layer :: Nat -> Nat -> Type where
    Layer :: { _lWeights :: Tensor '[m, n]
             , _lBiases  :: Tensor '[m]
             }
          -> Layer n m
      deriving (Show, Generic)

instance SOP.Generic (Layer n m)
instance (KnownNat n, KnownNat m) => Variate (Layer n m) where
    uniform g = subtract 1 . (* 2) <$> (Layer <$> uniform g <*> uniform g)

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

data Network :: Nat -> [Nat] -> Nat -> Type where
    NØ   :: !(Layer a b) -> Network a '[] b
    (:&) :: !(Layer a b) -> Network b bs c -> Network a (b ': bs) c

netExternal :: Iso' (Network a '[] b) (Tuple '[Layer a b])
netExternal = iso (\case NØ x     -> x ::< Ø)
                  (\case I x :< Ø -> NØ x   )

netInternal :: Iso' (Network a (b ': bs) c) (Tuple '[Layer a b, Network b bs c])
netInternal = iso (\case x :& xs          -> x ::< xs ::< Ø)
                  (\case I x :< I xs :< Ø -> x :& xs       )

netOp
    :: forall s a bs c. (KnownNat a, KnownNat c)
    => Sing bs
    -> BPOp s '[ Tensor '[a], Network a bs c ] (Tensor '[c])
netOp sbs = go sbs
  where
    go :: forall d es. KnownNat d
        => Sing es
        -> BPOp s '[ Tensor '[d], Network d es c ] (Tensor '[c])
    go = \case
      ses@SNil -> withInps $ \(x :< n :< Ø) -> do
        l :< Ø       <- netExternal #<~ n
        layerOp ~$ x :< l :< Ø
      se@SNat `SCons` ses -> withInps $ \(x :< n :< Ø) -> singWit ses // do
        l :< n' :< Ø <- netInternal #<~ n
        z <- layerOp ~$ x :< l :< Ø
        go ses       ~$ z :< n' :< Ø
    layerOp
        :: forall d e. (KnownNat d, KnownNat e)
        => BPOp s '[ Tensor '[d], Layer d e ] (Tensor '[e])
    layerOp = withInps $ \(x :< l :< Ø) -> do
        w :< b :< Ø <- gTuple #<~ l
        y           <- opRef2 w x matVec
        y'          <- opRef2 y b (op2 (+))
        opRef1 y' (op1 logistic)

singWit
    :: Sing a
    -> Wit (SingI a)
singWit s = withSingI s Wit

err
    :: KnownNat m
    => Tensor '[m]
    -> BPRef s rs (Tensor '[m])
    -> BPOp s rs (Tensor '[])
err targ r = do
    let t = constRef targ
    d <- opRef2 r t $ op2 (-)
    opRef2 d d      $ dot

instance (KnownNat a, SingI bs, KnownNat c) => Variate (Network a bs c) where
    uniform g = genNet sing (uniform g)

train
    :: (KnownNat a, SingI bs, KnownNat c)
    => Double
    -> Tensor '[a]
    -> Tensor '[b]
    -> Network a bs c
    -> Network a bs c
train r x t n = case backprop (netOp sing) (x ::< n ::< Ø) of
    (_, _ :< I gN :< Ø) -> n - (realToFrac r * gN)

main :: IO ()
main = withSystemRandom $ \g -> do
    n <- uniform @(Network 4 '[3,2] 1) g
    void $ traverseNetwork sing (\l -> l <$ print l) n







liftT0
    :: SingI ns
    => (forall a. Floating a => a)
    -> Tensor ns
liftT0 f = go sing
  where
    go :: forall ms. Sing ms -> Tensor ms
    go = \case
      SNil -> TS f
      SNat `SCons` SNil -> TV f
      SNat `SCons` (SNat `SCons` SNil) -> TM f
      _ `SCons` (_ `SCons` (_ `SCons` _)) -> error "not implemented"

liftT1
    :: SingI ns
    => (forall a. Floating a => a -> a)
    -> Tensor ns
    -> Tensor ns
liftT1 f = go sing
  where
    go :: forall ms. Sing ms -> Tensor ms -> Tensor ms
    go = \case
      SNil                             -> \case TS x -> TS (f x)
      SNat `SCons` SNil                -> \case TV x -> TV (f x)
      SNat `SCons` (SNat `SCons` SNil) -> \case TM x -> TM (f x)
      _ `SCons` (_ `SCons` (_ `SCons` _)) -> \case

liftT2
    :: SingI ns
    => (forall a. Floating a => a -> a -> a)
    -> Tensor ns
    -> Tensor ns
    -> Tensor ns
liftT2 f = go sing
  where
    go :: forall ms. Sing ms -> Tensor ms -> Tensor ms -> Tensor ms
    go = \case
      SNil -> \case
        TS x -> \case TS y -> TS (f x y)
      SNat `SCons` SNil -> \case
        TV x -> \case TV y -> TV (f x y)
      SNat `SCons` (SNat `SCons` SNil) -> \case
        TM x -> \case TM y -> TM (f x y)
      _ `SCons` (_ `SCons` (_ `SCons` _)) -> \case

instance SingI ns => Num (Tensor ns) where
    (+) = liftT2 (+)
    (-) = liftT2 (-)
    (*) = liftT2 (*)
    negate = liftT1 negate
    signum = liftT1 signum
    abs    = liftT1 abs
    fromInteger x = liftT0 (fromInteger x)

instance SingI ns => Fractional (Tensor ns) where
    (/) = liftT2 (/)
    recip = liftT1 recip
    fromRational x = liftT0 (fromRational x)

instance SingI ns => Floating (Tensor ns) where
    pi = liftT0 pi
    exp = liftT1 exp
    log = liftT1 log
    sqrt = liftT1 sqrt
    (**) = liftT2 (**)
    logBase = liftT2 logBase
    sin = liftT1 sin
    cos = liftT1 cos
    tan = liftT1 tan
    asin = liftT1 asin
    acos = liftT1 acos
    atan = liftT1 atan
    sinh = liftT1 sinh
    cosh = liftT1 cosh
    tanh = liftT1 tanh
    asinh = liftT1 asinh
    acosh = liftT1 acosh
    atanh = liftT1 atanh

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
