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

import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Data.Type.Vector
import           GHC.Generics                        (Generic)
import           GHC.TypeLits
import           Numeric.AD
import           Numeric.Backprop
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Numeric.LinearAlgebra.Static hiding (dot)
import           System.Random.MWC
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness hiding           (outer)
import           Type.Family.List

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

data Layer :: (Nat, Nat) -> Type where
    Layer :: { _lWeights :: Tensor '[m, n]
             , _lBiases  :: Tensor '[m]
             }
          -> Layer '(n, m)

deriving instance (KnownNat n, KnownNat m) => Show (Layer '(n, m))
instance (KnownNat n, KnownNat m) => Variate (Layer '(n, m)) where
    uniform g = subtract 1 . (* 2) <$> (Layer <$> uniform g <*> uniform g)

layer :: Iso' (Layer '(n, m)) (Tuple '[ Tensor '[m, n], Tensor '[m] ])
layer = iso (\case Layer w b -> w ::< b ::< Ø)
            (\case I w :< I b :< Ø -> Layer w b)

instance (KnownNat m, KnownNat n) => Num (Layer '(n, m)) where
    Layer w1 b1 + Layer w2 b2 = Layer (w1 + w2) (b1 + b2)
    Layer w1 b1 - Layer w2 b2 = Layer (w1 - w2) (b1 - b2)
    Layer w1 b1 * Layer w2 b2 = Layer (w1 * w2) (b1 * b2)
    abs    (Layer w b) = Layer (abs w) (abs b)
    signum (Layer w b) = Layer (signum w) (signum b)
    negate (Layer w b) = Layer (negate w) (negate b)
    fromInteger x = Layer (fromInteger x) (fromInteger x)

instance (KnownNat m, KnownNat n) => Fractional (Layer '(n, m)) where
    Layer w1 b1 / Layer w2 b2 = Layer (w1 / w2) (b1 / b2)
    recip (Layer w b) = Layer (recip w) (recip b)
    fromRational x = Layer (fromRational x) (fromRational x)

unLayer
    :: Layer '(n, m)
    -> (Tensor '[m, n], Tensor '[m])
unLayer (Layer w b) = (w, b)

ffLayer
    :: forall i o m. (KnownNat i, KnownNat o, PrimMonad m)
    => Gen (PrimState m)
    -> m (LayerOp i o)
ffLayer g = LO ffLayer' <$> uniform g
  where
    ffLayer' :: BPOp s '[ Tensor '[i], Layer '(i, o) ] (Tensor '[o])
    ffLayer' = withInps $ \(x :< l :< Ø) -> do
      w :< b :< Ø <- refParts layer l
      y           <- newBPRef2 w x matVec
      y'          <- newBPRef2 y b $ op2 (+)
      newBPRef1 y' $ op1 logistic

data Network :: Nat -> Nat -> Type where
    N :: (Every Num (Layer <$> ls), Every Fractional (Layer <$> ls), Known Length (Layer <$> ls))
      => { _nsLs    :: !(Sing ls)
         , _nParams :: !(Tuple (Layer <$> ls))
         , _nOp     :: !(forall s. BPOp s (Tensor '[i] ': (Layer <$> ls)) (Tensor '[o]))
         }
      -> Network i o

data LayerOp :: Nat -> Nat -> Type where
    LO :: { _loOp    :: (forall s. BPOp s '[ Tensor '[n],  Layer '(n, m) ] (Tensor '[m]))
          , _loLayer :: Layer '(n, m)
          }
        -> LayerOp n m

net0 :: Network n n
net0 = N SNil Ø (return $ inpRef IZ)

(~*)
    :: forall n m o. (KnownNat n, KnownNat m, KnownNat o)
    => LayerOp n m
    -> Network m o
    -> Network n o
(~*) = \case
    LO oL l -> \case
      N gs ls oN ->
        N (sing `SCons` gs) (l ::< ls) $ withInps $ \(rx :< rl :< rls) -> do
          ry <- oL ~$ rx :< rl :< Ø
          oN ~$ ry :< rls

err
    :: KnownNat m
    => Tensor '[m]
    -> BPRef s rs (Tensor '[m])
    -> BPOp s rs (Tensor '[])
err targ r = do
    let t = constRef targ
    d <- newBPRef2 r t $ op2 (-)
    newBPRef2 d d      $ dot

train
    :: (KnownNat i, KnownNat o)
    => Double
    -> Tensor '[i]
    -> Tensor '[o]
    -> Network i o
    -> Network i o
train r x t = \case
    N ss ls oN ->
      case backprop (err t =<< oN) (x ::< ls) of
        (_, _ :< gLs) ->
          N ss
            (imap1 (\ix (I l :&: I g) -> I (l - realToFrac r * g) \\ every @_ @Fractional ix)
                   (ls `zipP` gLs)
            )
            oN

genNet
    :: forall i o (ls :: [Nat]) m. (KnownNat i, KnownNat o, PrimMonad m)
    => Sing ls
    -> Gen (PrimState m)
    -> m (Network i o)
genNet = \case
    SNil            -> fmap (~* net0) . ffLayer
    (SNat :: Sing h) `SCons` ss -> \g -> do
      l <- ffLayer @i @h g
      n <- genNet ss g
      return $ l ~* n

main :: IO ()
main = withSystemRandom $ \g -> do
    n <- genNet @4 @1 (sing :: Sing '[3,2]) g
    -- print n
    print . _loLayer =<< ffLayer @4 @3 g










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


