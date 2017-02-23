{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module Numeric.BackProp where

-- import           Data.Proxy
-- import           Data.Type.Sum hiding   (index)
import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Kind
import           Data.Monoid
import           Data.STRef
import           Data.Traversable
import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Product
import           Type.Class.Higher

data BPState :: Type -> [Type] -> Type -> Type where
    BPS :: { bpsSources :: !(Prod ([] :.: BPInpRef s as) as)
           , bpsOutput  :: !(BPRef s as a)
           }
        -> BPState s as a

newtype BP s as a b = BP { bpST :: StateT (BPState s as a) (ST s) b }
      deriving (Functor, Applicative, Monad)

newtype Op as a = Op { runOp :: Tuple as -> (a, Tuple as) }
newtype Scaler a b = Scaler { runScaler :: a -> b -> b }

data BPRef :: Type -> [Type] -> Type -> Type where
    BPRNode :: STRef s (BPNode s as bs a)
            -> BPRef s as a
    BPRInp  :: Index as a
            -> BPRef s as a

data BPInpRef :: Type -> [Type] -> Type -> Type where
    BPIR :: { bpirIndex :: Index bs a
            , bpirRef   :: STRef s (BPNode s as bs b)
            }
         -> BPInpRef s as a

data BPNode :: Type -> [Type] -> [Type] -> Type -> Type where
    BPN :: { bpInp       :: !(Prod (BPRef s bs) as)
           , bpOut       :: !([BPInpRef s bs a])
           , bpOp        :: !(Op as a)
           , bpResCache  :: !(Maybe (a, Tuple as))
           , bpGradCache :: !(Maybe (a, Tuple as))
           , bpSummer    :: !([a] -> a)
           , bpScaler    :: !(Prod (Scaler a) as)
           }
        -> BPNode s bs as a

-- | bpResCache lens
_bpResCache
    :: Functor f
    => (Maybe (a, Tuple as) -> f (Maybe (a, Tuple as)))
    -> BPNode s bs as a
    -> f (BPNode s bs as a)
_bpResCache f bpn = (\rc -> bpn { bpResCache = rc }) <$> f (bpResCache bpn)

-- | bpGradCache lens
_bpGradCache
    :: Functor f
    => (Maybe (a, Tuple as) -> f (Maybe (a, Tuple as)))
    -> BPNode s bs as a
    -> f (BPNode s bs as a)
_bpGradCache f bpn = (\rc -> bpn { bpGradCache = rc }) <$> f (bpGradCache bpn)

stBP
    :: ST s b
    -> BP s as a b
stBP s = BP (lift s)

newBPRef
    :: forall s as a b bs. Num a
    => Prod (BPRef s bs) as
    -> Op as a
    -> Prod (Scaler a) as
    -> BP s bs b (BPRef s bs a)
newBPRef i o s = do
    r <- stBP (newSTRef bp)
    ifor1_ i $ \ix bpr' -> do
      let bpir = BPIR ix r
      case bpr' of
        BPRNode r' -> stBP . modifySTRef r' $ \bpn ->
          bpn { bpOut = bpir : bpOut bpn }
        BPRInp ix' -> BP . modify $ \bps ->
          bps { bpsSources = overP (Comp . (bpir :) . getComp)
                               ix'
                               (bpsSources bps)
              }
    return (BPRNode r)
  where
    bp :: BPNode s bs as a
    bp = BPN { bpInp       = i
             , bpOut       = []
             , bpOp        = o
             , bpResCache  = Nothing
             , bpGradCache = Nothing
             , bpSummer    = sum
             , bpScaler    = s
             }

forwardPass
    :: Tuple bs
    -> BPRef s bs a
    -> ST s a
forwardPass env = \case
    BPRNode r -> fmap fst . caching _bpResCache r $ \BPN{..} ->
      runOp bpOp <$> traverse1 (fmap I . forwardPass env) bpInp
    BPRInp ix -> return . getI $ index ix env

backwardPass
    :: forall s as a bs. ()
    => STRef s (BPNode s bs as a)
    -> ST s (Tuple as)
backwardPass r = fmap snd . caching _bpGradCache r $ \BPN{..} -> do
    outs :: [a]  <- for bpOut $ \bpir -> do
      BPIR (ix :: Index cs a) (r' :: STRef s (BPNode s bs cs c)) <- return bpir
      getI . index ix <$> backwardPass r'
    let totderv  = bpSummer outs
    case snd <$> bpResCache of
      Just gs  -> do
        let gradProd = zipWithP (\s g -> runScaler s totderv <$> g) bpScaler gs
        return (totderv, gradProd)
      Nothing -> error "backwards pass before forwards pass"




-- | Apply a function to the contents of an STRef, and cache the results
-- using the given lens.  If already calculated, simply returned the cached
-- result.
caching
    :: (forall f. Functor f => (Maybe b -> f (Maybe b)) -> a -> f a)    -- ^ lens
    -> STRef s a
    -> (a -> ST s b)
    -> ST s b
caching l r f = do
    x <- readSTRef r
    let y = getC . l C $ x
    case y of
      Just z ->
        return z
      Nothing -> do
        z <- f x
        modifySTRef r (getI . l (I . const (Just z)))
        return z

-- for1
--     :: (Applicative h, Traversable1 t)
--     => t f b
--     -> (forall a. f a -> h (g a))
--     -> h (t g b)
-- for1 x f = traverse1 f x

ifor1
    :: (Applicative h, IxTraversable1 i t)
    => t f b
    -> (forall a. i b a -> f a -> h (g a))
    -> h (t g b)
ifor1 x f = itraverse1 f x

ifor1_
    :: (Applicative h, IxTraversable1 i t)
    => t f b
    -> (forall a. i b a -> f a -> h ())
    -> h ()
ifor1_ x f = ($ pure ())
           . appEndo
           . getConst
           . ifoldMap1 (\i y -> Const (Endo (f i y *>)))
           $ x

zipWithP
    :: (forall a. f a -> g a -> h a)
    -> Prod f as
    -> Prod g as
    -> Prod h as
zipWithP f = \case
    Ø -> \case
      Ø -> Ø
    x :< xs -> \case
      y :< ys ->
        f x y :< zipWithP f xs ys

overP
    :: (f a -> f a)
    -> Index as a
    -> Prod f as
    -> Prod f as
overP f = \case
    IZ -> \case
      x :< xs -> f x :< xs
    IS i -> \case
      x :< xs ->
        x :< overP f i xs
