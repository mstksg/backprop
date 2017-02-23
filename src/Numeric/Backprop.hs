{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Backprop
  ( BP
  , BPNode
  , newBPRef
  , backprop
  ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Monoid
import           Data.STRef
import           Data.Traversable
import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Product
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Type.Class.Higher

newBPRef
    :: forall s as a bs. Num a
    => Prod (BPRef s bs) as
    -> Op as a
    -> Prod (Scaler a) as
    -> BP s bs (BPRef s bs a)
newBPRef i o s = do
    r <- liftBase $ newSTRef bp
    ifor1_ i $ \ix bpr' -> do
      let bpir = BPIR ix r
      case bpr' of
        BPRNode r' -> liftBase $ modifySTRef r' (over bpnOut (bpir:))
        BPRInp ix' -> modifying (bpsSources . indexP ix' . comp) (bpir :)
    return (BPRNode r)
  where
    bp :: BPNode s bs as a
    bp = BPN { _bpnInp       = i
             , _bpnOut       = []
             , _bpnOp        = o
             , _bpnResCache  = Nothing
             , _bpnGradCache = Nothing
             , _bpnSummer    = Summer sum
             , _bpnScaler    = s
             }

forwardPass
    :: Tuple bs
    -> BPRef s bs a
    -> ST s a
forwardPass env = \case
    BPRNode r -> fmap fst . caching bpnResCache r $ \BPN{..} ->
      runOp' _bpnOp <$> traverse1 (fmap I . forwardPass env) _bpnInp
    BPRInp ix -> return . getI $ index ix env

backwardPass
    :: forall s as a bs. ()
    => STRef s (BPNode s bs as a)
    -> ST s (Tuple as)
backwardPass r = fmap snd . caching bpnGradCache r $ \BPN{..} -> do
    outs :: [a]  <- for _bpnOut $ \bpir -> do
      BPIR (ix :: Index cs a) (r' :: STRef s (BPNode s bs cs c)) <- return bpir
      getI . index ix <$> backwardPass r'
    let totderv  = runSummer _bpnSummer outs
    case snd <$> _bpnResCache of
      Just gs  -> do
        let gradProd = zipWithP (\s g -> runScaler s totderv <$> g) _bpnScaler gs
        return (totderv, gradProd)
      Nothing -> error "backwards pass before forwards pass"

backprop
    :: (forall s. BP s as (BPRef s as a))
    -> Prod Summer as
    -> Tuple as
    -> (a, Tuple as)
backprop bp ss env = runST $ do
    (r, BPS{..}) <- runStateT (bpST bp) $ BPS (map1 (\_ -> Comp []) env)
    res  <- forwardPass env r
    gradLists <- for1 _bpsSources $ \(Comp rs) ->
      for rs $ \bpir -> do
        BPIR (ix :: Index cs a) (r' :: STRef s (BPNode s bs cs c)) <- return bpir
        getI . index ix <$> backwardPass r'
    let grad = zipWithP (\s xs -> I (runSummer s xs)) ss gradLists
    return (res, grad)


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
    let y = view l x
    case y of
      Just z ->
        return z
      Nothing -> do
        z <- f x
        modifySTRef r (set l (Just z))
        return z

for1
    :: (Applicative h, Traversable1 t)
    => t f b
    -> (forall a. f a -> h (g a))
    -> h (t g b)
for1 x f = traverse1 f x

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

indexP
    :: Functor f
    => Index as a
    -> (g a -> f (g a))
    -> Prod g as
    -> f (Prod g as)
indexP = \case
    IZ   -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> indexP i f xs

comp
    :: Functor f
    => (g (h a) -> f (g' (h' b)))
    -> (g :.: h) a
    -> f ((g' :.: h') b)
comp f (Comp x) = Comp <$> f x
