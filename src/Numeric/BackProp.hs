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

import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Kind
import           Data.Proxy
import           Data.STRef
import           Data.Traversable
import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Product
import           Data.Type.Sum hiding      (index)
import           Type.Class.Higher

data BPState :: Type -> [Type] -> Type -> Type where
    BPS :: { bpsSources :: !(Prod ([] :.: BPInpRef s) as)
           , bpsOutput  :: !(BPRef s a)
           }
        -> BPState s as a

newtype BP s as a b = BP { bpST :: StateT (BPState s as a) (ST s) b }
      deriving (Functor, Applicative, Monad)

newtype Op as a = Op { runOp :: Tuple as -> (a, Tuple as) }
newtype Scaler a b = Scaler { runScaler :: a -> b -> b }

data BPRef :: Type -> Type -> Type where
    BPRef :: { getBPRef :: STRef s (BPNode s as a) }
          -> BPRef s a

data BPInpRef :: Type -> Type -> Type where
    BPIR :: { bpirIndex :: Index bs a
            , bpirRef   :: STRef s (BPNode s bs b)
            }
         -> BPInpRef s a

data BPNode :: Type -> [Type] -> Type -> Type where
    BPN :: { bpInp       :: !(Prod (BPRef s) as)
           , bpOut       :: !([BPInpRef s a])
           , bpOp        :: !(Op as a)
           , bpResCache  :: !(Maybe (a, Tuple as))
           , bpGradCache :: !(Maybe (a, Tuple as))
           , bpSummer    :: !([a] -> a)
           , bpScaler    :: !(Prod (Scaler a) as)
           }
        -> BPNode s as a

-- | bpResCache lens
_bpResCache
    :: Functor f
    => (Maybe (a, Tuple as) -> f (Maybe (a, Tuple as)))
    -> BPNode s as a
    -> f (BPNode s as a)
_bpResCache f bpn = (\rc -> bpn { bpResCache = rc }) <$> f (bpResCache bpn)

-- | bpGradCache lens
_bpGradCache
    :: Functor f
    => (Maybe (a, Tuple as) -> f (Maybe (a, Tuple as)))
    -> BPNode s as a
    -> f (BPNode s as a)
_bpGradCache f bpn = (\rc -> bpn { bpGradCache = rc }) <$> f (bpGradCache bpn)

stBP
    :: ST s b
    -> BP s as a b
stBP s = BP (lift s)

newBPRef
    :: forall s as a b bs. Num a
    => Prod (BPRef s) as
    -> Op as a
    -> BP s bs b (BPRef s a)
newBPRef i o = BPRef <$> BP (lift (newSTRef bp))
  where
    bp :: BPNode s as a
    bp = BPN { bpInp       = i
             , bpOut       = []
             , bpOp        = o
             , bpResCache  = Nothing
             , bpGradCache = Nothing
             , bpSummer    = sum
             , bpScaler    = undefined
             }

forwardPass
    :: BPRef s a
    -> ST s a
forwardPass (BPRef r) = fmap fst . caching _bpResCache r $ \BPN{..} ->
    runOp bpOp <$> traverse1 (fmap I . forwardPass) bpInp

backwardPass
    :: forall s bs b as a. ()
    => STRef s (BPNode s as a)
    -> ST s (Tuple as)
backwardPass r = fmap snd . caching _bpGradCache r $ \BPN{..} -> do
    outs :: [a]  <- for bpOut $ \bpir -> do
      BPIR (ix :: Index cs a) (r' :: STRef s (BPNode s cs c)) <- return bpir
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

ifor1
    :: (Applicative h, IxTraversable1 i t)
    => t f b
    -> (forall a. i b a -> f a -> h (g a))
    -> h (t g b)
ifor1 x f = itraverse1 f x

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
