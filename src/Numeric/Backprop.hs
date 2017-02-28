{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Backprop
  ( BP, BPOp
  , BPRef
  , newBPRef
  , newBPRef0
  , newBPRef1
  , newBPRef2
  , newBPRef3
  , backprop
  , plugBP
  , newBPRef'
  , newBPRef0'
  , newBPRef1'
  , newBPRef2'
  , newBPRef3'
  , backprop'
  , plugBP'
  , inpRef, inpRefs, withInps
  , Op(..)
  , Summer(..)
  , Unity(..)
  , Prod(..), pattern (:>), only
  , Tuple, pattern (::<), only_
  ) where

import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.STRef
import           Data.Traversable
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.List

type BPOp s f rs a = BP s f rs (BPRef s f rs a)

newBPRef'
    :: forall s f rs as a. ()
    => Prod (BPRef s f rs) as
    -> Op f as a
    -> Summer f a
    -> BP s f rs (BPRef s f rs a)
newBPRef' i o sm = do
    xs <- traverse1 resolveRef i
    let (res, gf) = runOp' o xs
        bp = BPN { _bpnOut       = FRInternal []
                 , _bpnRes       = res
                 , _bpnGradFunc  = return . gf
                 , _bpnGradCache = Nothing
                 , _bpnSummer    = sm
                 }
    r <- liftBase $ newSTRef bp
    itraverse1_ (registerRef r) i
    return (BPRNode r)

resolveRef
    :: (MonadReader (Prod f rs) m, MonadBase (ST s) m)
    => BPRef s f rs a
    -> m (f a)
resolveRef = \case
    BPRNode r -> _bpnRes <$> liftBase (readSTRef r)
    BPRInp ix -> index ix <$> ask

registerRef
    :: STRef s (BPNode s f rs as b)
    -> Index as a
    -> BPRef s f rs a
    -> BP s f rs ()
registerRef r ix = \case
    BPRNode r' -> liftBase . modifySTRef r' $ over (bpnOut . _FRInternal) (bpir :)
    BPRInp ix' -> modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
  where
    bpir = BPIR ix r

newBPRef
    :: Num (f a)
    => Prod (BPRef s f rs) as
    -> Op f as a
    -> BP s f rs (BPRef s f rs a)
newBPRef i o = newBPRef' i o known

newBPRef0'
    :: Op f '[] a
    -> Summer f a
    -> BP s f rs (BPRef s f rs a)
newBPRef0' = newBPRef' Ø

newBPRef0
    :: Num (f a)
    => Op f '[] a
    -> BP s f rs (BPRef s f rs a)
newBPRef0 = newBPRef Ø

newBPRef1'
    :: BPRef s f rs a
    -> Op f '[a] b
    -> Summer f b
    -> BP s f rs (BPRef s f rs b)
newBPRef1' r = newBPRef' (r :< Ø)

newBPRef1
    :: Num (f b)
    => BPRef s f rs a
    -> Op f '[a] b
    -> BP s f rs (BPRef s f rs b)
newBPRef1 r o = newBPRef1' r o known

newBPRef2'
    :: BPRef s f rs a
    -> BPRef s f rs b
    -> Op f '[a,b] c
    -> Summer f c
    -> BP s f rs (BPRef s f rs c)
newBPRef2' rx ry = newBPRef' (rx :< ry :< Ø)

newBPRef2
    :: Num (f c)
    => BPRef s f rs a
    -> BPRef s f rs b
    -> Op f '[a,b] c
    -> BP s f rs (BPRef s f rs c)
newBPRef2 rx ry o = newBPRef2' rx ry o known

newBPRef3'
    :: BPRef s f rs a
    -> BPRef s f rs b
    -> BPRef s f rs c
    -> Op f '[a,b,c] d
    -> Summer f d
    -> BP s f rs (BPRef s f rs d)
newBPRef3' rx ry rz = newBPRef' (rx :< ry :< rz :< Ø)

newBPRef3
    :: Num (f d)
    => BPRef s f rs a
    -> BPRef s f rs b
    -> BPRef s f rs c
    -> Op f '[a,b,c] d
    -> BP s f rs (BPRef s f rs d)
newBPRef3 rx ry rz o = newBPRef3' rx ry rz o known

backwardPass
    :: forall s f rs as a. ()
    => STRef s (BPNode s f rs as a)
    -> ST s (Prod f as)
backwardPass r = fmap snd . caching bpnGradCache r $ \BPN{..} -> do
    totderv <- case _bpnOut of
      FRInternal rs -> do
        outs <- for rs $ \(BPIR ix r') ->
          index ix <$> backwardPass r'
        return (Just $ runSummer _bpnSummer outs)
      FRExternal gE -> return (Just gE)
      FRTerminal    -> return Nothing
    g <- _bpnGradFunc totderv
    return (totderv, g)

backprop'
    :: (forall s. BPOp s f rs a)
    -> Prod (Summer f) rs
    -> Prod (Unity f) rs
    -> Prod f rs
    -> (f a, Prod f rs)
backprop' bp ss us env = runST $ do
    (res, gFunc) <- backpropWith bp ss us env
    grad <- gFunc Nothing
    return (res, grad)

backprop
    :: forall f rs a. Every Num (f <$> rs)
    => (forall s. BPOp s f rs a)
    -> Prod f rs
    -> (f a, Prod f rs)
backprop bp xs = backprop' bp (imap1 (\i _ -> known \\ every @_ @Num (reIndex @_ @f i)) xs)
                              (imap1 (\i _ -> known \\ every @_ @Num (reIndex @_ @f i)) xs)
                              xs

backpropWith
    :: BPOp s f rs a
    -> Prod (Summer f) rs
    -> Prod (Unity f) rs
    -> Prod f rs
    -> ST s (f a, Maybe (f a) -> ST s (Prod f rs))
backpropWith bp ss us env = do
    (r, bps0) <- runStateT (runReaderT (bpST bp) env)
                           (BPS (map1 (\_ -> FRInternal []) env))
    res <- runReaderT (resolveRef r) env
    let gradFunc gradOut = do
          let fr = case gradOut of
                     Just g  -> FRExternal g
                     Nothing -> FRTerminal
          BPS{..} <- case r of
            BPRNode sr -> bps0 <$ modifySTRef sr (set bpnOut fr)
            BPRInp  ix -> return $ set (bpsSources . indexP ix) fr bps0
          for1 (ss `zipP` us `zipP` _bpsSources) $ \((s :&: u) :&: rs) -> do
            case rs of
              FRInternal rs' ->
                fmap (runSummer s) . for rs' $ \(BPIR ix r') ->
                  index ix <$> backwardPass r'
              FRExternal gE  ->
                return $ gE
              FRTerminal     ->
                return $ getUnity u
    return (res, gradFunc)

plugBP'
    :: Prod (BPRef s f rs) as
    -> BPOp s f as a
    -> Prod (Summer f) as
    -> Prod (Unity f) as
    -> Summer f a
    -> BPOp s f rs a
plugBP' i bp ss us sa = do
    env <- traverse1 resolveRef i
    (res, gFunc) <- liftBase $ backpropWith bp ss us env
    let bpn = BPN { _bpnOut       = FRInternal []
                  , _bpnRes       = res
                  , _bpnGradFunc  = gFunc
                  , _bpnGradCache = Nothing
                  , _bpnSummer    = sa
                  }
    r <- liftBase $ newSTRef bpn
    itraverse1_ (registerRef r) i
    return (BPRNode r)

plugBP
    :: forall s f rs as a. (Every Num (f <$> as), Num (f a))
    => Prod (BPRef s f rs) as
    -> BPOp s f as a
    -> BPOp s f rs a
plugBP i bp = plugBP' i bp (imap1 (\j _ -> known \\ every @_ @Num (reIndex @_ @f j)) i)
                           (imap1 (\j _ -> known \\ every @_ @Num (reIndex @_ @f j)) i)
                           known


inpRef
    :: Index rs a
    -> BPRef s f rs a
inpRef = BPRInp

inpRefs
    :: Known Length rs
    => Prod (BPRef s f rs) rs
inpRefs = map1 inpRef indices

withInps
    :: Known Length rs
    => (Prod (BPRef s f rs) rs -> BP s f rs a)
    -> BP s f rs a
withInps f = f inpRefs










-- | Apply a function to the contents of an STRef, and cache the results
-- using the given lens.  If already calculated, simply returned the cached
-- result.
caching
    :: Lens' a (Maybe b)
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

