{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Backprop
  ( BP
  , BPRef
  , newBPRef
  , newBPRef0
  , newBPRef1
  , newBPRef2
  , newBPRef3
  , backprop
  , plugBP
  , newBPRef'
  , newBPRef1'
  , newBPRef2'
  , newBPRef3'
  , backprop'
  , plugBP'
  , inpRef, inpRefs, withInps
  , Op(..)
  , Summer(..)
  , Unity(..)
  ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Monoid
import           Data.STRef
import           Data.Traversable
import           Data.Type.Combinator
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

newBPRef'
    :: forall s rs as a. ()
    => Prod (BPRef s rs) as
    -> Op as a
    -> Summer a
    -> BP s rs (BPRef s rs a)
newBPRef' i o sm = do
    xs <- traverse1 (fmap I . resolveRef) i
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
    :: (MonadReader (Tuple rs) m, MonadBase (ST s) m)
    => BPRef s rs a
    -> m a
resolveRef = \case
    BPRNode r -> _bpnRes <$> liftBase (readSTRef r)
    BPRInp ix -> getI . index ix <$> ask

registerRef
    :: STRef s (BPNode s rs as b)
    -> Index as a
    -> BPRef s rs a
    -> BP s rs ()
registerRef r ix = \case
    BPRNode r' -> liftBase . modifySTRef r' $ over (bpnOut . _FRInternal) (bpir :)
    BPRInp ix' -> modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
  where
    bpir = BPIR ix r

newBPRef
    :: Num a
    => Prod (BPRef s rs) as
    -> Op as a
    -> BP s rs (BPRef s rs a)
newBPRef i o = newBPRef' i o (Summer sum)

newBPRef0'
    :: Op '[] a
    -> Summer a
    -> BP s rs (BPRef s rs a)
newBPRef0' = newBPRef' Ø

newBPRef0
    :: Num a
    => Op '[] a
    -> BP s rs (BPRef s rs a)
newBPRef0 = newBPRef Ø

newBPRef1'
    :: BPRef s rs a
    -> Op '[a] b
    -> Summer b
    -> BP s rs (BPRef s rs b)
newBPRef1' r = newBPRef' (r :< Ø)

newBPRef1
    :: Num b
    => BPRef s rs a
    -> Op '[a] b
    -> BP s rs (BPRef s rs b)
newBPRef1 r o = newBPRef1' r o known

newBPRef2'
    :: BPRef s rs a
    -> BPRef s rs b
    -> Op '[a,b] c
    -> Summer c
    -> BP s rs (BPRef s rs c)
newBPRef2' rx ry = newBPRef' (rx :< ry :< Ø)

newBPRef2
    :: Num c
    => BPRef s rs a
    -> BPRef s rs b
    -> Op '[a,b] c
    -> BP s rs (BPRef s rs c)
newBPRef2 rx ry o = newBPRef2' rx ry o known

newBPRef3'
    :: BPRef s rs a
    -> BPRef s rs b
    -> BPRef s rs c
    -> Op '[a,b,c] d
    -> Summer d
    -> BP s rs (BPRef s rs d)
newBPRef3' rx ry rz = newBPRef' (rx :< ry :< rz :< Ø)

newBPRef3
    :: Num d
    => BPRef s rs a
    -> BPRef s rs b
    -> BPRef s rs c
    -> Op '[a,b,c] d
    -> BP s rs (BPRef s rs d)
newBPRef3 rx ry rz o = newBPRef3' rx ry rz o known

backwardPass
    :: forall s rs as a. ()
    => STRef s (BPNode s rs as a)
    -> ST s (Tuple as)
backwardPass r = fmap snd . caching bpnGradCache r $ \BPN{..} -> do
    totderv <- case _bpnOut of
      FRInternal rs -> do
        outs <- for rs $ \(BPIR ix r') ->
          getI . index ix <$> backwardPass r'
        return (Just $ runSummer _bpnSummer outs)
      FRExternal gE -> return (Just gE)
      FRTerminal    -> return Nothing
    g <- _bpnGradFunc totderv
    return (totderv, g)

backprop'
    :: (forall s. BP s rs (BPRef s rs a))
    -> Prod Summer rs
    -> Prod Unity rs
    -> Tuple rs
    -> (a, Tuple rs)
backprop' bp ss us env = runST $ do
    (res, gFunc) <- backpropWith bp ss us env
    grad <- gFunc Nothing
    return (res, grad)

backprop
    :: forall rs a. Every Num rs
    => (forall s. BP s rs (BPRef s rs a))
    -> Tuple rs
    -> (a, Tuple rs)
backprop bp xs = backprop' bp (imap1 (\i _ -> known \\ every @_ @Num i) xs)
                              (imap1 (\i _ -> known \\ every @_ @Num i) xs)
                              xs

backpropWith
    :: BP s rs (BPRef s rs a)
    -> Prod Summer rs
    -> Prod Unity rs
    -> Tuple rs
    -> ST s (a, Maybe a -> ST s (Tuple rs))
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
                fmap (I . runSummer s) . for rs' $ \(BPIR ix r') ->
                  getI . index ix <$> backwardPass r'
              FRExternal gE  ->
                return $ I gE
              FRTerminal     ->
                return $ I (getUnity u)
    return (res, gradFunc)

plugBP'
    :: Prod (BPRef s rs) as
    -> BP s as (BPRef s as a)
    -> Prod Summer as
    -> Prod Unity as
    -> Summer a
    -> BP s rs (BPRef s rs a)
plugBP' i bp ss us sa = do
    env <- traverse1 (fmap I . resolveRef) i
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
    :: (Every Num as, Num a)
    => Prod (BPRef s rs) as
    -> BP s as (BPRef s as a)
    -> BP s rs (BPRef s rs a)
plugBP i bp = plugBP' i bp (imap1 (\j _ -> known \\ every @_ @Num j) i)
                           (imap1 (\j _ -> known \\ every @_ @Num j) i)
                           (Summer sum)


inpRef
    :: Index rs a
    -> BPRef s rs a
inpRef = BPRInp

inpRefs
    :: Known Length rs
    => Prod (BPRef s rs) rs
inpRefs = map1 inpRef indices

withInps
    :: Known Length rs
    => (Prod (BPRef s rs) rs -> BP s rs a)
    -> BP s rs a
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

