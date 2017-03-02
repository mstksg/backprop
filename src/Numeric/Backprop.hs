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
  , runBPOp
  , splitRefs
  , internally
  , plugBP
  , newBPRef'
  , newBPRef0'
  , newBPRef1'
  , newBPRef2'
  , newBPRef3'
  , backprop'
  , runBPOp'
  , splitRefs'
  , internally'
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
import           Data.Foldable
import           Data.Maybe
import           Data.STRef
import           Data.Traversable
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Product         as TCP
import           Data.Type.Util
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

type BPOp s rs a = BP s rs (BPRef s rs a)

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

splitRefs'
    :: forall s rs as. ()
    => Prod Summer as
    -> Prod Unity as
    -> BPRef s rs (Tuple as)
    -> BP s rs (Prod (BPRef s rs) as)
splitRefs' ss us r = do
    xs <- resolveRef r
    let noths = map1 (const Nothing) xs
        empts = map1 (const []) xs
    rs <- ifor1 (ss `zipP` us `zipP` xs) $ \(ix :: Index as a) ((s :&: u) :&: I x) -> do
      let bp :: BPNode s rs '[ Prod Maybe as ] a
          bp = BPN { _bpnOut       = FRInternal []
                   , _bpnRes       = x
                   , _bpnGradFunc  = \case
                        Nothing -> return . only_ $ set (indexP ix) (Just (getUnity u)) noths
                        Just g  -> return . only_ $ set (indexP ix) (Just g) noths
                   , _bpnGradCache = Nothing
                   , _bpnSummer    = s
                   }
      Comp <$> liftBase (newSTRef bp)
    let master :: BPNode s rs '[Tuple as] (Prod Maybe as)
        master = BPN { _bpnOut = FRInternal $ TCP.toList (BPIR IZ . getComp) rs
                     , _bpnRes = map1 (Just . getI) xs
                     , _bpnGradFunc = \case
                          Nothing -> return . only_ $ map1 (I . getUnity) us
                          Just g  -> return . only_ $
                            map1 (\(s :&: g') -> I $ runSummer s (maybeToList g'))
                                 (ss `zipP` g)
                     , _bpnGradCache = Nothing
                     , _bpnSummer    = Summer $ map1 (\(s :&: ys) -> Just $ runSummer s ys)
                                              . zipP ss
                                              . foldl' folder empts
                                              . map (map1 maybeToList)
                     }
    r' <- liftBase $ newSTRef master
    registerRef r' IZ r
    return $ map1 (BPRNode . getComp) rs
  where
    folder :: Prod [] as -> Prod [] as -> Prod [] as
    folder xs = map1 (uncurryFan (++)) . zipP xs

splitRefs
    :: forall s rs as. (Every Num as, Known Length as)
    => BPRef s rs (Tuple as)
    -> BP s rs (Prod (BPRef s rs) as)
splitRefs = splitRefs' (map1 ((// known) . every @_ @Num) indices)
                       (map1 ((// known) . every @_ @Num) indices)

internally'
    :: forall s rs bs b a. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> (b -> Tuple bs)
    -> (Tuple bs -> b)
    -> BPRef s rs b
    -> BP s bs (BPRef s bs a)
    -> BP s rs (BPRef s rs a)
internally' ss us sa f g r bp = do
    xs <- f <$> resolveRef r
    (res, gFunc) <- liftBase $ backpropWith bp ss us xs
    let bpn :: BPNode s rs '[ b ] a
        bpn = BPN { _bpnOut       = FRInternal []
                  , _bpnRes       = res
                  , _bpnGradFunc  = fmap (only_ . g) . gFunc
                  , _bpnGradCache = Nothing
                  , _bpnSummer    = sa
                  }
    r' <- liftBase $ newSTRef bpn
    registerRef r' IZ r
    return (BPRNode r')

internally
    :: forall s rs bs b a. (Every Num bs, Known Length bs, Num a)
    => (b -> Tuple bs)
    -> (Tuple bs -> b)
    -> BPRef s rs b
    -> BP s bs (BPRef s bs a)
    -> BP s rs (BPRef s rs a)
internally = internally' (map1 ((// known) . every @_ @Num) indices)
                         (map1 ((// known) . every @_ @Num) indices)
                         known

-- TODO: pull summers too
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
newBPRef i o = newBPRef' i o known

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
    :: (forall s. BPOp s rs a)
    -> Prod (Summer) rs
    -> Prod (Unity) rs
    -> Tuple rs
    -> (a, Tuple rs)
backprop' bp ss us env = runST $ do
    (res, gFunc) <- backpropWith bp ss us env
    grad <- gFunc Nothing
    return (res, grad)

backprop
    :: forall rs a. Every Num rs
    => (forall s. BPOp s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop bp xs = backprop' bp (imap1 (\i _ -> known \\ every @_ @Num i) xs)
                              (imap1 (\i _ -> known \\ every @_ @Num i) xs)
                              xs

runBPOp'
    :: (forall s. BPOp s rs a)
    -> Prod (Summer) rs
    -> Prod (Unity) rs
    -> Tuple rs
    -> a
runBPOp' bp ss us = fst . backprop' bp ss us

runBPOp
    :: Every Num rs
    => (forall s. BPOp s rs a)
    -> Tuple rs
    -> a
runBPOp bp = fst . backprop bp


backpropWith
    :: BPOp s rs a
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
            I <$> case rs of
              FRInternal rs' ->
                fmap (runSummer s) . for rs' $ \(BPIR ix r') ->
                  getI . index ix <$> backwardPass r'
              FRExternal gE  ->
                return $ gE
              FRTerminal     ->
                return $ getUnity u
    return (res, gradFunc)

plugBP'
    :: Prod (BPRef s rs) as
    -> BPOp s as a
    -> Prod (Summer) as
    -> Prod (Unity) as
    -> Summer a
    -> BPOp s rs a
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
    :: forall s rs as a. (Every Num as, Num a)
    => Prod (BPRef s rs) as
    -> BPOp s as a
    -> BPOp s rs a
plugBP i bp = plugBP' i bp (imap1 (\j _ -> known \\ every @_ @Num j) i)
                           (imap1 (\j _ -> known \\ every @_ @Num j) i)
                           known


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

