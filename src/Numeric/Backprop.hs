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
  , constRef
  , opRef
  , opRef1
  , opRef2
  , opRef3
  , backprop
  , runBPOp
  , partsRef
  , splitGeneric
  , splitRefs
  , internally
  , generically
  , plugBP, (~$)
  , inpRef
  , inpRefs
  , withInps
  , opRef'
  , opRef1'
  , opRef2'
  , opRef3'
  , backprop'
  , runBPOp'
  , partsRef'
  , splitGeneric'
  , splitRefs'
  , internally'
  , generically'
  , plugBP'
  , inpRefs'
  , withInps'
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
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Iso
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Generics.SOP              as SOP

type BPOp s rs a = BP s rs (BPRef s rs a)

opRef'
    :: forall s rs as a. ()
    => Prod (BPRef s rs) as
    -> Op as a
    -> Summer a
    -> BP s rs (BPRef s rs a)
opRef' i o sm = do
    xs <- traverse1 (fmap I . resolveRef) i
    let (res, gf) = runOp' o xs
        bp = BPN { _bpnOut       = FRInternal [] :< Ø
                 , _bpnRes       = only_ res
                 , _bpnGradFunc  = return . gf . head'
                 , _bpnGradCache = Nothing
                 , _bpnSummer    = sm :< Ø
                 }
    r <- liftBase $ newSTRef bp
    itraverse1_ (registerRef r) i
    return (BPRNode IZ r)

splitRefs'
    :: forall s rs as. ()
    => Prod Summer as
    -> Prod Unity as
    -> BPRef s rs (Tuple as)
    -> BP s rs (Prod (BPRef s rs) as)
splitRefs' ss us = partsRef' ss us id

splitRefs
    :: forall s rs as. (Every Num as, Known Length as)
    => BPRef s rs (Tuple as)
    -> BP s rs (Prod (BPRef s rs) as)
splitRefs = partsRef id

partsRef'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BPRef s rs b
    -> BP s rs (Prod (BPRef s rs) bs)
partsRef' ss us l r = do
    x <- resolveRef r
    let xs = view l x
        bp :: BPNode s rs '[b] bs
        bp = BPN { _bpnOut       = map1 (const (FRInternal [])) xs
                 , _bpnRes       = xs
                 , _bpnGradFunc  = return . only_
                                 . review l
                                 . map1 (uncurryFan $ \u ->
                                           maybe (I (getUnity u)) I
                                        )
                                 . zipP us
                 , _bpnGradCache = Nothing
                 , _bpnSummer    = ss
                 }
    r' <- liftBase $ newSTRef bp
    return $ imap1 (\i _ -> BPRNode i r') xs

partsRef
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BPRef s rs b
    -> BP s rs (Prod (BPRef s rs) bs)
partsRef = partsRef' (map1 ((// known) . every @_ @Num) indices)
                     (map1 ((// known) . every @_ @Num) indices)

splitGeneric'
    :: (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> BPRef s rs b
    -> BP s rs (Prod (BPRef s rs) bs)
splitGeneric' ss us = partsRef' ss us genProd

splitGeneric
    :: (Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BPRef s rs b
    -> BP s rs (Prod (BPRef s rs) bs)
splitGeneric = splitGeneric' (map1 ((// known) . every @_ @Num) indices)
                             (map1 ((// known) . every @_ @Num) indices)

internally'
    :: forall s rs bs b a. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> Iso' b (Tuple bs)
    -> BPRef s rs b
    -> BP s bs (BPRef s bs a)
    -> BP s rs (BPRef s rs a)
internally' ss us sa l r bp = do
    xs <- view l <$> resolveRef r
    (res, gFunc) <- liftBase $ backpropWith bp ss us xs
    let bpn :: BPNode s rs '[ b ] '[ a ]
        bpn = BPN { _bpnOut       = FRInternal [] :< Ø
                  , _bpnRes       = only_ res
                  , _bpnGradFunc  = fmap (only_ . review l) . gFunc . head'
                  , _bpnGradCache = Nothing
                  , _bpnSummer    = sa :< Ø
                  }
    r' <- liftBase $ newSTRef bpn
    registerRef r' IZ r
    return (BPRNode IZ r')

internally
    :: forall s rs bs b a. (Every Num bs, Known Length bs, Num a)
    => Iso' b (Tuple bs)
    -> BPRef s rs b
    -> BP s bs (BPRef s bs a)
    -> BP s rs (BPRef s rs a)
internally = internally' (map1 ((// known) . every @_ @Num) indices)
                         (map1 ((// known) . every @_ @Num) indices)
                         known

generically'
    :: forall s rs bs b a. (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> BPRef s rs b
    -> BP s bs (BPRef s bs a)
    -> BP s rs (BPRef s rs a)
generically' ss us sa = internally' ss us sa genProd

generically
    :: forall s rs bs b a. (Num a, Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BPRef s rs b
    -> BP s bs (BPRef s bs a)
    -> BP s rs (BPRef s rs a)
generically = internally genProd

-- TODO: pull summers too
resolveRef
    :: (MonadReader (Tuple rs) m, MonadBase (ST s) m)
    => BPRef s rs a
    -> m a
resolveRef = \case
    BPRNode  ix r -> getI . index ix . _bpnRes <$> liftBase (readSTRef r)
    BPRInp   ix   -> getI . index ix <$> ask
    BPRConst    x -> return x

registerRef
    :: STRef s (BPNode s rs as b)
    -> Index as a
    -> BPRef s rs a
    -> BP s rs ()
registerRef r ix = \case
    BPRNode  ix' r' -> liftBase . modifySTRef r' $
                         over (bpnOut . indexP ix' . _FRInternal) (bpir :)
    BPRInp   ix'    -> modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
    BPRConst _      -> return ()
  where
    bpir = BPIR ix r

opRef
    :: Num a
    => Prod (BPRef s rs) as
    -> Op as a
    -> BP s rs (BPRef s rs a)
opRef i o = opRef' i o known

constRef :: a -> BPRef s rs a 
constRef = BPRConst

opRef1'
    :: BPRef s rs a
    -> Op '[a] b
    -> Summer b
    -> BP s rs (BPRef s rs b)
opRef1' r = opRef' (r :< Ø)

opRef1
    :: Num b
    => BPRef s rs a
    -> Op '[a] b
    -> BP s rs (BPRef s rs b)
opRef1 r o = opRef1' r o known

opRef2'
    :: BPRef s rs a
    -> BPRef s rs b
    -> Op '[a,b] c
    -> Summer c
    -> BP s rs (BPRef s rs c)
opRef2' rx ry = opRef' (rx :< ry :< Ø)

opRef2
    :: Num c
    => BPRef s rs a
    -> BPRef s rs b
    -> Op '[a,b] c
    -> BP s rs (BPRef s rs c)
opRef2 rx ry o = opRef2' rx ry o known

opRef3'
    :: BPRef s rs a
    -> BPRef s rs b
    -> BPRef s rs c
    -> Op '[a,b,c] d
    -> Summer d
    -> BP s rs (BPRef s rs d)
opRef3' rx ry rz = opRef' (rx :< ry :< rz :< Ø)

opRef3
    :: Num d
    => BPRef s rs a
    -> BPRef s rs b
    -> BPRef s rs c
    -> Op '[a,b,c] d
    -> BP s rs (BPRef s rs d)
opRef3 rx ry rz o = opRef3' rx ry rz o known

backwardPass
    :: forall s rs as a. ()
    => STRef s (BPNode s rs as a)
    -> ST s (Tuple as)
backwardPass r = fmap snd . caching bpnGradCache r $ \BPN{..} -> do
    totdervs <- for1 (_bpnSummer `zipP` _bpnOut) $ \case
      s :&: FRInternal rs -> do
        outs <- for rs $ \(BPIR ix r') ->
          getI . index ix <$> backwardPass r'
        return (Just $ runSummer s outs)
      _ :&: FRExternal gE -> return (Just gE)
      _ :&: FRTerminal    -> return Nothing
    g <- _bpnGradFunc totdervs
    return (totdervs, g)

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
            BPRNode  ix sr -> bps0 <$ modifySTRef sr (set (bpnOut . indexP ix) fr)
            BPRInp   ix    -> return $ set (bpsSources . indexP ix) fr bps0
            BPRConst _     -> return bps0
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
    -> Prod Summer as
    -> Prod Unity as
    -> Summer a
    -> BPOp s as a
    -> BPOp s rs a
plugBP' i ss us sa bp = do
    env <- traverse1 (fmap I . resolveRef) i
    (res, gFunc) <- liftBase $ backpropWith bp ss us env
    let bpn = BPN { _bpnOut       = FRInternal [] :< Ø
                  , _bpnRes       = only_ res
                  , _bpnGradFunc  = gFunc . head'
                  , _bpnGradCache = Nothing
                  , _bpnSummer    = sa :< Ø
                  }
    r <- liftBase $ newSTRef bpn
    itraverse1_ (registerRef r) i
    return (BPRNode IZ r)

plugBP
    :: forall s rs as a. (Every Num as, Num a)
    => Prod (BPRef s rs) as
    -> BPOp s as a
    -> BPOp s rs a
plugBP i = plugBP' i (imap1 (\j _ -> known \\ every @_ @Num j) i)
                     (imap1 (\j _ -> known \\ every @_ @Num j) i)
                     known

infixr 1 ~$
(~$)
    :: (Every Num as, Num a)
    => BPOp s as a
    -> Prod (BPRef s rs) as
    -> BPOp s rs a
(~$) = flip plugBP

inpRef
    :: Index rs a
    -> BPRef s rs a
inpRef = BPRInp

inpRefs
    :: Known Length rs
    => Prod (BPRef s rs) rs
inpRefs = inpRefs' known

inpRefs'
    :: Length rs
    -> Prod (BPRef s rs) rs
inpRefs' = map1 inpRef . indices'

withInps'
    :: Length rs
    -> (Prod (BPRef s rs) rs -> BP s rs a)
    -> BP s rs a
withInps' l f = f (inpRefs' l)

withInps
    :: Known Length rs
    => (Prod (BPRef s rs) rs -> BP s rs a)
    -> BP s rs a
withInps = withInps' known










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

