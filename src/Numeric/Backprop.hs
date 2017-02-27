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
  , newBPRef, newBPRef'
  , newBPRef0
  , newBPRef1, newBPRef1'
  , newBPRef2, newBPRef2'
  , newBPRef3, newBPRef3'
  , backprop, backprop'
  , inpRef, inpRefs, withInps
  , Op(..)
  , Summer(..)
  , Unity(..)
  ) where

import           Control.Applicative
import           Control.Monad.Base
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
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

newBPRef
    :: forall s rs as a. ()
    => Prod (BPRef s rs) as
    -> Op as a
    -> Summer a
    -> BP s rs (BPRef s rs a)
newBPRef i o sm = do
    r <- liftBase $ newSTRef bp
    ifor1_ i $ \ix bpr' -> do
      let bpir = BPIR ix r
      case bpr' of
        BPRNode r' -> liftBase $ modifySTRef r' (over (bpnOut . _FRInternal) (bpir:))
        BPRInp ix' -> modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
    return (BPRNode r)
  where
    bp :: BPNode s rs as a
    bp = BPN { _bpnInp       = i
             , _bpnOut       = FRInternal []
             , _bpnOp        = o
             , _bpnResCache  = Nothing
             , _bpnGradCache = Nothing
             , _bpnSummer    = sm
             }

newBPRef'
    :: Num a
    => Prod (BPRef s rs) as
    -> Op as a
    -> BP s rs (BPRef s rs a)
newBPRef' i o = newBPRef i o (Summer sum)

newBPRef0
    :: Op '[] a
    -> Summer a
    -> BP s rs (BPRef s rs a)
newBPRef0 = newBPRef Ø

newBPRef1
    :: BPRef s rs a
    -> Op '[a] b
    -> Summer b
    -> BP s rs (BPRef s rs b)
newBPRef1 r = newBPRef (r :< Ø)

newBPRef1'
    :: Num b
    => BPRef s rs a
    -> Op '[a] b
    -> BP s rs (BPRef s rs b)
newBPRef1' r o = newBPRef1 r o known

newBPRef2
    :: BPRef s rs a
    -> BPRef s rs b
    -> Op '[a,b] c
    -> Summer c
    -> BP s rs (BPRef s rs c)
newBPRef2 rx ry = newBPRef (rx :< ry :< Ø)

newBPRef2'
    :: Num c
    => BPRef s rs a
    -> BPRef s rs b
    -> Op '[a,b] c
    -> BP s rs (BPRef s rs c)
newBPRef2' rx ry o = newBPRef2 rx ry o known

newBPRef3
    :: BPRef s rs a
    -> BPRef s rs b
    -> BPRef s rs c
    -> Op '[a,b,c] d
    -> Summer d
    -> BP s rs (BPRef s rs d)
newBPRef3 rx ry rz = newBPRef (rx :< ry :< rz :< Ø)

newBPRef3'
    :: Num d
    => BPRef s rs a
    -> BPRef s rs b
    -> BPRef s rs c
    -> Op '[a,b,c] d
    -> BP s rs (BPRef s rs d)
newBPRef3' rx ry rz o = newBPRef3 rx ry rz o known

forwardPass
    :: Tuple rs
    -> BPRef s rs a
    -> ST s a
forwardPass env = \case
    BPRNode r -> fmap fst . caching bpnResCache r $ \BPN{..} ->
      runOp' _bpnOp <$> traverse1 (fmap I . forwardPass env) _bpnInp
    BPRInp ix -> return . getI $ index ix env

backwardPass
    :: forall s rs as a. ()
    => STRef s (BPNode s rs as a)
    -> ST s (Tuple as)
backwardPass r = fmap snd . caching bpnGradCache r $ \BPN{..} -> do
    totderv <- for (view frMaybe _bpnOut) $ \outrefs -> do
      outs <- for outrefs $ \(BPIR ix r') ->
        getI . index ix <$> backwardPass r'
      return (runSummer _bpnSummer outs)
    return $ case snd <$> _bpnResCache of
      Just gs -> (totderv, gs totderv)
      Nothing -> error "backwards pass before forwards pass"
      -- can we just do the filling in here and so not require a separate
      -- forward pass at all?

backprop
    :: (forall s. BP s rs (BPRef s rs a))
    -> Prod Summer rs
    -> Prod Unity rs
    -> Tuple rs
    -> (a, Tuple rs)
backprop bp ss us env = runST $ do
    (r, bps0) <- runStateT (bpST bp) $ BPS (map1 (\_ -> FRInternal []) env)
    res  <- forwardPass env r
    -- set "out" lists to Nothing if is the output
    BPS{..} <- liftBase $ case r of
      BPRNode sr -> bps0 <$ modifySTRef sr (set bpnOut FRTerminal)
      BPRInp  ix -> return $ set (bpsSources . indexP ix) FRTerminal bps0
    gradLists <- for1 _bpsSources $ \rs ->
      fmap Comp . for (view frMaybe rs) $ \rs' ->
        for rs' $ \(BPIR ix r') ->
          getI . index ix <$> backwardPass r'
    let sug  = ss `zipP` us `zipP` gradLists
        grad = map1 (\((s :&: u) :&: Comp xs) -> I $
                        case xs of
                          Nothing  -> getUnity u
                          Just xs' -> runSummer s xs'
                    ) sug
    return (res, grad)

backprop'
    :: forall rs a. Every Num rs
    => (forall s. BP s rs (BPRef s rs a))
    -> Tuple rs
    -> (a, Tuple rs)
backprop' bp xs = backprop bp (imap1 (\i _ -> known \\ every @_ @Num i) xs)
                              (imap1 (\i _ -> known \\ every @_ @Num i) xs)
                              xs

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

zipP
    :: Prod f as
    -> Prod g as
    -> Prod (f :&: g) as
zipP = \case
    Ø -> \case
      Ø       -> Ø
    x :< xs -> \case
      y :< ys -> x :&: y :< zipP xs ys

indexP :: Index as a -> Lens' (Prod g as) (g a)
indexP = \case
    IZ   -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> indexP i f xs
