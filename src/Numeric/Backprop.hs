{-# LANGUAGE FlexibleContexts    #-}
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
  , newBPRef, newBPRef'
  , newBPRef0
  , newBPRef1, newBPRef1'
  , newBPRef2, newBPRef2'
  , newBPRef3, newBPRef3'
  , backprop, backprop'
  , inpRef, inpRefs, withInps
  , Op(..)
  , Summer(..)
  , Scaler(..)
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
import           Data.Type.Length
import           Data.Type.Product
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Type.Class.Higher
import           Type.Class.Known

newBPRef
    :: forall s as a bs. ()
    => Prod (BPRef s bs) as
    -> Op as a
    -> Prod (Scaler a) as
    -> Summer a
    -> BP s bs (BPRef s bs a)
newBPRef i o sc sm = do
    r <- liftBase $ newSTRef bp
    ifor1_ i $ \ix bpr' -> do
      let bpir = BPIR ix r
      case bpr' of
        BPRNode r' -> liftBase $ modifySTRef r' (over (bpnOut . _FRInternal) (bpir:))
        BPRInp ix' -> modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
    return (BPRNode r)
  where
    bp :: BPNode s bs as a
    bp = BPN { _bpnInp       = i
             , _bpnOut       = FRInternal []
             , _bpnOp        = o
             , _bpnResCache  = Nothing
             , _bpnGradCache = Nothing
             , _bpnSummer    = sm
             , _bpnScaler    = sc
             }

newBPRef'
    :: Num a
    => Prod (BPRef s bs) as
    -> Op as a
    -> Prod (Scaler a) as
    -> BP s bs (BPRef s bs a)
newBPRef' i o s = newBPRef i o s (Summer sum)

newBPRef0
    :: Op '[] a
    -> Summer a
    -> BP s as (BPRef s as a)
newBPRef0 o = newBPRef Ø o Ø

newBPRef1
    :: BPRef s as a
    -> Op '[a] b
    -> Scaler b a
    -> Summer b
    -> BP s as (BPRef s as b)
newBPRef1 r o s = newBPRef (r :< Ø) o (s :< Ø)

newBPRef1'
    :: Num b
    => BPRef s as a
    -> Op '[a] b
    -> Scaler b a
    -> BP s as (BPRef s as b)
newBPRef1' r o s = newBPRef1 r o s known

newBPRef2
    :: BPRef s bs a
    -> BPRef s bs b
    -> Op '[a,b] c
    -> Scaler c a
    -> Scaler c b
    -> Summer c
    -> BP s bs (BPRef s bs c)
newBPRef2 rx ry o sx sy = newBPRef (rx :< ry :< Ø) o (sx :< sy :< Ø)

newBPRef2'
    :: Num c
    => BPRef s bs a
    -> BPRef s bs b
    -> Op '[a,b] c
    -> Scaler c a
    -> Scaler c b
    -> BP s bs (BPRef s bs c)
newBPRef2' rx ry o sx sy = newBPRef2 rx ry o sx sy known

newBPRef3
    :: BPRef s bs a
    -> BPRef s bs b
    -> BPRef s bs c
    -> Op '[a,b,c] d
    -> Scaler d a
    -> Scaler d b
    -> Scaler d c
    -> Summer d
    -> BP s bs (BPRef s bs d)
newBPRef3 rx ry rz o sx sy sz =
    newBPRef (rx :< ry :< rz :< Ø) o (sx :< sy :< sz :< Ø)

newBPRef3'
    :: Num d
    => BPRef s bs a
    -> BPRef s bs b
    -> BPRef s bs c
    -> Op '[a,b,c] d
    -> Scaler d a
    -> Scaler d b
    -> Scaler d c
    -> BP s bs (BPRef s bs d)
newBPRef3' rx ry rz o sx sy sz = newBPRef3 rx ry rz o sx sy sz known

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
    totderv <- for (view frMaybe _bpnOut) $ \outrefs -> do
      outs <- for outrefs $ \bpir -> do
        BPIR (ix :: Index cs a) (r' :: STRef s (BPNode s bs cs c)) <- return bpir
        getI . index ix <$> backwardPass r'
      return (runSummer _bpnSummer outs)
    case snd <$> _bpnResCache of
      Just gs  -> do
        let gradProd = case totderv of
              Just td -> zipWithP (\s g -> runScaler s td <$> g) _bpnScaler gs
              Nothing -> gs
        return (totderv, gradProd)
      Nothing -> error "backwards pass before forwards pass"
      -- can we just do the filling in here and so not require a separate
      -- forward pass at all?

-- TODO: restrict output refs to not being one of the inputs lol
backprop
    :: (forall s. BP s as (BPRef s as a))
    -> Prod Summer as
    -> Tuple as
    -> (a, Tuple as)
backprop bp ss env = runST $ do
    (r, bps0) <- runStateT (bpST bp) $ BPS (map1 (\_ -> FRInternal []) env)
    res  <- forwardPass env r
    -- set "out" lists to Nothing if is the output
    BPS{..} <- liftBase $ case r of
      BPRNode sr -> bps0 <$ modifySTRef sr (set bpnOut FRTerminal)
      BPRInp  ix -> return $ set (bpsSources . indexP ix) FRTerminal bps0
    gradLists <- for1 _bpsSources $ \rs ->
      fmap Comp . for (view frMaybe rs) $ \rs' ->
        for rs' $ \bpir -> do
          BPIR (ix :: Index cs a) (r' :: STRef s (BPNode s bs cs c)) <- return bpir
          getI . index ix <$> backwardPass r'
    let grad = zipWithP (\s (Comp xs) ->
                            case xs of
                              Nothing  -> I undefined
                              Just xs' -> I (runSummer s xs')
                        ) ss gradLists
    return (res, grad)

backprop'
    :: Known (Prod Summer) as
    => (forall s. BP s as (BPRef s as a))
    -> Tuple as
    -> (a, Tuple as)
backprop' bp = backprop bp known

inpRef
    :: Index as a
    -> BPRef s as a
inpRef = BPRInp

inpRefs
    :: Known Length as
    => Prod (BPRef s as) as
inpRefs = map1 BPRInp indices

withInps
    :: Known Length as
    => (Prod (BPRef s as) as -> BP s as a)
    -> BP s as a
withInps f = f (map1 BPRInp indices)










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

indexP :: Index as a -> Lens' (Prod g as) (g a)
indexP = \case
    IZ   -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> indexP i f xs
