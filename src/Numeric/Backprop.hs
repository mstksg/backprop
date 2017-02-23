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

-- addEnv
--     :: forall s a as b. ()
--     => BP s as b
--     -> BP s (a ': as) b
-- addEnv (BP (StateT f)) = BP . StateT $ \(BPS (i :< is)) -> do
--     (x, BPS is') <- f (BPS (map1 (over (comp . traverse) g) is))
--     return (x, BPS (i :< map1 (over (comp . traverse) h) is'))
--   where
--     g :: forall c. BPInpRef s (a ': as) c -> BPInpRef s as c
--     g = \case
--       BPIR ix r -> BPIR (IS ix) (_ r)
--     h :: forall c. BPInpRef s as c -> BPInpRef s (a ': as) c
--     h = undefined









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
