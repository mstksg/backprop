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

module Numeric.Backprop (
  -- * Types
    BP, BPOp, BPOpI, BRef, Op, OpB
  -- * BP
  -- ** Backprop
  , backprop, evalBPOp, gradBPOp, bpOp
  , backprop', evalBPOp', gradBPOp', bpOp'
  -- ** Inputs
  , withInps, implicitly
  , plugBP, (~$), ($~)
  , withInps', implicitly'
  , plugBP'
  -- * Refs
  , constRef
  , inpRef, inpRefs
  , bindRef
  , inpRefs'
  , bindRef'
  -- ** From Ops
  , opRef, (-$)
  , opRef1, opRef2, opRef3
  , opRef'
  , opRef1', opRef2', opRef3'
  -- ** Ref manipulation
  -- *** As parts
  , partsRef, (#<~), withParts
  , splitRefs, gSplit
  , partsRef', withParts'
  , splitRefs', gSplit'
  -- *** As sums
  , choicesRef
  , choicesRef'
  -- *** As sums of products
  , sopRef, gSplits
  , sopRef', gSplits'
  -- ** Transforming BP
  , internally
  , generically
  , internally'
  , generically'
  -- ** Combining
  , liftR, liftR1, liftR2, liftR3
  -- * Op
  , op1, op2, op3, opN
  , op1', op2', op3', opN'
  -- * Utility
  , Prod(..), pattern (:>), only, head'
  , Tuple, pattern (::<), only_
  , Summer(..), summers, summers'
  , Unity(..), unities, unities'
  ) where

import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.STRef
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Sum hiding      (index)
import           Data.Type.Util
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Generics.SOP              as SOP

type BPOp s rs a  = BP s rs (BRef s rs a)
type BPOpI s rs a = Prod (BRef s rs) rs -> BRef s rs a

opRef'
    :: forall s rs as a. ()
    => Summer a
    -> Prod (BRef s rs) as
    -> OpB s as a
    -> BP s rs (BRef s rs a)
opRef' s i o = do
    xs <- traverse1 (fmap I . BP . resolveRef) i
    (res, gf) <- BP . liftBase $ runOpM' o xs
    let bp = BPN { _bpnOut       = only $ FRInternal []
                 , _bpnRes       = only_ res
                 , _bpnGradFunc  = gf . head'
                 , _bpnGradCache = Nothing
                 , _bpnSummer    = only s
                 }
    r <- BP . liftBase $ newSTRef bp
    itraverse1_ (registerRef . flip IRNode r) i
    return (BRNode IZ r)

splitRefs'
    :: forall s rs as. ()
    => Prod Summer as
    -> Prod Unity as
    -> BRef s rs (Tuple as)
    -> BP s rs (Prod (BRef s rs) as)
splitRefs' ss us = partsRef' ss us id

splitRefs
    :: forall s rs as. (Every Num as, Known Length as)
    => BRef s rs (Tuple as)
    -> BP s rs (Prod (BRef s rs) as)
splitRefs = partsRef id

partsRef'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BRef s rs b
    -> BP s rs (Prod (BRef s rs) bs)
partsRef' ss us i =
    fmap (view sum1) . sopRef' (only ss) (only us) (i . resum1)

partsRef
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BRef s rs b
    -> BP s rs (Prod (BRef s rs) bs)
partsRef = partsRef' summers unities

infixr 1 #<~
(#<~)
    :: (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BRef s rs b
    -> BP s rs (Prod (BRef s rs) bs)
(#<~) = partsRef

withParts'
    :: Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BRef s rs b
    -> (Prod (BRef s rs) bs -> BP s rs a)
    -> BP s rs a
withParts' ss us i r f = do
    p <- partsRef' ss us i r
    f p

withParts
    :: (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BRef s rs b
    -> (Prod (BRef s rs) bs -> BP s rs a)
    -> BP s rs a
withParts i r f = do
    p <- partsRef i r
    f p

gSplit'
    :: (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> BRef s rs b
    -> BP s rs (Prod (BRef s rs) bs)
gSplit' ss us = partsRef' ss us gTuple

gSplit
    :: (Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BRef s rs b
    -> BP s rs (Prod (BRef s rs) bs)
gSplit = gSplit' summers unities

internally'
    :: forall s rs bs b a. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> Iso' b (Tuple bs)
    -> BRef s rs b
    -> BP s bs (BRef s bs a)
    -> BP s rs (BRef s rs a)
internally' ss us sa l r bp = do
    xs <- view l <$> BP (resolveRef r)
    (res, gFunc) <- BP . liftBase $ backpropWith ss us bp xs
    let bpn :: BPNode s rs '[ b ] '[ a ]
        bpn = BPN { _bpnOut       = only $ FRInternal []
                  , _bpnRes       = only_ res
                  , _bpnGradFunc  = fmap (only_ . review l) . gFunc . head'
                  , _bpnGradCache = Nothing
                  , _bpnSummer    = only sa
                  }
    r' <- BP . liftBase $ newSTRef bpn
    registerRef (IRNode IZ r') r
    return (BRNode IZ r')

internally
    :: forall s rs bs b a. (Every Num bs, Known Length bs, Num a)
    => Iso' b (Tuple bs)
    -> BRef s rs b
    -> BP s bs (BRef s bs a)
    -> BP s rs (BRef s rs a)
internally = internally' summers unities known

generically'
    :: forall s rs bs b a. (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> BRef s rs b
    -> BP s bs (BRef s bs a)
    -> BP s rs (BRef s rs a)
generically' ss us sa = internally' ss us sa gTuple

generically
    :: forall s rs bs b a. (Num a, Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BRef s rs b
    -> BP s bs (BRef s bs a)
    -> BP s rs (BRef s rs a)
generically = internally gTuple

choicesRef'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Sum I bs)
    -> BRef s rs b
    -> BP s rs (Sum (BRef s rs) bs)
choicesRef' ss us i r = do
    x <- BP $ resolveRef r
    let xs :: Sum I bs
        xs = view i x
    ifor1 ((ss `zipP` us) `tagSum` xs) $ \ix ((s :&: u) :&: I (y :: c)) -> do
      let bp :: BPNode s rs '[b] '[c]
          bp = BPN { _bpnOut       = only $ FRInternal []
                   , _bpnRes       = only_ y
                   , _bpnGradFunc  = return . only_ . review i
                                   . injectSum ix
                                   . maybe (I (getUnity u)) I
                                   . head'
                   , _bpnGradCache = Nothing
                   , _bpnSummer    = only s
                   }
      r' <- BP . liftBase $ newSTRef bp
      registerRef (IRNode IZ r') r
      return $ BRNode IZ r'
-- TODO: cannot implement via sopRef?  oh well.

choicesRef
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Sum I bs)
    -> BRef s rs b
    -> BP s rs (Sum (BRef s rs) bs)
choicesRef = choicesRef' summers unities

sopRef'
    :: forall s rs bss b. ()
    => Prod (Prod Summer) bss
    -> Prod (Prod Unity) bss
    -> Iso' b (Sum Tuple bss)
    -> BRef s rs b
    -> BP s rs (Sum (Prod (BRef s rs)) bss)
sopRef' sss uss i r = do
    x <- BP $ resolveRef r
    let xs :: Sum Tuple bss
        xs = view i x
    ifor1 ((sss `zipP` uss) `tagSum` xs) $ \ix ((ss :&: us) :&: (ys :: Tuple bs)) -> do
      let bp :: BPNode s rs '[b] bs
          bp = BPN { _bpnOut       = map1 (const (FRInternal [])) ys
                   , _bpnRes       = ys
                   , _bpnGradFunc  = return . only_
                                   . review i . injectSum ix
                                   . map1 (uncurryFan $ \u ->
                                             maybe (I (getUnity u)) I
                                          )
                                   . zipP us
                   , _bpnGradCache = Nothing
                   , _bpnSummer    = ss
                   }
      r' <- BP . liftBase $ newSTRef bp
      registerRef (IRNode IZ r') r
      return $ imap1 (\ix' _ -> BRNode ix' r') ys

sopRef
    :: forall s rs bss b. (Known Length bss, Every (Every Num ∧ Known Length) bss)
    => Iso' b (Sum Tuple bss)
    -> BRef s rs b
    -> BP s rs (Sum (Prod (BRef s rs)) bss)
sopRef = sopRef' (withEvery @(Every Num ∧ Known Length) summers)
                 (withEvery @(Every Num ∧ Known Length) unities)

gSplits'
    :: forall s rs b. SOP.Generic b
    => Prod (Prod Summer) (SOP.Code b)
    -> Prod (Prod Unity) (SOP.Code b)
    -> BRef s rs b
    -> BP s rs (Sum (Prod (BRef s rs)) (SOP.Code b))
gSplits' sss uss = sopRef' sss uss gSOP

gSplits
    :: forall s rs b.
      ( SOP.Generic b
      , Known Length (SOP.Code b)
      , Every (Every Num ∧ Known Length) (SOP.Code b)
      )
    => BRef s rs b
    -> BP s rs (Sum (Prod (BRef s rs)) (SOP.Code b))
gSplits = sopRef gSOP


-- TODO: pull summers too
resolveRef
    :: (MonadReader (Tuple rs) m, MonadBase (ST s) m)
    => BRef s rs a
    -> m a
resolveRef = \case
    BRNode  ix r -> getI . index ix . _bpnRes <$> liftBase (readSTRef r)
    BRInp   ix   -> getI . index ix <$> ask
    BRConst    x -> return x
    BROp    rs o -> do
      xs <- traverse1 (fmap I . resolveRef) rs
      liftBase $ runOpM o xs

registerRef
    :: forall s rs a. ()
    => BPInpRef s rs a
    -> BRef s rs a
    -> BP s rs ()
registerRef bpir = \case
    BRNode  ix' r' -> BP . liftBase . modifySTRef r' $
                        over (bpnOut . indexP ix' . _FRInternal) (bpir :)
    BRInp   ix'    -> BP $ modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
    BRConst _      -> return ()
    -- This independently makes a new BPPipe for every usage site of the
    -- BROp, so it's a bit inefficient.
    BROp    (rs :: Prod (BRef s rs) ds) (o :: OpM (ST s) ds a) -> do
      xs :: Tuple ds <- traverse1 (fmap I . BP . resolveRef) rs
      (res, gF) <- BP . liftBase $ runOpM' o xs
      let bpp :: BPPipe s rs ds '[a]
          bpp = BPP { _bppOut       = only bpir
                    , _bppRes       = only_ res
                    , _bppGradFunc  = gF . Just . getI . head'
                    , _bppGradCache = Nothing
                    }
      r' <- BP . liftBase $ newSTRef bpp
      ifor1_ rs $ \ix' (bpr :: BRef s rs d) ->
        registerRef (IRPipe ix' r') bpr

opRef
    :: Num a
    => Prod (BRef s rs) as
    -> Op as a
    -> BP s rs (BRef s rs a)
opRef = opRef' known

infixr 1 -$
(-$)
    :: Num a
    => Op as a
    -> Prod (BRef s rs) as
    -> BP s rs (BRef s rs a)
o -$ xs = opRef xs o

constRef :: a -> BRef s rs a
constRef = BRConst

opRef1'
    :: Summer b
    -> BRef s rs a
    -> Op '[a] b
    -> BP s rs (BRef s rs b)
opRef1' s r = opRef' s (r :< Ø)

opRef1
    :: Num b
    => BRef s rs a
    -> Op '[a] b
    -> BP s rs (BRef s rs b)
opRef1 = opRef1' known

opRef2'
    :: Summer c
    -> BRef s rs a
    -> BRef s rs b
    -> Op '[a,b] c
    -> BP s rs (BRef s rs c)
opRef2' s rx ry = opRef' s (rx :< ry :< Ø)

opRef2
    :: Num c
    => BRef s rs a
    -> BRef s rs b
    -> Op '[a,b] c
    -> BP s rs (BRef s rs c)
opRef2 = opRef2' known

opRef3'
    :: Summer d
    -> BRef s rs a
    -> BRef s rs b
    -> BRef s rs c
    -> Op '[a,b,c] d
    -> BP s rs (BRef s rs d)
opRef3' s rx ry rz = opRef' s (rx :< ry :< rz :< Ø)

opRef3
    :: Num d
    => BRef s rs a
    -> BRef s rs b
    -> BRef s rs c
    -> Op '[a,b,c] d
    -> BP s rs (BRef s rs d)
opRef3 = opRef3' known

-- can be recursive too?  would have to have resolveRef also pull summers
bindRef'
    :: Summer a
    -> BRef s rs a
    -> BP s rs (BRef s rs a)
bindRef' s r = case r of
    BRNode  _  _ -> return r
    BRInp   _    -> return r
    BRConst _    -> return r
    BROp    rs o -> opRef' s rs o

bindRef
    :: Num a
    => BRef s rs a
    -> BP s rs (BRef s rs a)
bindRef = bindRef' known



backwardPass
    :: forall s rs a. ()
    => BPInpRef s rs a
    -> ST s a
backwardPass = \case
    IRNode  ix r' -> getI . index ix <$> pullNode r'
    IRPipe  ix r' -> getI . index ix <$> pullPipe r'
    IRConst g     -> return g
  where
    pullNode
        :: forall as bs. ()
        => STRef s (BPNode s rs as bs)
        -> ST s (Tuple as)
    pullNode r = caching bpnGradCache r $ \BPN{..} -> do
        totdervs <- for1 (_bpnSummer `zipP` _bpnOut) $ \case
          s :&: FRInternal rs -> Just . runSummer s
              <$> traverse backwardPass rs
          _ :&: FRTerminal g   -> return g
        g <- _bpnGradFunc totdervs
        return g
    pullPipe
        :: forall as bs. ()
        => STRef s (BPPipe s rs as bs)
        -> ST s (Tuple as)
    pullPipe r = caching bppGradCache r $ \BPP{..} ->
        _bppGradFunc =<< traverse1 (fmap I . backwardPass) _bppOut

backprop'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop' ss us bp env = runST $ do
    (res, gFunc) <- backpropWith ss us bp env
    grad <- gFunc Nothing
    return (res, grad)

backprop
    :: forall rs a. Every Num rs
    => (forall s. BPOp s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop bp xs = backprop' (summers' l) (unities' l) bp xs
  where
    l :: Length rs
    l = prodLength xs

bpOp'
    :: Prod Summer as
    -> Prod Unity as
    -> BPOp s as a
    -> OpB s as a
bpOp' ss us bp = OpM $ backpropWith ss us bp

bpOp
    :: (Every Num as, Known Length as)
    => BPOp s as a
    -> OpB s as a
bpOp = bpOp' summers unities

evalBPOp'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)
    -> Tuple rs
    -> a
evalBPOp' ss us bp = fst . backprop' ss us bp

evalBPOp
    :: Every Num rs
    => (forall s. BPOp s rs a)
    -> Tuple rs
    -> a
evalBPOp bp = fst . backprop bp

gradBPOp'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)
    -> Tuple rs
    -> Tuple rs
gradBPOp' ss us bp = snd . backprop' ss us bp

gradBPOp
    :: Every Num rs
    => (forall s. BPOp s rs a)
    -> Tuple rs
    -> Tuple rs
gradBPOp bp = snd . backprop bp


closeOff
    :: (MonadReader (Tuple rs) m, MonadState (BPState s rs) m, MonadBase (ST s) m)
    => Bool
    -> Maybe a
    -> BRef s rs a
    -> m ()
closeOff isTerminal gOut = \case
    BRNode  ix sr -> liftBase $ modifySTRef sr (over (bpnOut . indexP ix) (<> fr))
    BRInp   ix'   -> modifying (bpsSources . indexP ix') (<> fr)
    BRConst _     -> return ()
    BROp    rs o  -> do
      xs <- traverse1 (fmap I . resolveRef) rs
      gs <- liftBase $ gradOpWithM' o xs gOut
      for1_ (gs `zipP` rs) $ \(I g :&: r) ->
        closeOff False (Just g) r
  where
    fr | isTerminal = FRTerminal gOut
       | otherwise  = FRInternal (IRConst <$> maybeToList gOut)

backpropWith
    :: Prod Summer rs
    -> Prod Unity rs
    -> BPOp s rs a
    -> Tuple rs
    -> ST s (a, Maybe a -> ST s (Tuple rs))
backpropWith ss us bp env = do
    (r, bps0) <- runStateT (runReaderT (bpST bp) env)
                           (BPS (map1 (\_ -> FRInternal []) env))
    res <- runReaderT (resolveRef r) env
    let gradFunc gradOut = do
          BPS{..} <- execStateT (runReaderT (closeOff True gradOut r) env) bps0
          for1 (ss `zipP` us `zipP` _bpsSources) $ \((s :&: u) :&: rs) -> do
            I <$> case rs of
              FRInternal rs' -> runSummer s <$> traverse backwardPass rs'
              FRTerminal g   -> return $ fromMaybe (getUnity u) g
    return (res, gradFunc)

implicitly'
    :: Length rs
    -> BPOpI s rs a
    -> BPOp s rs a
implicitly' l f = withInps' l (return . f)

implicitly
    :: Known Length rs
    => BPOpI s rs a
    -> BPOp s rs a
implicitly = implicitly' known

plugBP'
    :: Prod (BRef s rs) as
    -> Prod Summer as
    -> Prod Unity as
    -> Summer a
    -> BPOp s as a
    -> BPOp s rs a
plugBP' i ss us sa bp = opRef' sa i $ bpOp' ss us bp

plugBP
    :: forall s rs as a. (Every Num as, Num a)
    => Prod (BRef s rs) as
    -> BPOp s as a
    -> BPOp s rs a
plugBP i = plugBP' i (imap1 (\j _ -> known \\ every @_ @Num j) i)
                     (imap1 (\j _ -> known \\ every @_ @Num j) i)
                     known

infixr 1 ~$
(~$)
    :: (Every Num as, Num a)
    => BPOp s as a
    -> Prod (BRef s rs) as
    -> BPOp s rs a
o ~$ xs = plugBP xs o

infixr 1 $~
($~)
    :: (Every Num as, Num a)
    => Prod (BRef s rs) as
    -> (Prod (BRef s as) as -> BPOp s as a)
    -> BPOp s rs a
x $~ f = plugBP x (withInps' (prodLength x) f)


inpRef
    :: Index rs a
    -> BRef s rs a
inpRef = BRInp

inpRefs
    :: Known Length rs
    => Prod (BRef s rs) rs
inpRefs = inpRefs' known

inpRefs'
    :: Length rs
    -> Prod (BRef s rs) rs
inpRefs' = map1 inpRef . indices'

withInps'
    :: Length rs
    -> (Prod (BRef s rs) rs -> BP s rs a)
    -> BP s rs a
withInps' l f = f (inpRefs' l)

withInps
    :: Known Length rs
    => (Prod (BRef s rs) rs -> BP s rs a)
    -> BP s rs a
withInps = withInps' known

liftR
    :: Op as a
    -> Prod (BRef s rs) as
    -> BRef s rs a
liftR o xs = BROp xs o

liftR1
    :: Op '[a] b
    -> BRef s rs a
    -> BRef s rs b
liftR1 o = liftR o . only

liftR2
    :: Op '[a,b] c
    -> BRef s rs a
    -> BRef s rs b
    -> BRef s rs c
liftR2 o x y = liftR o (x :< y :< Ø)

liftR3
    :: Op '[a,b,c] d
    -> BRef s rs a
    -> BRef s rs b
    -> BRef s rs c
    -> BRef s rs d
liftR3 o x y z = liftR o (x :< y :< z :< Ø)











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

