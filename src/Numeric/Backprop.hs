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
  -- ** Backprop Types
    BP, BPOp, BPOpI, BVar, Op, OpB
  -- ** Utility types
  , Prod(..), Tuple
  , Summer(..), Unity(..)
  -- * BP
  -- ** Backprop
  , backprop, evalBPOp, gradBPOp
  , backprop', evalBPOp', gradBPOp'
  -- ** Utility combinators
  , withInps, implicitly
  , withInps', implicitly'
  -- * Vars
  , constVar
  , inpVar, inpVars
  , bpOp
  , bindVar
  , inpVars'
  , bpOp'
  , bindVar'
  -- ** From Ops
  , opVar, (~$)
  , opVar1, opVar2, opVar3
  , (-$), (-&)
  , opVar'
  , opVar1', opVar2', opVar3'
  -- ** Var manipulation
  -- *** As parts
  , partsVar, (#<~), withParts
  , splitVars, gSplit
  , partsVar', withParts'
  , splitVars', gSplit'
  -- *** As sums
  , choicesVar
  , choicesVar'
  -- *** As sums of products
  , sopVar, gSplits
  , sopVar', gSplits'
  -- ** Transforming BP
  , internally
  , generically
  , internally'
  , generically'
  -- ** Combining
  , liftR, liftR1, liftR2, liftR3
  -- * Op
  , op1, op2, op3, opN
  , op1', op2', op3'
  -- * Utility
  , pattern (:>), only, head'
  , pattern (::<), only_
  , summers, unities
  , summers', unities'
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
import           Lens.Micro.Mtl hiding     (view)
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Generics.SOP              as SOP

-- | A handy type synonym representing a 'BP' action that returns a 'BVar'.
-- This is handy because this is the form of 'BP' actions that
-- 'backprop' and 'gradBPOp' (etc.) expects.
--
-- A value of type:
--
-- @
-- 'BPOp' s rs a
-- @
--
-- is an action that takes an input environment of @rs@ and produces
-- a 'BVar' containing a value of type @a@.  Because it returns a 'BVar',
-- the library can track the data dependencies between the 'BVar' and the
-- input environment and perform backpropagation.
--
-- See documentation for 'BP' for an explanation of the phantom type
-- parameter @s@.
type BPOp s rs a  = BP s rs (BVar s rs a)

-- | An "implicit" operation on 'BVar's that can be backpropagated.
-- A value of type:
--
-- @
-- 'BPOpI' s rs a
-- @
--
-- takes a bunch of 'BVar's containg @rs@ and uses them to (purely) produce
-- a 'BVar' containing an @a@.
--
-- If you are exclusively doing implicit backpropagation by combining
-- 'BVar's and using 'BPOpI's, you are probably better off just importing
-- "Numeric.Backprop.Implicit", which provides better tools.  This type
-- synonym exists in "Numeric.Backprop" just for the 'implicitly' function,
-- which can convert "implicit" backprop functions like a @'BPOpI' s rs a@
-- into an "explicit" graph backprop function, a @'BPOp' s rs a@.
type BPOpI s rs a = Prod (BVar s rs) rs -> BVar s rs a

opVar'
    :: forall s rs as a. ()
    => Summer a
    -> OpB s as a
    -> Prod (BVar s rs) as
    -> BP s rs (BVar s rs a)
opVar' s o i = do
    xs <- traverse1 (fmap I . BP . resolveVar) i
    (res, gf) <- BP . liftBase $ runOpM' o xs
    let bp = BPN { _bpnOut       = only $ FRInternal []
                 , _bpnRes       = only_ res
                 , _bpnGradFunc  = gf . head'
                 , _bpnGradCache = Nothing
                 , _bpnSummer    = only s
                 }
    r <- BP . liftBase $ newSTRef bp
    itraverse1_ (registerVar . flip IRNode r) i
    return (BVNode IZ r)

splitVars'
    :: forall s rs as. ()
    => Prod Summer as
    -> Prod Unity as
    -> BVar s rs (Tuple as)
    -> BP s rs (Prod (BVar s rs) as)
splitVars' ss us = partsVar' ss us id

splitVars
    :: forall s rs as. (Every Num as, Known Length as)
    => BVar s rs (Tuple as)
    -> BP s rs (Prod (BVar s rs) as)
splitVars = partsVar id

partsVar'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
partsVar' ss us i =
    fmap (view sum1) . sopVar' (only ss) (only us) (i . resum1)

partsVar
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
partsVar = partsVar' summers unities

infixr 1 #<~
(#<~)
    :: (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
(#<~) = partsVar

withParts'
    :: Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BVar s rs b
    -> (Prod (BVar s rs) bs -> BP s rs a)
    -> BP s rs a
withParts' ss us i r f = do
    p <- partsVar' ss us i r
    f p

withParts
    :: (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> (Prod (BVar s rs) bs -> BP s rs a)
    -> BP s rs a
withParts i r f = do
    p <- partsVar i r
    f p

gSplit'
    :: (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
gSplit' ss us = partsVar' ss us gTuple

gSplit
    :: (Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
gSplit = gSplit' summers unities

internally'
    :: forall s rs bs b a. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s bs (BVar s bs a)
    -> BP s rs (BVar s rs a)
internally' ss us sa l r bp = do
    xs <- view l <$> BP (resolveVar r)
    (res, gFunc) <- BP . liftBase $ backpropWith ss us bp xs
    let bpn :: BPNode s rs '[ b ] '[ a ]
        bpn = BPN { _bpnOut       = only $ FRInternal []
                  , _bpnRes       = only_ res
                  , _bpnGradFunc  = fmap (only_ . review l) . gFunc . head'
                  , _bpnGradCache = Nothing
                  , _bpnSummer    = only sa
                  }
    r' <- BP . liftBase $ newSTRef bpn
    registerVar (IRNode IZ r') r
    return (BVNode IZ r')

internally
    :: forall s rs bs b a. (Every Num bs, Known Length bs, Num a)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s bs (BVar s bs a)
    -> BP s rs (BVar s rs a)
internally = internally' summers unities known

generically'
    :: forall s rs bs b a. (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> Summer a
    -> BVar s rs b
    -> BP s bs (BVar s bs a)
    -> BP s rs (BVar s rs a)
generically' ss us sa = internally' ss us sa gTuple

generically
    :: forall s rs bs b a. (Num a, Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BVar s rs b
    -> BP s bs (BVar s bs a)
    -> BP s rs (BVar s rs a)
generically = internally gTuple

choicesVar'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Sum I bs)
    -> BVar s rs b
    -> BP s rs (Sum (BVar s rs) bs)
choicesVar' ss us i r = do
    x <- BP $ resolveVar r
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
      registerVar (IRNode IZ r') r
      return $ BVNode IZ r'
-- TODO: cannot implement via sopVar?  oh well.

choicesVar
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Sum I bs)
    -> BVar s rs b
    -> BP s rs (Sum (BVar s rs) bs)
choicesVar = choicesVar' summers unities

sopVar'
    :: forall s rs bss b. ()
    => Prod (Prod Summer) bss
    -> Prod (Prod Unity) bss
    -> Iso' b (Sum Tuple bss)
    -> BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) bss)
sopVar' sss uss i r = do
    x <- BP $ resolveVar r
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
      registerVar (IRNode IZ r') r
      return $ imap1 (\ix' _ -> BVNode ix' r') ys

sopVar
    :: forall s rs bss b. (Known Length bss, Every (Every Num ∧ Known Length) bss)
    => Iso' b (Sum Tuple bss)
    -> BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) bss)
sopVar = sopVar' (withEvery @(Every Num ∧ Known Length) summers)
                 (withEvery @(Every Num ∧ Known Length) unities)

gSplits'
    :: forall s rs b. SOP.Generic b
    => Prod (Prod Summer) (SOP.Code b)
    -> Prod (Prod Unity) (SOP.Code b)
    -> BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) (SOP.Code b))
gSplits' sss uss = sopVar' sss uss gSOP

gSplits
    :: forall s rs b.
      ( SOP.Generic b
      , Known Length (SOP.Code b)
      , Every (Every Num ∧ Known Length) (SOP.Code b)
      )
    => BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) (SOP.Code b))
gSplits = sopVar gSOP


-- TODO: pull summers too
resolveVar
    :: (MonadReader (Tuple rs) m, MonadBase (ST s) m)
    => BVar s rs a
    -> m a
resolveVar = \case
    BVNode  ix r -> getI . index ix . _bpnRes <$> liftBase (readSTRef r)
    BVInp   ix   -> getI . index ix <$> ask
    BVConst    x -> return x
    BVOp    rs o -> do
      xs <- traverse1 (fmap I . resolveVar) rs
      liftBase $ runOpM o xs

registerVar
    :: forall s rs a. ()
    => BPInpRef s rs a
    -> BVar s rs a
    -> BP s rs ()
registerVar bpir = \case
    BVNode  ix' r' -> BP . liftBase . modifySTRef r' $
                        over (bpnOut . indexP ix' . _FRInternal) (bpir :)
    BVInp   ix'    -> BP $ modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
    BVConst _      -> return ()
    -- This independently makes a new BPPipe for every usage site of the
    -- BVOp, so it's a bit inefficient.
    BVOp    (rs :: Prod (BVar s rs) ds) (o :: OpM (ST s) ds a) -> do
      xs :: Tuple ds <- traverse1 (fmap I . BP . resolveVar) rs
      (res, gF) <- BP . liftBase $ runOpM' o xs
      let bpp :: BPPipe s rs ds '[a]
          bpp = BPP { _bppOut       = only bpir
                    , _bppRes       = only_ res
                    , _bppGradFunc  = gF . Just . getI . head'
                    , _bppGradCache = Nothing
                    }
      r' <- BP . liftBase $ newSTRef bpp
      ifor1_ rs $ \ix' (bpr :: BVar s rs d) ->
        registerVar (IRPipe ix' r') bpr

-- | Apply several 
opVar
    :: Num a
    => OpB s as a
    -> Prod (BVar s rs) as
    -> BP s rs (BVar s rs a)
opVar = opVar' known

infixr 1 ~$
(~$)
    :: Num a
    => OpB s as a
    -> Prod (BVar s rs) as
    -> BP s rs (BVar s rs a)
(~$) = opVar

infixr 1 -$
(-$)
    :: (Every Num as, Known Length as, Num a)
    => BPOp s as a
    -> Prod (BVar s rs) as
    -> BPOp s rs a
o -$ xs = bpOp o ~$ xs

infixr 1 -&
(-&)
    :: (Every Num as, Known Length as, Num a)
    => Prod (BVar s rs) as
    -> (Prod (BVar s as) as -> BPOp s as a)
    -> BPOp s rs a
xs -& f = bpOp (withInps f) ~$ xs

-- | Create a 'BVar' that represents just a specific value, that doesn't
-- depend on any other 'BVar's.
constVar :: a -> BVar s rs a
constVar = BVConst

opVar1'
    :: Summer b
    -> OpB s '[a] b
    -> BVar s rs a
    -> BP s rs (BVar s rs b)
opVar1' s o = opVar' s o . only

opVar1
    :: Num b
    => OpB s '[a] b
    -> BVar s rs a
    -> BP s rs (BVar s rs b)
opVar1 = opVar1' known

opVar2'
    :: Summer c
    -> OpB s '[a,b] c
    -> BVar s rs a
    -> BVar s rs b
    -> BP s rs (BVar s rs c)
opVar2' s o rx ry = opVar' s o (rx :< ry :< Ø)

opVar2
    :: Num c
    => OpB s '[a,b] c
    -> BVar s rs a
    -> BVar s rs b
    -> BP s rs (BVar s rs c)
opVar2 = opVar2' known

opVar3'
    :: Summer d
    -> OpB s '[a,b,c] d
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
    -> BP s rs (BVar s rs d)
opVar3' s o rx ry rz = opVar' s o (rx :< ry :< rz :< Ø)

opVar3
    :: Num d
    => OpB s '[a,b,c] d
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
    -> BP s rs (BVar s rs d)
opVar3 = opVar3' known

-- can be recursive too?  would have to have resolveVar also pull summers
bindVar'
    :: Summer a
    -> BVar s rs a
    -> BP s rs (BVar s rs a)
bindVar' s r = case r of
    BVNode  _  _ -> return r
    BVInp   _    -> return r
    BVConst _    -> return r
    BVOp    rs o -> opVar' s o rs

bindVar
    :: Num a
    => BVar s rs a
    -> BP s rs (BVar s rs a)
bindVar = bindVar' known



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

-- | A version of 'backprop' taking explicit 'Summer's and 'Unity's, so it
-- can be run with types that aren't instances of 'Num'.
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

-- | Perform backpropagation on the given 'BPOp'.  Returns the result of
-- the operation it represents, as well as the gradient of the result with
-- respect to its inputs.
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

-- | Turn a 'BPOp' into an 'OpB'.  Basically converts a 'BP' taking an @rs@
-- and producing an @a@ into an 'Op' taking an @rs@ and returning an @a@,
-- with all of the powers and utility of an 'Op', including all of its
-- gradient-finding glory.
--
-- Handy because an 'OpB' can be used with almost all of
-- the 'Op'-related functions in this moduel, including 'opVar', '~$', etc.
bpOp
    :: (Every Num as, Known Length as)
    => BPOp s as a
    -> OpB s as a
bpOp = bpOp' summers unities

-- | A version of 'evalBPOp' taking explicit 'Summer's and 'Unity's, so it
-- can be run with types that aren't instances of 'Num'.
evalBPOp'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)  -- ^ 'BPOp' to run
    -> Tuple rs                 -- ^ input
    -> a                        -- ^ output
evalBPOp' ss us bp env = runST $
    fst <$> backpropWith ss us bp env

-- | Simply run the 'BPOp' on an input tuple, getting the result without
-- bothering with the gradient or with backpropagation.
evalBPOp
    :: forall rs a. Every Num rs
    => (forall s. BPOp s rs a)  -- ^ 'BPOp' to run
    -> Tuple rs                 -- ^ input
    -> a                        -- ^ output
evalBPOp o env = evalBPOp' (summers' l) (unities' l) o env
  where
    l :: Length rs
    l = prodLength env

-- | A version of 'gradBPOp' taking explicit 'Summer's and 'Unity's, so it
-- can be run with types that aren't instances of 'Num'.
gradBPOp'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)  -- ^ 'BPOp' to differentiate'
    -> Tuple rs                 -- ^ input
    -> Tuple rs                 -- ^ gradient
gradBPOp' ss us bp = snd . backprop' ss us bp

-- | Run the 'BPOp' on an input tuple and return the gradient of the result
-- with respect to the input tuple.
gradBPOp
    :: Every Num rs
    => (forall s. BPOp s rs a)  -- ^ 'BPOp' to differentiate
    -> Tuple rs                 -- ^ input
    -> Tuple rs                 -- ^ gradient
gradBPOp bp = snd . backprop bp


closeOff
    :: (MonadReader (Tuple rs) m, MonadState (BPState s rs) m, MonadBase (ST s) m)
    => Bool
    -> Maybe a
    -> BVar s rs a
    -> m ()
closeOff isTerminal gOut = \case
    BVNode  ix sr -> liftBase $ modifySTRef sr (over (bpnOut . indexP ix) (<> fr))
    BVInp   ix'   -> modifying (bpsSources . indexP ix') (<> fr)
    BVConst _     -> return ()
    BVOp    rs o  -> do
      xs <- traverse1 (fmap I . resolveVar) rs
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
    res <- runReaderT (resolveVar r) env
    let gradFunc gradOut = do
          BPS{..} <- execStateT (runReaderT (closeOff True gradOut r) env) bps0
          for1 (ss `zipP` us `zipP` _bpsSources) $ \((s :&: u) :&: rs) -> do
            I <$> case rs of
              FRInternal rs' -> runSummer s <$> traverse backwardPass rs'
              FRTerminal g   -> return $ fromMaybe (getUnity u) g
    return (res, gradFunc)

-- | A version of 'implicitly' taking explicit 'Length', indicating the
-- number of inputs required and their types.
--
-- Mostly useful for rare "extremely polymorphic" situations.  If you ever
-- actually explicitly write down @rs@ as a list of types, you should be
-- able to just use 'implicitly'.
implicitly'
    :: Length rs
    -> BPOpI s rs a
    -> BPOp s rs a
implicitly' l f = withInps' l (return . f)

-- | Convert a 'BPOpI' into a 'BPOp'.  That is, convert a function on
-- a bundle of 'BVar's (generating an implicit graph) into a fully fledged
-- 'BPOp' that you can run 'backprop' on.  See 'BPOpI' for more
-- information.
--
-- If you are going to write exclusively using implicit 'BVar' operations,
-- it might be more convenient to use "Numeric.Backprop.Implicit" instead,
-- which is geared around that use case.
implicitly
    :: Known Length rs
    => BPOpI s rs a
    -> BPOp s rs a
implicitly = implicitly' known

-- | Create a 'BVar' given an index into the input environment.  For an
-- example,
--
-- @
-- 'inpVar' 'IZ'
-- @
--
-- would refer to the /first/ input variable (the 'Int' in a
-- @'BP' s '[Int, Bool]@), and
--
-- @
-- 'inpVar' ('IS' 'IZ')
-- @
--
-- Would refer to the /second/ input variable (the 'Bool' in a
-- @'BP' s '[Int, Bool]@)
--
-- Typically, there shouldn't be any reason to use 'inpVar' directly.  It's
-- cleaner to get all of your input 'BVar's together using 'withInps' or
-- 'inpVars'.
inpVar
    :: Index rs a
    -> BVar s rs a
inpVar = BVInp

inpVars
    :: Known Length rs
    => Prod (BVar s rs) rs
inpVars = inpVars' known

inpVars'
    :: Length rs
    -> Prod (BVar s rs) rs
inpVars' = map1 inpVar . indices'

-- | A version of 'withInps' taking explicit 'Length', indicating the
-- number of inputs required and their types.
--
-- Mostly useful for rare "extremely polymorphic" situations.  If you ever
-- actually explicitly write down @rs@ as a list of types, you should be
-- able to just use 'withInps'.
withInps'
    :: Length rs
    -> (Prod (BVar s rs) rs -> BP s rs a)
    -> BP s rs a
withInps' l f = f (inpVars' l)

-- | Runs a continuation on a 'Prod' of all of the input 'BVar's.
--
-- Handy for bringing the environment into scope and doing stuff with it:
--
-- @
-- foo :: 'BPOp' '[Double, Int] a
-- foo = 'withInps' $ \(x :< y :< Ø) -> do
--     -- do stuff with inputs
-- @
--
-- Looks kinda like @foo (x :< y :< Ø) = -- ...@, don't it?
--
-- Note that the above is the same as
--
-- @
-- foo :: 'BPOp' '[Double, Int] a
-- foo = do
--     x :< y :< Ø <- 'inpVars'
--     -- do stuff with inputs
-- @
--
-- But just a little nicer!
withInps
    :: Known Length rs
    => (Prod (BVar s rs) rs -> BP s rs a)
    -> BP s rs a
withInps = withInps' known

liftR
    :: OpB s as a
    -> Prod (BVar s rs) as
    -> BVar s rs a
liftR = flip BVOp

liftR1
    :: OpB s '[a] b
    -> BVar s rs a
    -> BVar s rs b
liftR1 o = liftR o . only

liftR2
    :: OpB s '[a,b] c
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
liftR2 o x y = liftR o (x :< y :< Ø)

liftR3
    :: OpB s '[a,b,c] d
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
    -> BVar s rs d
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

