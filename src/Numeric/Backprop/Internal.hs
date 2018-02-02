{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Numeric.Backprop.Internal
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the types and instances used for the graph
-- building/back-propagation for the library.

module Numeric.Backprop.Internal (
    BVar
  , W
  , constVar
  , liftOpN
  , liftOp1, liftOp2, liftOp3, liftOp4
  , lensVar
  , backprop
  ) where

import           Control.Applicative.Backwards
import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Foldable
import           Data.IORef
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Index
import           Data.Type.Product
import           Data.Type.Util
import           Lens.Micro hiding             (ix)
import           Numeric.Backprop.Op
import           System.IO.Unsafe
import           Type.Class.Higher
import           Type.Class.Witness
import           Unsafe.Coerce
import qualified Data.Vector.Mutable           as MV

data BVar (s :: Type) (a :: Type)
        = BVInp
        | BVIx !Int
        | BVC !a

isBVC :: BVar s a -> Maybe a
isBVC (BVC x) = Just x
isBVC _       = Nothing

forceBVar :: BVar s a -> ()
forceBVar BVInp     = ()
forceBVar (BVIx !_) = ()
forceBVar (BVC  !_) = ()

data InpRef :: Type -> Type where
    IR :: Num a
       => { _irIx  :: !(BVar s b)
          , _irUpd :: !(Lens' b a)
          }
       -> InpRef a

forceInpRef :: InpRef a -> ()
forceInpRef (IR !v !_) = forceBVar v `seq` ()

data TapeNode :: Type -> Type where
    TN :: { _tnInputs :: !(Prod InpRef as)
          , _tnOp     :: !(Op as a)
          }
       -> TapeNode a

forceTapeNode :: TapeNode a -> ()
forceTapeNode (TN !inps !_) = foldMap1 forceInpRef inps `seq` ()

data SomeTapeNode :: Type where
    STN :: forall a. Num a
        => !(TapeNode a)
        -> SomeTapeNode

forceSomeTapeNode :: SomeTapeNode -> ()
forceSomeTapeNode (STN !tn) = forceTapeNode tn `seq` ()

newtype W = W { wRef :: IORef (Int, [SomeTapeNode]) }

initWengert :: IO W
initWengert = W <$> newIORef (0,[])

insertNode
    :: Num a
    => TapeNode a
    -> W
    -> IO (BVar s a)
insertNode tn w = fmap BVIx . atomicModifyIORef' (wRef w) $ \(n,t) ->
    let n' = n + 1
        t' = STN tn:t
    in  n' `seq` t' `seq` ((n', t'), n)

constVar :: a -> BVar s a
constVar = BVC

liftOpN_
    :: forall s as b. (Reifies s W, Num b, Every Num as)
    => Op as b
    -> Prod (BVar s) as
    -> IO (BVar s b)
liftOpN_ o !vs = case traverse1 (fmap I . isBVC) vs of
                   Just xs -> return $ BVC (evalOp o xs)
                   Nothing -> insertNode tn (reflect (Proxy @s))
  where
    tn = TN { _tnInputs = imap1 go vs
            , _tnOp     = o
            }
    go :: forall a. Index as a -> BVar s a -> InpRef a
    go i !v = forceBVar v `seq` (IR v id \\ every @_ @Num i)

liftOpN
    :: forall s as b. (Reifies s W, Num b, Every Num as)
    => Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOpN o !vs = unsafePerformIO $ liftOpN_ o vs

liftOp1_
    :: forall s a b. (Reifies s W, Num a, Num b)
    => Op '[a] b
    -> BVar s a
    -> IO (BVar s b)
liftOp1_ o (BVC x) = return . BVC . evalOp o $ (x ::< Ø)
liftOp1_ o !v = forceBVar v `seq` insertNode tn (reflect (Proxy @s))
  where
    tn = TN { _tnInputs = IR v id :< Ø
            , _tnOp     = o
            }

liftOp1
    :: forall s a b. (Reifies s W, Num a, Num b)
    => Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 o !v = unsafePerformIO $ liftOp1_ o v

liftOp2_
    :: forall s a b c. (Reifies s W, Num a, Num b, Num c)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> IO (BVar s c)
liftOp2_ o (BVC x) (BVC y) = return . BVC . evalOp o $ x ::< y ::< Ø
liftOp2_ o !v !u = forceBVar v
             `seq` forceBVar u
             `seq` insertNode tn (reflect (Proxy @s))
  where
    tn = TN { _tnInputs = IR v id :< IR u id :< Ø
            , _tnOp     = o
            }

liftOp2
    :: forall s a b c. (Reifies s W, Num a, Num b, Num c)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 o !v !u = unsafePerformIO $ liftOp2_ o v u

liftOp3_
    :: forall s a b c d. (Reifies s W, Num a, Num b, Num c, Num d)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> IO (BVar s d)
liftOp3_ o (BVC x) (BVC y) (BVC z) = return . BVC . evalOp o
                                   $ x ::< y ::< z ::< Ø
liftOp3_ o !v !u !w = forceBVar v
                `seq` forceBVar u
                `seq` forceBVar w
                `seq` insertNode tn (reflect (Proxy @s))
  where
    tn = TN { _tnInputs = IR v id :< IR u id :< IR w id :< Ø
            , _tnOp     = o
            }

liftOp3
    :: forall s a b c d. (Reifies s W, Num a, Num b, Num c, Num d)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 o !v !u !w = unsafePerformIO $ liftOp3_ o v u w

liftOp4_
    :: forall s a b c d e. (Reifies s W, Num a, Num b, Num c, Num d, Num e)
    => Op '[a,b,c,d] e
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
    -> IO (BVar s e)
liftOp4_ o (BVC x) (BVC y) (BVC z) (BVC w) = return . BVC . evalOp o
                                           $ x ::< y ::< z ::< w ::< Ø
liftOp4_ o !v !u !w !x = forceBVar v
                   `seq` forceBVar u
                   `seq` forceBVar w
                   `seq` forceBVar x
                   `seq` insertNode tn (reflect (Proxy @s))
  where
    tn = TN { _tnInputs = IR v id :< IR u id :< IR w id :< IR x id :< Ø
            , _tnOp     = o
            }

liftOp4
    :: forall s a b c d e. (Reifies s W, Num a, Num b, Num c, Num d, Num e)
    => Op '[a,b,c,d] e
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
    -> BVar s e
liftOp4 o !v !u !w !x = unsafePerformIO $ liftOp4_ o v u w x

lensVar_
    :: forall a b s. (Reifies s W, Num b, Num a)
    => Lens' b a
    -> BVar s b
    -> IO (BVar s a)
lensVar_ l !v = forceBVar v `seq` insertNode tn (reflect (Proxy @s))
  where
    tn = TN { _tnInputs = IR v l :< Ø
            , _tnOp     = idOp
            }

lensVar
    :: forall a b s. (Reifies s W, Num b, Num a)
    => Lens' b a
    -> BVar s b
    -> BVar s a
lensVar l !v = unsafePerformIO $ lensVar_ l v

data Grad :: Type where
    G :: Prod InpRef as
      -> Proxy a
      -> (a -> Tuple as)
      -> Grad

data SomeNum :: Type where
    SN  :: Num a
        => Proxy a
        -> a
        -> SomeNum

data Runner s = R { _rRes   :: MV.MVector s SomeNum
                  , _rDelta :: MV.MVector s SomeNum
                  , _rGrads :: MV.MVector s Grad
                  }

initRunner
    :: (PrimMonad m, PrimState m ~ s)
    => Int
    -> m (Runner s)
initRunner n = R <$> MV.new n <*> MV.new n <*> MV.new n

evalRunner
    :: forall m a b s. (PrimMonad m, PrimState m ~ s)
    => Runner s
    -> (Int, [SomeTapeNode])
    -> a
    -> m b
evalRunner R{..} (n, stns) x = do
    forwards . for_ (zip [n-1,n-2..] stns) $ \(i, STN (TN{..} :: TapeNode c)) -> Backwards $ do
      inps <- traverse1 (fmap I . findInps) _tnInputs
      let (res, gr) = runOpWith _tnOp inps
      MV.write _rRes   i $ SN (Proxy @c) res
      MV.write _rDelta i $ SN (Proxy @c) 0
      MV.write _rGrads i $ G _tnInputs (Proxy @c) gr
    SN _ y <- MV.read _rRes (MV.length _rRes - 1)
    return $ unsafeCoerce y
  where
    findInps :: forall x. InpRef x -> m x
    findInps (IR v ln) = do
      targ <- case v of
        BVInp  -> return $ unsafeCoerce x
        BVIx i -> do
          SN _ y <- MV.read _rRes i
          return $ unsafeCoerce y
        BVC y -> return y
      return $ targ ^. ln

gradRunner
    :: forall m a b s p. (PrimMonad m, PrimState m ~ s, Num b)
    => p b
    -> Runner s
    -> (Int, [SomeTapeNode])
    -> MutVar s a
    -> m ()
gradRunner _ R{..} (n,stns) dx = do
    MV.write _rDelta (n - 1) (SN (Proxy @b) 1)
    zipWithM_ go [n-1,n-2..] stns
  where
    go :: Int -> SomeTapeNode -> m ()
    go i (STN TN{..}) = do
      SN _ delt  <- MV.read _rDelta i
      G  irs _ f <- MV.read _rGrads i
      let gs = f (unsafeCoerce delt)
      zipWithPM_ propagate irs gs
    propagate :: forall x. InpRef x -> I x -> m ()
    propagate (IR v ln) (I !d) = case v of
      BVInp     -> modifyMutVar' (unsafeCoerce dx) (ln %~ (+ d))
      BVIx !i   -> flip (MV.modify _rDelta) i $ \case
        SN p !y -> let y' = unsafeCoerce y & ln %~ (+d)
                   in  y' `seq` SN p (unsafeCoerce y')
      BVC _ -> return ()

registerOut
    :: (Reifies s W, Num a)
    => BVar s a
    -> IO ()
registerOut !v = void . liftOp1_ idOp $ v

backprop
    :: forall a b. (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop f = \x -> runST (go x)
  where
    !tp@(!_,!_) = unsafePerformIO $ do
      w <- initWengert
      reify w $ \(Proxy :: Proxy s) ->
        registerOut =<< evaluate (f (BVInp @s))
      t <- readIORef (wRef w)
      traverse_ (evaluate . forceSomeTapeNode) (snd t)
      return t
    go :: a -> ST s (b, a)
    go x = do
      r <- initRunner (fst tp)
      y :: b <- evalRunner r tp x
      o <- newMutVar (0 :: a)
      gradRunner (Proxy @b) r tp o
      (y,) <$> readMutVar o

instance (Num a, Reifies s W) => Num (BVar s a) where
    (+)         = liftOp2 (+.)
    {-# INLINE (+) #-}
    (-)         = liftOp2 (-.)
    {-# INLINE (-) #-}
    (*)         = liftOp2 (*.)
    {-# INLINE (*) #-}
    negate      = liftOp1 negateOp
    {-# INLINE negate #-}
    signum      = liftOp1 signumOp
    {-# INLINE signum #-}
    abs         = liftOp1 absOp
    {-# INLINE abs #-}
    fromInteger = BVC . fromInteger
    {-# INLINE fromInteger #-}

instance (Fractional a, Reifies s W) => Fractional (BVar s a) where
    (/)          = liftOp2 (/.)
    {-# INLINE (/) #-}
    recip        = liftOp1 recipOp
    {-# INLINE recip #-}
    fromRational = BVC . fromRational
    {-# INLINE fromRational #-}

instance (Floating a, Reifies s W) => Floating (BVar s a) where
    pi      = BVC pi
    {-# INLINE pi #-}
    exp     = liftOp1 expOp
    {-# INLINE exp #-}
    log     = liftOp1 logOp
    {-# INLINE log #-}
    sqrt    = liftOp1 sqrtOp
    {-# INLINE sqrt #-}
    (**)    = liftOp2 (**.)
    {-# INLINE (**) #-}
    logBase = liftOp2 logBaseOp
    {-# INLINE logBase #-}
    sin     = liftOp1 sinOp
    {-# INLINE sin #-}
    cos     = liftOp1 cosOp
    {-# INLINE cos #-}
    tan     =  liftOp1 tanOp
    {-# INLINE tan  #-}
    asin    = liftOp1 asinOp
    {-# INLINE asin #-}
    acos    = liftOp1 acosOp
    {-# INLINE acos #-}
    atan    = liftOp1 atanOp
    {-# INLINE atan #-}
    sinh    = liftOp1 sinhOp
    {-# INLINE sinh #-}
    cosh    = liftOp1 coshOp
    {-# INLINE cosh #-}
    tanh    = liftOp1 tanhOp
    {-# INLINE tanh #-}
    asinh   = liftOp1 asinhOp
    {-# INLINE asinh #-}
    acosh   = liftOp1 acoshOp
    {-# INLINE acosh #-}
    atanh   = liftOp1 atanhOp
    {-# INLINE atanh #-}

---- | A subclass of 'OpM' (and superclass of 'Op'), representing 'Op's that
---- the /backprop/ library uses to perform backpropation.
----
---- An
----
---- @
---- 'OpB' s rs a
---- @
----
---- represents a differentiable function that takes a tuple of @rs@ and
---- produces an a @a@, which can be run on @'BVar' s@s and also inside @'BP'
---- s@s.  For example, an @'OpB' s '[ Int, Double ] Bool@ takes an 'Int' and
---- a 'Double' and produces a 'Bool', and does it in a differentiable way.
----
---- 'OpB' is a /superset/ of 'Op', so, if you see any function
---- that expects an 'OpB' (like 'Numeric.Backprop.opVar'' and
---- 'Numeric.Backprop.~$', for example), you can give them an 'Op', as well.
----
---- You can think of 'OpB' as a superclass/parent class of 'Op' in this
---- sense, and of 'Op' as a subclass of 'OpB'.
--type OpB s as a = OpM (ST s) as a

---- | Convenience wrapper over a @forall s. 'OpB' s as a@, to work around
---- lack of impredicative types in GHC
--newtype OpBS as a = OpBS { runOpBS :: forall s. OpB s as a }

---- | A version of 'runOp' for 'OpB': runs the function that an 'OpB'
---- encodes, returning the result.
----
---- >>> runOpB (op2 (*)) (3 ::< 5 ::< Ø)
---- 15
--runOpB :: (forall s. OpB s as a) -> Tuple as -> a
--runOpB o xs = runST $ runOpM o xs

---- | A version of 'gradOp' for 'OpB': runs the function that an 'OpB'
---- encodes, getting the gradient of the output with respect to the inputs.
----
---- >>> gradOpB (op2 (*)) (3 ::< 5 ::< Ø)
---- 5 ::< 3 ::< Ø
---- -- the gradient of x*y is (y, x)
--gradOpB :: (forall s. OpB s as a) -> Tuple as -> Tuple as
--gradOpB o xs = runST $ gradOpM o xs

---- | A version of 'gradOp'' for 'OpB': runs the function that an 'OpB'
---- encodes, getting the result along with the gradient of the output with
---- respect to the inputs.
----
---- >>> gradOpB' (op2 (*)) (3 ::< 5 ::< Ø)
---- (15, 5 ::< 3 ::< Ø)
--gradOpB' :: (forall s. OpB s as a) -> Tuple as -> (a, Tuple as)
--gradOpB' o xs = runST $ gradOpM' o xs

---- | Reference to /usage sites/ for a given entity, used to get partial or
---- total derivatives.
--data ForwardRefs s rs a
--    -- | A list of 'BPInpRef's pointing to places that use the entity, to
--    -- provide partial derivatives.
--    = FRInternal ![BPInpRef s rs a]
--    -- | The entity is the terminal result of a BP, so its total derivative
--    -- is fixed.
--    | FRTerminal !(Maybe a)

---- | Combines two 'FRInternal' lists.  If either input is an 'FRTerminal',
---- then throws away the other result and keeps the new terminal forced
---- total derivative.  (Biases to the left)
--instance Monoid (ForwardRefs s rs a) where
--    mempty  = FRInternal []
--    mappend = \case
--        FRInternal rs -> \case
--          FRInternal rs'   -> FRInternal (rs ++ rs')
--          t@(FRTerminal _) -> t
--        FRTerminal _  -> id

---- | The "state" of a 'BP' action, which keeps track of what nodes, if any,
---- refer to any of the inputs.
--data BPState :: Type -> [Type] -> Type where
--    BPS :: { _bpsSources :: !(Prod (ForwardRefs s rs) rs)
--           }
--        -> BPState s rs

---- | A Monad allowing you to explicitly build hetereogeneous data
---- dependency graphs and that the library can perform back-propagation on.
----
---- A @'BP' s rs a@ is a 'BP' action that uses an environment of @rs@
---- returning a @a@.  When "run", it will compute a gradient that is a tuple
---- of @rs@.  (The phantom parameter @s@ is used to ensure that any 'BVar's
---- aren't leaked out of the monad)
----
---- Note that you can only "run" a @'BP' s rs@ that produces a 'BVar' --
---- that is, things of the form
----
---- @
---- 'BP' s rs ('BVar' s rs a)
---- @
----
---- The above is a 'BP' action that returns a 'BVar' containing an @a@.
---- When this is run, it'll produce a result of type @a@ and a gradient of
---- that is a tuple of @rs@.  (This form has a type synonym,
---- 'Numeric.Backprop.BPOp', for convenience)
----
---- For example, a @'BP' s '[ Int, Double, Double ]@ is a monad that
---- represents a computation with an 'Int', 'Double', and 'Double' as
---- inputs.   And, if you ran a
----
---- @
---- 'BP' s '[ Int, Double, Double ] ('BVar' s '[ Int, Double, Double ] Double)
---- @
----
---- Or, using the 'BPOp' type synonym:
----
---- @
---- 'Numeric.Backprop.BPOp' s '[ Int, Double, Double ] Double
---- @
----
---- with 'Numeric.Backprop.backprop' or 'Numeric.Backprop.gradBPOp', it'll
---- return a gradient on the inputs ('Int', 'Double', and 'Double') and
---- produce a value of type 'Double'.
----
---- Now, one powerful thing about this type is that a 'BP' is itself an
---- 'Op' (or more precisely, an 'Numeric.Backprop.OpB', which is a subtype of
---- 'OpM').  So, once you create your fancy 'BP' computation, you can
---- transform it into an 'OpM' using 'Numeric.Backprop.bpOp'.
--newtype BP s rs a
--    = BP { bpST :: ReaderT (Tuple rs) (StateT (BPState s rs) (ST s)) a }
--    deriving (Functor, Applicative, Monad)

---- | The basic unit of manipulation inside 'BP' (or inside an
---- implicit-graph backprop function).  Instead of directly working with
---- values, you work with 'BVar's contating those values.  When you work
---- with a 'BVar', the /backprop/ library can keep track of what values
---- refer to which other values, and so can perform back-propagation to
---- compute gradients.
----
---- A @'BVar' s rs a@ refers to a value of type @a@, with an environment
---- of values of the types @rs@.  The phantom parameter @s@ is used to
---- ensure that stray 'BVar's don't leak outside of the backprop process.
----
---- (That is, if you're using implicit backprop, it ensures that you interact
---- with 'BVar's in a polymorphic way.  And, if you're using explicit
---- backprop, it ensures that a @'BVar' s rs a@ never leaves the @'BP' s rs@
---- that it was created in.)
----
---- 'BVar's have 'Num', 'Fractional', 'Floating', etc. instances, so they
---- can be manipulated using polymorphic functions and numeric functions in
---- Haskell.  You can add them, subtract them, etc., in "implicit" backprop
---- style.
----
---- (However, note that if you directly manipulate 'BVar's using those
---- instances or using 'Numeric.Backprop.liftB', it delays evaluation, so every usage site
---- has to re-compute the result/create a new node.  If you want to re-use
---- a 'BVar' you created using '+' or '-' or 'Numeric.Backprop.liftB', use
---- 'Numeric.Backprop.bindVar' to force it first.  See documentation for
---- 'Numeric.Backprop.bindVar' for more details.)
--data BVar :: Type -> [Type] -> Type -> Type where
--    -- | A BVar referring to a 'BPNode'
--    BVNode  :: !(Index bs a)
--            -> !(STRef s (BPNode s rs as bs))
--            -> BVar s rs a
--    -- | A BVar referring to an environment input variable
--    BVInp   :: !(Index rs a)
--            -> BVar s rs a
--    -- | A constant BVar that refers to a specific Haskell value
--    BVC :: !a
--            -> BVar s rs a
--    -- | A BVar that combines several other BVars using a function (an
--    -- 'Op').  Essentially a branch of a tree.
--    BVOp    :: !(Prod (BVar s rs) as)
--            -> !(OpB s as a)
--            -> BVar s rs a

---- | Used exclusively by 'ForwardRefs' to specify "where" and "how" to look
---- for partial derivatives at usage sites of a given entity.
--data BPInpRef :: Type -> [Type] -> Type -> Type where
--    -- | The entity is used in a 'BPNode', and as an Nth input
--    IRNode  :: Every Num cs
--            => !(Index bs a)
--            -> !(STRef s (BPNode s rs bs cs))
--            -> BPInpRef s rs a
--    -- | The entity is used in a 'BPPipe', and as an Nth input
--    IRPipe  :: !(Index bs a)
--            -> !(STRef s (BPPipe s rs bs cs))
--            -> BPInpRef s rs a
--    -- | The entity is used somehow in the terminal result of a 'BP', and
--    -- so therefore has a fixed partial derivative contribution.
--    IRConst :: !a
--            -> BPInpRef s rs a

---- | A (stateful) node in the graph of operations/data dependencies in 'BP'
---- that the library uses.  'BVar's can refer to these to get results from
---- them, and 'BPInpRef's can refer to these to get partial derivatives from
---- them.
--data BPNode :: Type -> [Type] -> [Type] -> [Type] -> Type where
--    BPN :: { _bpnOut       :: !(Prod (ForwardRefs s rs) bs)
--           , _bpnRes       :: !(Tuple bs)
--           , _bpnGradFunc  :: !(Prod Maybe bs -> ST s (Tuple as))
--           , _bpnGradCache :: !(Maybe (Tuple as))  -- nothing if is the "final output"
--           }
--        -> BPNode s rs as bs

---- | Essentially a "single-usage" 'BPNode'.  It's a stateful node, but only
---- ever has a single consumer (and so its total derivative comes from
---- a single partial derivative).  Used when keeping track of 'BVOp's.
--data BPPipe :: Type -> [Type] -> [Type] -> [Type] -> Type where
--    BPP :: { _bppOut       :: !(Prod (BPInpRef s rs) bs)
--           , _bppRes       :: !(Tuple bs)
--           , _bppGradFunc  :: !(Tuple bs -> ST s (Tuple as))
--           , _bppGradCache :: !(Maybe (Tuple as))
--           }
--        -> BPPipe s rs as bs

--makeLenses ''BPState
--makeLenses ''BPNode
--makeLenses ''BPPipe

---- | Traversal (fake prism) to refer to the list of internal refs if the
---- 'ForwardRef' isn't associated with a terminal entity.
--_FRInternal
--    :: Traversal (ForwardRefs s as a) (ForwardRefs t bs a)
--                 [BPInpRef s as a]    [BPInpRef t bs a]
--_FRInternal f = \case
--    FRInternal xs -> FRInternal <$> f xs
--    FRTerminal g  -> pure (FRTerminal g)




---- | Note that if you use the 'Num' instance to create 'BVar's, the
---- resulting 'BVar' is deferred/delayed.  At every location you use it, it
---- will be recomputed, and a separate graph node will be created.  If you
---- are using a 'BVar' you made with the 'Num' instance in multiple
---- locations, use 'Numeric.Backprop.bindVar' first to force it and prevent
---- recomputation.
--instance Num a => Num (BVar s rs a) where
--    r1 + r2       = BVOp (r1 :< r2 :< Ø) (+.)
--    {-# INLINE (+) #-}
--    r1 - r2       = BVOp (r1 :< r2 :< Ø) (-.)
--    {-# INLINE (-) #-}
--    r1 * r2       = BVOp (r1 :< r2 :< Ø) (*.)
--    {-# INLINE (*) #-}
--    negate r      = BVOp (r  :< Ø)       negateOp
--    {-# INLINE negate #-}
--    signum r      = BVOp (r  :< Ø)       signumOp
--    {-# INLINE signum #-}
--    abs r         = BVOp (r  :< Ø)       absOp
--    {-# INLINE abs #-}
--    fromInteger x = BVC (fromInteger x)
--    {-# INLINE fromInteger #-}

---- | See note for 'Num' instance.
--instance Fractional a => Fractional (BVar s rs a) where
--    r1 / r2        = BVOp (r1 :< r2 :< Ø) (/.)
--    {-# INLINE (/) #-}
--    recip r        = BVOp (r  :< Ø)       recipOp
--    {-# INLINE recip #-}
--    fromRational x = BVC (fromRational x)
--    {-# INLINE fromRational #-}

---- | See note for 'Num' instance.
--instance Floating a => Floating (BVar s rs a) where
--    pi            = BVC pi
--    {-# INLINE pi #-}
--    exp   r       = BVOp (r :< Ø)        expOp
--    {-# INLINE exp #-}
--    log   r       = BVOp (r :< Ø)        logOp
--    {-# INLINE log #-}
--    sqrt  r       = BVOp (r :< Ø)        sqrtOp
--    {-# INLINE sqrt #-}
--    r1 ** r2      = BVOp (r1 :< r2 :< Ø) (**.)
--    {-# INLINE (**) #-}
--    logBase r1 r2 = BVOp (r1 :< r2 :< Ø) logBaseOp
--    {-# INLINE logBase #-}
--    sin   r       = BVOp (r :< Ø)        sinOp
--    {-# INLINE sin #-}
--    cos   r       = BVOp (r :< Ø)        cosOp
--    {-# INLINE cos #-}
--    tan   r       = BVOp (r :< Ø)        tanOp
--    {-# INLINE tan  #-}
--    asin  r       = BVOp (r :< Ø)        asinOp
--    {-# INLINE asin #-}
--    acos  r       = BVOp (r :< Ø)        acosOp
--    {-# INLINE acos #-}
--    atan  r       = BVOp (r :< Ø)        atanOp
--    {-# INLINE atan #-}
--    sinh  r       = BVOp (r :< Ø)        sinhOp
--    {-# INLINE sinh #-}
--    cosh  r       = BVOp (r :< Ø)        coshOp
--    {-# INLINE cosh #-}
--    tanh  r       = BVOp (r :< Ø)        tanhOp
--    {-# INLINE tanh #-}
--    asinh r       = BVOp (r :< Ø)        asinhOp
--    {-# INLINE asinh #-}
--    acosh r       = BVOp (r :< Ø)        acoshOp
--    {-# INLINE acosh #-}
--    atanh r       = BVOp (r :< Ø)        atanhOp
--    {-# INLINE atanh #-}

