{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Numeric.Backprop.Internal
-- Copyright   : (c) Justin Le 2018
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
  , liftOp
  , liftOp1, liftOp2, liftOp3, liftOp4
  , viewVar, setVar, sequenceVar, collectVar
  , backprop
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.IORef
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Index
import           Data.Type.Product hiding  (toList)
import           Data.Type.Util
import           Data.Type.Vector hiding   (itraverse)
import           GHC.Generics
import           Lens.Micro
import           Numeric.Backprop.Op
import           System.IO.Unsafe
import           Type.Class.Higher
import           Type.Class.Witness
import           Unsafe.Coerce
import qualified Data.Vector.Mutable       as MV

-- | A @'BVar' s a@ is a value of type @a@ that can be "backpropagated".
--
-- Functions referring to 'BVar's are tracked by the library and can be
-- automatically differentiated to get their gradients and results.
--
-- For simple numeric values, you can use its 'Num', 'Fractional', and
-- 'Floating' instances to manipulate them as if they were the numbers they
-- represent.
--
-- If @a@ contains items, the items can be accessed and extracted using
-- lenses. A @'Lens'' b a@ can be used to access an @a@ inside a @b@:
--
-- @
-- ('^.')  ::        a -> 'Lens'' a b ->        b
-- ('^^.') :: 'BVar' s a -> 'Lens'' a b -> 'BVar' s b
-- @
--
-- For more complex operations, libraries can provide functions on 'BVar's
-- using 'liftOp' and related functions.  This is how you can create
-- primitive functions that users can use to manipulate your library's
-- values.
--
-- For example, the /hmatrix/ library has a matrix-vector multiplication
-- function, @#> :: L m n -> R n -> L m@.
--
-- A library could instead provide a function @#> :: 'BVar' (L m n) -> BVar
-- (R n) -> BVar (R m)@, which the user can then use to manipulate their
-- 'BVar's of @L m n@s and @R n@s, etc.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
--
data BVar s a = BV { _bvRef :: !(BRef s)
                   , _bvVal :: !a
                   }

data BRef (s :: Type) = BRInp
                      | BRIx !Int
                      | BRC
  deriving Generic

instance NFData (BRef s)

-- | This will force the value inside, as well.
instance NFData a => NFData (BVar s a) where
    rnf (BV r v) = force r `seq` force v `seq` ()

-- | Project out a constant value if the 'BVar' refers to one.
bvConst :: BVar s a -> Maybe a
bvConst (BV BRC !x) = Just x
bvConst _           = Nothing
{-# INLINE bvConst #-}

forceBVar :: BVar s a -> ()
forceBVar (BV !r !_) = force r `seq` ()
{-# INLINE forceBVar #-}

data InpRef :: Type -> Type where
    IR :: Num a
       => { _irIx  :: !(BVar s b)
          , _irUpd :: !(Lens' b a)
          }
       -> InpRef a

forceInpRef :: InpRef a -> ()
forceInpRef (IR !v !_) = forceBVar v `seq` ()
{-# INLINE forceInpRef #-}

data TapeNode :: Type -> Type where
    TN :: { _tnInputs :: !(Prod InpRef as)
          , _tnGrad   :: !(a -> Tuple as)
          }
       -> TapeNode a

forceTapeNode :: TapeNode a -> ()
forceTapeNode (TN !inps !_) = foldMap1 forceInpRef inps `seq` ()
{-# INLINE forceTapeNode #-}

data SomeTapeNode :: Type where
    STN :: forall a. Num a
        => !(TapeNode a)
        -> SomeTapeNode

forceSomeTapeNode :: SomeTapeNode -> ()
forceSomeTapeNode (STN !tn) = forceTapeNode tn `seq` ()
{-# INLINE forceSomeTapeNode #-}

-- | An ephemeral Wengert Tape in the environment.  Used internally to
-- track of the computational graph of variables.
--
-- For the end user, one can just imagine @'Reifies' s 'W'@ as a required
-- constraint on @s@ that allows backpropagation to work.
newtype W = W { wRef :: IORef (Int, [SomeTapeNode]) }

initWengert :: IO W
initWengert = W <$> newIORef (0,[])
{-# INLINE initWengert #-}

insertNode
    :: Num a
    => TapeNode a
    -> a
    -> W
    -> IO (BVar s a)
insertNode !tn !x !w = fmap ((`BV` x) . BRIx) . atomicModifyIORef' (wRef w) $ \(!(!n,!t)) ->
    let n' = n + 1
        t' = STN tn:t
    in  forceTapeNode tn `seq` n' `seq` t' `seq` ((n', t'), n)
{-# INLINE insertNode #-}

-- | Lift a value into a 'BVar' representing a constant value.
--
-- This value will not be considered an input, and its gradients will not
-- be backpropagated.
constVar :: a -> BVar s a
constVar = BV BRC
{-# INLINE constVar #-}

liftOp_
    :: forall s as b. (Reifies s W, Num b, Every Num as)
    => Op as b
    -> Prod (BVar s) as
    -> IO (BVar s b)
liftOp_ o !vs = case traverse1 (fmap I . bvConst) vs of
                   Just xs -> return $ constVar (evalOp o xs)
                   Nothing -> insertNode tn y (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (map1 (I . _bvVal) vs)
    tn = TN { _tnInputs = imap1 go vs
            , _tnGrad   = g
            }
    go :: forall a. Index as a -> BVar s a -> InpRef a
    go i !v = forceBVar v `seq` (IR v id \\ every @_ @Num i)
{-# INLINE liftOp_ #-}

-- | Lift an 'Op' with an arbitrary number of inputs to a function on the
-- appropriate number of 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information, and "Numeric.Backprop.Op#prod" for a mini-tutorial on using
-- 'Prod' and 'Tuple'.
liftOp
    :: forall s as b. (Reifies s W, Num b, Every Num as)
    => Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOp o !vs = unsafePerformIO $ liftOp_ o vs
{-# INLINE liftOp #-}

liftOp1_
    :: forall s a b. (Reifies s W, Num a, Num b)
    => Op '[a] b
    -> BVar s a
    -> IO (BVar s b)
liftOp1_ o (bvConst->Just x) = return . constVar . evalOp o $ (x ::< Ø)
liftOp1_ o !v = forceBVar v `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (_bvVal v ::< Ø)
    tn = TN { _tnInputs = IR v id :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp1_ #-}

-- | Lift an 'Op' with a single input to be a function on a single 'BVar'.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp1
    :: forall s a b. (Reifies s W, Num a, Num b)
    => Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 o !v = unsafePerformIO $ liftOp1_ o v
{-# INLINE liftOp1 #-}

liftOp2_
    :: forall s a b c. (Reifies s W, Num a, Num b, Num c)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> IO (BVar s c)
liftOp2_ o (bvConst->Just x) (bvConst->Just y) = return . constVar . evalOp o $ x ::< y ::< Ø
liftOp2_ o !v !u = forceBVar v
             `seq` forceBVar u
             `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (_bvVal v ::< _bvVal u ::< Ø)
    tn = TN { _tnInputs = IR v id :< IR u id :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp2_ #-}

-- | Lift an 'Op' with two inputs to be a function on a two 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp2
    :: forall s a b c. (Reifies s W, Num a, Num b, Num c)
    => Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 o !v !u = unsafePerformIO $ liftOp2_ o v u
{-# INLINE liftOp2 #-}

liftOp3_
    :: forall s a b c d. (Reifies s W, Num a, Num b, Num c, Num d)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> IO (BVar s d)
liftOp3_ o (bvConst->Just x) (bvConst->Just y) (bvConst->Just z)
    = return . constVar . evalOp o $ x ::< y ::< z ::< Ø
liftOp3_ o !v !u !w = forceBVar v
                `seq` forceBVar u
                `seq` forceBVar w
                `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y, g) = runOpWith o (_bvVal v ::< _bvVal u ::< _bvVal w ::< Ø)
    tn = TN { _tnInputs = IR v id :< IR u id :< IR w id :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp3_ #-}

-- | Lift an 'Op' with three inputs to be a function on a three 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp3
    :: forall s a b c d. (Reifies s W, Num a, Num b, Num c, Num d)
    => Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 o !v !u !w = unsafePerformIO $ liftOp3_ o v u w
{-# INLINE liftOp3 #-}

liftOp4_
    :: forall s a b c d e. (Reifies s W, Num a, Num b, Num c, Num d, Num e)
    => Op '[a,b,c,d] e
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
    -> IO (BVar s e)
liftOp4_ o (bvConst->Just x) (bvConst->Just y) (bvConst->Just z) (bvConst->Just w)
    = return . constVar . evalOp o $ x ::< y ::< z ::< w ::< Ø
liftOp4_ o !v !u !w !x = forceBVar v
                   `seq` forceBVar u
                   `seq` forceBVar w
                   `seq` forceBVar x
                   `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y, g) = runOpWith o (_bvVal v ::< _bvVal u ::< _bvVal w ::< _bvVal x ::< Ø)
    tn = TN { _tnInputs = IR v id :< IR u id :< IR w id :< IR x id :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp4_ #-}

-- | Lift an 'Op' with four inputs to be a function on a four 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp4
    :: forall s a b c d e. (Reifies s W, Num a, Num b, Num c, Num d, Num e)
    => Op '[a,b,c,d] e
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
    -> BVar s e
liftOp4 o !v !u !w !x = unsafePerformIO $ liftOp4_ o v u w x
{-# INLINE liftOp4 #-}

viewVar_
    :: forall a b s. (Reifies s W, Num a)
    => Lens' b a
    -> BVar s b
    -> IO (BVar s a)
viewVar_ l !v = forceBVar v `seq` insertNode tn y (reflect (Proxy @s))
  where
    y = _bvVal v ^. l
    tn = TN { _tnInputs = IR v l :< Ø
            , _tnGrad   = only_
            }
{-# INLINE viewVar_ #-}

-- | Using a 'Lens'', extract a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to 'view' from lens.
--
-- See documentation for '^^.' for more information.
viewVar
    :: forall a b s. (Reifies s W, Num a)
    => Lens' b a
    -> BVar s b
    -> BVar s a
viewVar l !v = unsafePerformIO $ viewVar_ l v
{-# INLINE viewVar #-}

setVar_
    :: forall a b s. (Reifies s W, Num a, Num b)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> IO (BVar s b)
setVar_ l !w !v = forceBVar v
            `seq` forceBVar w
            `seq` insertNode tn y (reflect (Proxy @s))
  where
    y = _bvVal v & l .~ _bvVal w
    tn = TN { _tnInputs = IR w id :< IR v id :< Ø
            , _tnGrad   = \d -> let (dw,dv) = l (,0) d
                                in  dw ::< dv ::< Ø
            }
{-# INLINE setVar_ #-}

-- | Using a 'Lens'', set a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to "set" from lens.
--
-- See documentation for '.~~' for more information.
setVar
    :: forall a b s. (Reifies s W, Num a, Num b)
    => Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
setVar l !w !v = unsafePerformIO $ setVar_ l w v
{-# INLINE setVar #-}

sequenceVar_
    :: forall t a s. (Reifies s W, Traversable t, Num a)
    => BVar s (t a)
    -> IO (t (BVar s a))
sequenceVar_ !v = forceBVar v `seq` itraverse go (_bvVal v)
  where
    go :: Int -> a -> IO (BVar s a)
    go i y = insertNode tn y (reflect (Proxy @s))
      where
        tn = TN { _tnInputs = IR v (ixt i) :< Ø
                , _tnGrad   = only_
                }
{-# INLINE sequenceVar_ #-}

-- | Extract all of the 'BVar's out of a 'Traversable' container of
-- 'BVar's.
sequenceVar
    :: forall t a s. (Reifies s W, Traversable t, Num a)
    => BVar s (t a)
    -> t (BVar s a)
sequenceVar !v = unsafePerformIO $ sequenceVar_ v
{-# INLINE sequenceVar #-}

collectVar_
    :: forall a t s. (Reifies s W, Foldable t, Functor t, Num (t a), Num a)
    => t (BVar s a)
    -> IO (BVar s (t a))
collectVar_ !vs = withV (toList vs) $ \(vVec :: Vec n (BVar s a)) -> do
    let tn :: TapeNode (t a)
        tn = TN { _tnInputs = vecToProd (vmap ((`IR` id) . getI) vVec)
                , _tnGrad   = maybe (error "distributeVar") vecToProd
                            . listToVec (vecLen vVec)
                            . map I . toList
                }
    traverse_ (evaluate . forceBVar) vs
    insertNode tn (_bvVal <$> vs) (reflect (Proxy @s))
{-# INLINE collectVar_ #-}

-- | Collect all of the 'BVar's in a container into a 'BVar' of that
-- container's contents.
collectVar
    :: forall a t s. (Reifies s W, Foldable t, Functor t, Num (t a), Num a)
    => t (BVar s a)
    -> BVar s (t a)
collectVar !vs = unsafePerformIO $ collectVar_ vs
{-# INLINE collectVar #-}

data SomeNum :: Type where
    SN  :: Num a
        => Proxy a
        -> a
        -> SomeNum

data Runner s = R { _rDelta :: MV.MVector s SomeNum
                  }

initRunner
    :: (PrimMonad m, PrimState m ~ s)
    => (Int, [SomeTapeNode])
    -> m (Runner s)
initRunner (n, stns) = R <$> do
    r <- MV.new n
    for_ (zip [n-1,n-2..] stns) $ \(i, STN (TN{..} :: TapeNode c)) -> do
      MV.write r i $ SN (Proxy @c) 0
    return r
{-# INLINE initRunner #-}

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
      let gs = _tnGrad (unsafeCoerce delt)
      zipWithPM_ propagate _tnInputs gs
    propagate :: forall x. InpRef x -> I x -> m ()
    propagate (IR v ln) (I !d) = case _bvRef v of
      BRInp   -> modifyMutVar' (unsafeCoerce dx) (ln %~ (+ d))    -- bad for tuples
      BRIx !i -> flip (MV.modify _rDelta) i $ \case
        SN p !y -> let y' = unsafeCoerce y & ln %~ (+d)
                   in  y' `seq` SN p (unsafeCoerce y')
      BRC     -> return ()
{-# INLINE gradRunner #-}

registerOut
    :: (Reifies s W, Num a)
    => BVar s a
    -> IO a
registerOut !v = _bvVal v <$ liftOp1_ idOp v
{-# INLINE registerOut #-}

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents, also computing its gradient @a@ as well.
--
-- The Rank-N type @forall s. 'Reifies' s 'W' => ...@ is used to ensure
-- that 'BVar's do not leak out of the context (similar to how it is used
-- in "Control.Monad.ST"), and also as a reference to an ephemeral Wengert
-- tape used to track the graph of references.
--
-- Note that every type involved has to be an instance of 'Num'.  This is
-- because gradients all need to be "summable" (which is implemented using
-- 'sum' and '+'), and we also need to able to generate gradients of 1
-- and 0.  Really, only '+' and 'fromInteger' methods are used from the
-- 'Num' typeclass.
backprop
    :: forall a b. (Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> a
    -> (b, a)
backprop f x = (y, g)
  where
    !(!tp@(!_,!_),!y) = unsafePerformIO $ do
      w <- initWengert
      o <- reify w $ \(Proxy :: Proxy s) ->
        registerOut =<< evaluate (f (BV (BRInp @s) x))
      t <- readIORef (wRef w)
      traverse_ (evaluate . forceSomeTapeNode) (snd t)
      return (t, o)
    g :: a
    g = runST $ do
      r <- initRunner tp
      o <- newMutVar (0 :: a)
      gradRunner (Proxy @b) r tp o
      readMutVar o
{-# INLINE backprop #-}

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
    fromInteger = constVar . fromInteger
    {-# INLINE fromInteger #-}

instance (Fractional a, Reifies s W) => Fractional (BVar s a) where
    (/)          = liftOp2 (/.)
    {-# INLINE (/) #-}
    recip        = liftOp1 recipOp
    {-# INLINE recip #-}
    fromRational = constVar . fromRational
    {-# INLINE fromRational #-}

instance (Floating a, Reifies s W) => Floating (BVar s a) where
    pi      = constVar pi
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

-- Some utility functions to get around a lens dependency
itraverse
    :: forall t a b f. (Traversable t, Monad f)
    => (Int -> a -> f b) -> t a -> f (t b)
itraverse f xs = evalStateT (traverse (StateT . go) xs) 0
  where
    go :: a -> Int -> f (b, Int)
    go x i = (,i+1) <$> f i x
{-# INLINE itraverse #-}

ixt :: forall t a. Traversable t => Int -> Lens' (t a) a
ixt i f xs = stuff <$> ixi i f contents
  where
    contents = toList xs
    stuff    = evalState (traverse (state . const go) xs)
      where
        go :: [a] -> (a,  [a])
        go []     = error "asList"
        go (y:ys) = (y, ys)
{-# INLINE ixt #-}

ixi :: Int -> Lens' [a] a
ixi _ _ []     = error "ixi"
ixi 0 f (x:xs) = (:xs) <$> f x
ixi n f (x:xs) = (x:)  <$> ixi (n - 1) f xs
{-# INLINE ixi #-}

