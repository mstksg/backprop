{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

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
  , backpropN, evalBPN
  , constVar
  , liftOp, liftOp1, liftOp2, liftOp3
  , viewVar, setVar, sequenceVar, collectVar, previewVar, toListOfVar
  , coerceVar
  -- * Func wrappers
  , ZeroFunc(..), zfNum, zeroFunc
  , AddFunc(..), afNum, addFunc
  , OneFunc(..), ofNum, oneFunc
  -- * Debug
  , debugSTN
  , debugIR
  -- * Generics
  , BVGroup(..)
  , splitBV, joinBV
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable
import           Data.Function
import           Data.Functor.Identity
import           Data.IORef
import           Data.Kind
import           Data.Maybe
import           Data.Monoid hiding           (Any(..))
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Conjunction hiding ((:*:))
import           Data.Type.Length
import           Data.Type.Product hiding     (toList)
import           Data.Type.Util
import           Data.Type.Vector hiding      (itraverse)
import           Data.Typeable
import           GHC.Exts                     (Any)
import           GHC.Generics                 as G
import           Lens.Micro
import           Numeric.Backprop.Class
import           Numeric.Backprop.Op
import           System.IO.Unsafe
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Family.List
import           Unsafe.Coerce
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as MV

-- | "Zero out" all components of a value.  For scalar values, this should
-- just be @'const' 0@.  For vectors and matrices, this should set all
-- components to zero, the additive identity.
--
-- Should be idempotent: Applying the function twice is the same as
-- applying it just once.
--
-- Each type should ideally only have one 'ZeroFunc'.  This coherence
-- constraint is given by the typeclass 'Backprop'.
--
-- @since 0.2.0.0
newtype ZeroFunc a = ZF { runZF :: a -> a }

-- | Add together two values of a type.  To combine contributions of
-- gradients, so should ideally be information-preserving.
--
-- See laws for 'Backprop' for the laws this should be expected to
-- preserve.  Namely, it should be commutative and associative, with an
-- identity for a valid 'ZeroFunc'.
--
-- Each type should ideally only have one 'AddFunc'.  This coherence
-- constraint is given by the typeclass 'Backprop'.
--
-- @since 0.2.0.0
newtype AddFunc  a = AF { runAF :: a -> a -> a }

-- | "One" all components of a value.  For scalar values, this should
-- just be @'const' 1@.  For vectors and matrices, this should set all
-- components to one, the multiplicative identity.
--
-- Should be idempotent: Applying the function twice is the same as
-- applying it just once.
--
-- Each type should ideally only have one 'OneFunc'.  This coherence
-- constraint is given by the typeclass 'Backprop'.
--
-- @since 0.2.0.0
newtype OneFunc  a = OF { runOF :: a -> a }

-- | If a type has a 'Num' instance, this is the canonical 'ZeroFunc'.
--
-- @since 0.2.0.0
zfNum :: Num a => ZeroFunc a
zfNum = ZF (const 0)
{-# INLINE zfNum #-}

-- | If a type has a 'Num' instance, this is the canonical 'AddFunc'.
--
-- @since 0.2.0.0
afNum :: Num a => AddFunc a
afNum = AF (+)
{-# INLINE afNum #-}

-- | If a type has a 'Num' instance, this is the canonical 'OneFunc'.
--
-- @since 0.2.0.0
ofNum :: Num a => OneFunc a
ofNum = OF (const 1)
{-# INLINE ofNum #-}

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
-- lenses. A @'Lens'' b a@ can be used to access an @a@ inside a @b@, using
-- '^^.' ('viewVar'):
--
-- @
-- ('^.')  ::        a -> 'Lens'' a b ->        b
-- ('^^.') :: 'BVar' s a -> 'Lens'' a b -> 'BVar' s b
-- @
--
-- There is also '^^?' ('previewVar'), to use a 'Prism'' or 'Traversal'' to
-- extract a target that may or may not be present (which can implement
-- pattern matching), '^^..' ('toListOfVar') to use a 'Traversal'' to
-- extract /all/ targets inside a 'BVar', and '.~~' ('setVar') to set and
-- update values inside a 'BVar'.
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

-- | @since 0.1.5.1
deriving instance Typeable (BVar s a)

data BRef (s :: Type) = BRInp !Int
                      | BRIx !Int
                      | BRC
  deriving (Generic, Show)

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
forceBVar (BV r !_) = force r `seq` ()
{-# INLINE forceBVar #-}

data InpRef :: Type -> Type where
    IR :: { _irIx  :: !(BVar s b)
          , _irAdd :: !(a -> b -> b)
          }
       -> InpRef a

forceInpRef :: InpRef a -> ()
forceInpRef (IR v !_) = forceBVar v `seq` ()
{-# INLINE forceInpRef #-}

-- | Debugging string for an 'InpRef'.
debugIR :: InpRef a -> String
debugIR IR{..} = show (_bvRef _irIx)

data TapeNode :: Type -> Type where
    TN :: { _tnInputs :: !(Prod InpRef as)
          , _tnGrad   :: !(a -> Tuple as)
          }
       -> TapeNode a

forceTapeNode :: TapeNode a -> ()
forceTapeNode (TN inps !_) = foldMap1 forceInpRef inps `seq` ()
{-# INLINE forceTapeNode #-}

data SomeTapeNode :: Type where
    STN :: { _stnZero :: a
           , _stnNode :: !(TapeNode a)
           }
        -> SomeTapeNode

-- | Debugging string for a 'SomeTapeMode'.
debugSTN :: SomeTapeNode -> String
debugSTN (STN _ TN{..}) = show . foldMap1 ((:[]) . debugIR) $ _tnInputs

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
    :: TapeNode a
    -> a                    -- ^ val
    -> ZeroFunc a
    -> W
    -> IO (BVar s a)
insertNode tn !x zf !w = fmap ((`BV` x) . BRIx) . atomicModifyIORef' (wRef w) $ \(!n,!t) ->
    let n' = n + 1
        t' = STN (runZF zf x) tn : t
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
    :: forall s as b. Reifies s W
    => Prod AddFunc as
    -> ZeroFunc b
    -> Op as b
    -> Prod (BVar s) as
    -> IO (BVar s b)
liftOp_ afs z o !vs = case traverse1 (fmap I . bvConst) vs of
    Just xs -> return $ constVar (evalOp o xs)
    Nothing -> insertNode tn y z (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (map1 (I . _bvVal) vs)
    tn = TN { _tnInputs = map1 go (zipP afs vs)
            , _tnGrad   = g
            }
    go :: forall a. (AddFunc :&: BVar s) a -> InpRef a
    go (af :&: (!v)) = forceBVar v `seq` IR v (runAF af)
    {-# INLINE go #-}
{-# INLINE liftOp_ #-}

-- | 'Numeric.Backprop.liftOp', but with explicit 'add' and 'zero'.
liftOp
    :: forall as b s. Reifies s W
    => Prod AddFunc as
    -> ZeroFunc b
    -> Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOp afs z o !vs = unsafePerformIO $ liftOp_ afs z o vs
{-# INLINE liftOp #-}

liftOp1_
    :: forall a b s. Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> Op '[a] b
    -> BVar s a
    -> IO (BVar s b)
liftOp1_ _  _ o (bvConst->Just x) = return . constVar . evalOp o $ (x ::< Ø)
liftOp1_ af z o v = forceBVar v `seq` insertNode tn y z (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (_bvVal v ::< Ø)
    tn = TN { _tnInputs = IR v (runAF af) :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp1_ #-}

-- | 'Numeric.Backprop.liftOp1', but with explicit 'add' and 'zero'.
liftOp1
    :: forall a b s. Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 af z o !v = unsafePerformIO $ liftOp1_ af z o v
{-# INLINE liftOp1 #-}

liftOp2_
    :: forall a b c s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc c
    -> Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> IO (BVar s c)
liftOp2_ _ _ _ o (bvConst->Just x) (bvConst->Just y)
    = return . constVar . evalOp o $ x ::< y ::< Ø
liftOp2_ afa afb z o v u = forceBVar v
                     `seq` forceBVar u
                     `seq` insertNode tn y z (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (_bvVal v ::< _bvVal u ::< Ø)
    tn = TN { _tnInputs = IR v (runAF afa) :< IR u (runAF afb) :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp2_ #-}

-- | 'Numeric.Backprop.liftOp2', but with explicit 'add' and 'zero'.
liftOp2
    :: forall a b c s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc c
    -> Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 afa afb z o !v !u = unsafePerformIO $ liftOp2_ afa afb z o v u
{-# INLINE liftOp2 #-}

liftOp3_
    :: forall a b c d s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> ZeroFunc d
    -> Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> IO (BVar s d)
liftOp3_ _ _ _ _ o (bvConst->Just x) (bvConst->Just y) (bvConst->Just z)
    = return . constVar . evalOp o $ x ::< y ::< z ::< Ø
liftOp3_ afa afb afc z o v u w = forceBVar v
                           `seq` forceBVar u
                           `seq` forceBVar w
                           `seq` insertNode tn y z (reflect (Proxy @s))
  where
    (y, g) = runOpWith o (_bvVal v ::< _bvVal u ::< _bvVal w ::< Ø)
    tn = TN { _tnInputs = IR v (runAF afa)
                       :< IR u (runAF afb)
                       :< IR w (runAF afc)
                       :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp3_ #-}

-- | 'Numeric.Backprop.liftOp3', but with explicit 'add' and 'zero'.
liftOp3
    :: forall a b c d s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> ZeroFunc d
    -> Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 afa afb afc z o !v !u !w = unsafePerformIO $ liftOp3_ afa afb afc z o v u w
{-# INLINE liftOp3 #-}

-- TODO: can we get the zero and scale func from the bvar?
viewVar_
    :: forall a b s. Reifies s W
    => AddFunc a
    -> ZeroFunc a
    -> Lens' b a
    -> BVar s b
    -> IO (BVar s a)
viewVar_ af z l v = forceBVar v `seq` insertNode tn y z (reflect (Proxy @s))
  where
    y = _bvVal v ^. l
    tn = TN { _tnInputs = IR v (over l . runAF af) :< Ø
            , _tnGrad   = only_
            }
{-# INLINE viewVar_ #-}

-- | 'Numeric.Backprop.viewVar', but with explicit 'add' and 'zero'.
viewVar
    :: forall a b s. Reifies s W
    => AddFunc a
    -> ZeroFunc a
    -> Lens' b a
    -> BVar s b
    -> BVar s a
viewVar af z l !v = unsafePerformIO $ viewVar_ af z l v
{-# INLINE viewVar #-}

-- TODO: can zero and scale func be gotten from the input bvars?
setVar_
    :: forall a b s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc a
    -> ZeroFunc b
    -> Lens' b a
    -> BVar s a
    -> BVar s b
    -> IO (BVar s b)
setVar_ afa afb za zb l w v = forceBVar v
                        `seq` forceBVar w
                        `seq` insertNode tn y zb (reflect (Proxy @s))
  where
    y = _bvVal v & l .~ _bvVal w
    tn = TN { _tnInputs = IR w (runAF afa) :< IR v (runAF afb) :< Ø
            , _tnGrad   = \d -> let (dw,dv) = l (\x -> (x, runZF za x)) d
                                in  dw ::< dv ::< Ø
            }
{-# INLINE setVar_ #-}

-- | 'Numeric.Backprop.setVar', but with explicit 'add' and 'zero'.
setVar
    :: forall a b s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc a
    -> ZeroFunc b
    -> Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
setVar afa afb za zb l !w !v = unsafePerformIO $ setVar_ afa afb za zb l w v
{-# INLINE setVar #-}

-- | 'Numeric.Backprop.sequenceVar', but with explicit 'add' and 'zero'.
sequenceVar
    :: forall t a s. (Reifies s W, Traversable t)
    => AddFunc a
    -> ZeroFunc a
    -> BVar s (t a)
    -> t (BVar s a)
sequenceVar af z !v = unsafePerformIO $ traverseVar' af z id traverse v
{-# INLINE sequenceVar #-}

-- TODO: can scale funcs and zeros be had from bvars and Functor instance?
collectVar_
    :: forall t a s. (Reifies s W, Foldable t, Functor t)
    => AddFunc a
    -> ZeroFunc a
    -> ZeroFunc (t a)
    -> t (BVar s a)
    -> IO (BVar s (t a))
collectVar_ af z z' !vs = withV (toList vs) $ \(vVec :: Vec n (BVar s a)) -> do
    let tn :: TapeNode (t a)
        tn = TN
          { _tnInputs = vecToProd (vmap ((`IR` runAF af) . getI) vVec)
          , _tnGrad   = vecToProd
                      . zipVecList (\(I v) -> I . fromMaybe (runZF z (_bvVal v))) vVec
                      . toList
          }
    traverse_ (evaluate . forceBVar) vs
    insertNode tn (_bvVal <$> vs) z' (reflect (Proxy @s))
{-# INLINE collectVar_ #-}

-- | 'Numeric.Backprop.collectVar', but with explicit 'add' and 'zero'.
collectVar
    :: forall t a s. (Reifies s W, Foldable t, Functor t)
    => AddFunc a
    -> ZeroFunc a
    -> ZeroFunc (t a)
    -> t (BVar s a)
    -> BVar s (t a)
collectVar af z z' !vs = unsafePerformIO $ collectVar_ af z z' vs
{-# INLINE collectVar #-}

traverseVar'
    :: forall b a f s. (Reifies s W, Traversable f)
    => AddFunc a
    -> ZeroFunc a
    -> (b -> f a)
    -> Traversal' b a
    -> BVar s b
    -> IO (f (BVar s a))
traverseVar' af z f t v = forceBVar v
                    `seq` itraverse go (f (_bvVal v))
  where
    go :: Int -> a -> IO (BVar s a)
    go i y = insertNode tn y z (reflect (Proxy @s))
      where
        tn = TN { _tnInputs = IR v (over (ixt t i) . runAF af) :< Ø
                , _tnGrad   = only_
                }
    {-# INLINE go #-}
{-# INLINE traverseVar' #-}

-- | 'Numeric.Backprop.previewVar', but with explicit 'add' and 'zero'.
previewVar
    :: forall b a s. Reifies s W
    => AddFunc a
    -> ZeroFunc a
    -> Traversal' b a
    -> BVar s b
    -> Maybe (BVar s a)
previewVar af z t !v = unsafePerformIO $ traverseVar' af z (listToMaybe . toListOf t) t v
{-# INLINE previewVar #-}

-- | 'Numeric.Backprop.toListOfVar', but with explicit 'add' and 'zero'.
toListOfVar
    :: forall b a s. Reifies s W
    => AddFunc a
    -> ZeroFunc a
    -> Traversal' b a
    -> BVar s b
    -> [BVar s a]
toListOfVar af z t !v = unsafePerformIO $ traverseVar' af z (toListOf t) t v
{-# INLINE toListOfVar #-}

-- | Coerce a 'BVar' contents.  Useful for things like newtype wrappers.
--
-- @since 0.1.5.2
coerceVar
    :: Coercible a b
    => BVar s a
    -> BVar s b
coerceVar v@(BV r x) = forceBVar v `seq` BV r (coerce x)

data Runner s = R { _rDelta  :: !(MV.MVector s Any)
                  , _rInputs :: !(MV.MVector s Any)
                  }

initRunner
    :: (PrimMonad m, PrimState m ~ s)
    => (Int, [SomeTapeNode])
    -> (Int, [Any])
    -> m (Runner s)
initRunner (n, stns) (nx,xs) = do
    delts <- MV.new n
    for_ (zip [n-1,n-2..] stns) $ \(i, STN z (TN{..} :: TapeNode c)) ->
      MV.write delts i $ unsafeCoerce z
    inps <- MV.new nx
    for_ (zip [0..] xs) . uncurry $ \i z ->
      MV.write inps i z
    return $ R delts inps
{-# INLINE initRunner #-}

gradRunner
    :: forall m b s. (PrimMonad m, PrimState m ~ s)
    => b                        -- ^ one
    -> Runner s
    -> (Int, [SomeTapeNode])
    -> m ()
gradRunner o R{..} (n,stns) = do
    when (n > 0) $
      MV.write _rDelta (n - 1) (unsafeCoerce o)
    zipWithM_ go [n-1,n-2..] stns
  where
    go :: Int -> SomeTapeNode -> m ()
    go i (STN _ TN{..}) = do
      delt <- MV.read _rDelta i
      let gs = _tnGrad (unsafeCoerce delt)
      zipWithPM_ propagate _tnInputs gs
    {-# INLINE go #-}
    propagate :: forall x. InpRef x -> I x -> m ()
    propagate (IR v (+*)) (I d) = case _bvRef v of
      BRInp i -> flip (MV.modify _rInputs) i $
        unsafeCoerce . (d +*) . unsafeCoerce
      BRIx i -> flip (MV.modify _rDelta) i $
        unsafeCoerce . (d +*) . unsafeCoerce
      BRC     -> return ()
    {-# INLINE propagate #-}
{-# INLINE gradRunner #-}

-- | 'Numeric.Backprop.backpropN', but with explicit 'zero' and 'one'.
backpropN
    :: forall as b. ()
    => Prod ZeroFunc as
    -> OneFunc b
    -> (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, Tuple as)
backpropN zfs ofb f !xs = (y, g)
  where
    !(!tp@(!_,!_),!y) = unsafePerformIO $ fillWengert f xs
    g :: Tuple as
    g = runST $ do
        r <- initRunner tp $ bimap getSum (`appEndo` [])
                           . fst
                           $ zipWithPM_ go zfs xs
        gradRunner (runOF ofb y) r tp
        delts <- toList <$> V.freeze (_rInputs r)
        return . fromMaybe (error "backpropN") $
          fillProd (\_ d -> I (unsafeCoerce d)) xs delts
      where
        go :: forall a. ZeroFunc a -> I a -> ((Sum Int, Endo [Any]),())
        go zf (I x) = ((1, Endo (unsafeCoerce (runZF zf x) :)), ())
        {-# INLINE go #-}
{-# INLINE backpropN #-}

-- | 'evalBP' generalized to multiple inputs of different types.  See
-- documentation for 'Numeric.Backprop.backpropN' for more details.
evalBPN
    :: forall as b. ()
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> b
evalBPN f = snd . unsafePerformIO . fillWengert f
{-# INLINE evalBPN #-}

fillWengert
    :: forall as b. ()
    => (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> IO ((Int, [SomeTapeNode]), b)
fillWengert f xs = do
    w <- initWengert
    o <- reify w $ \(Proxy :: Proxy s) -> do
      let oVar = f (inpProd @s)
      evaluate (forceBVar oVar)
      return (_bvVal oVar)
    t <- readIORef (wRef w)
    return (t, o)
  where
    inpProd :: forall s. Prod (BVar s) as
    inpProd = evalState (traverse1 (state . go . getI) xs) 0
      where
        go :: a -> Int -> (BVar s a, Int)
        go x i = (BV (BRInp i) x, i + 1)
        {-# INLINE go #-}
    {-# INLINE inpProd #-}
{-# INLINE fillWengert #-}


instance (Num a, Reifies s W) => Num (BVar s a) where
    (+)         = liftOp2 afNum afNum zfNum (+.)
    {-# INLINE (+) #-}
    (-)         = liftOp2 afNum afNum zfNum (-.)
    {-# INLINE (-) #-}
    (*)         = liftOp2 afNum afNum zfNum (*.)
    {-# INLINE (*) #-}
    negate      = liftOp1 afNum zfNum negateOp
    {-# INLINE negate #-}
    signum      = liftOp1 afNum zfNum signumOp
    {-# INLINE signum #-}
    abs         = liftOp1 afNum zfNum absOp
    {-# INLINE abs #-}
    fromInteger = constVar . fromInteger
    {-# INLINE fromInteger #-}

instance (Fractional a, Reifies s W) => Fractional (BVar s a) where
    (/)          = liftOp2 afNum afNum zfNum (/.)
    {-# INLINE (/) #-}
    recip        = liftOp1 afNum zfNum recipOp
    {-# INLINE recip #-}
    fromRational = constVar . fromRational
    {-# INLINE fromRational #-}

instance (Floating a, Reifies s W) => Floating (BVar s a) where
    pi      = constVar pi
    {-# INLINE pi #-}
    exp     = liftOp1 afNum zfNum expOp
    {-# INLINE exp #-}
    log     = liftOp1 afNum zfNum logOp
    {-# INLINE log #-}
    sqrt    = liftOp1 afNum zfNum sqrtOp
    {-# INLINE sqrt #-}
    (**)    = liftOp2 afNum afNum zfNum (**.)
    {-# INLINE (**) #-}
    logBase = liftOp2 afNum afNum zfNum logBaseOp
    {-# INLINE logBase #-}
    sin     = liftOp1 afNum zfNum sinOp
    {-# INLINE sin #-}
    cos     = liftOp1 afNum zfNum cosOp
    {-# INLINE cos #-}
    tan     = liftOp1 afNum zfNum tanOp
    {-# INLINE tan  #-}
    asin    = liftOp1 afNum zfNum asinOp
    {-# INLINE asin #-}
    acos    = liftOp1 afNum zfNum acosOp
    {-# INLINE acos #-}
    atan    = liftOp1 afNum zfNum atanOp
    {-# INLINE atan #-}
    sinh    = liftOp1 afNum zfNum sinhOp
    {-# INLINE sinh #-}
    cosh    = liftOp1 afNum zfNum coshOp
    {-# INLINE cosh #-}
    tanh    = liftOp1 afNum zfNum tanhOp
    {-# INLINE tanh #-}
    asinh   = liftOp1 afNum zfNum asinhOp
    {-# INLINE asinh #-}
    acosh   = liftOp1 afNum zfNum acoshOp
    {-# INLINE acosh #-}
    atanh   = liftOp1 afNum zfNum atanhOp
    {-# INLINE atanh #-}

-- | Compares the values inside the 'BVar'.
--
-- @since 0.1.5.0
instance Eq a => Eq (BVar s a) where
    (==) = (==) `on` _bvVal
    (/=) = (/=) `on` _bvVal

-- | Compares the values inside the 'BVar'.
--
-- @since 0.1.5.0
instance Ord a => Ord (BVar s a) where
    compare = compare `on` _bvVal
    (<)     = (<)     `on` _bvVal
    (<=)    = (<=)    `on` _bvVal
    (>)     = (>)     `on` _bvVal
    (>=)    = (>=)    `on` _bvVal

-- Some utility functions to get around a lens dependency
itraverse
    :: forall t a b f. (Traversable t, Monad f)
    => (Int -> a -> f b) -> t a -> f (t b)
itraverse f xs = evalStateT (traverse (StateT . go) xs) 0
  where
    go :: a -> Int -> f (b, Int)
    go x i = (,i+1) <$> f i x
{-# INLINE itraverse #-}

ixi :: Int -> Lens' [a] a
ixi _ _ []     = error "ixi"
ixi 0 f (x:xs) = (:xs) <$> f x
ixi n f (x:xs) = (x:)  <$> ixi (n - 1) f xs
{-# INLINE ixi #-}

ixt :: forall b a. Traversal' b a -> Int -> Lens' b a
ixt t i f xs = stuff <$> ixi i f contents
  where
    contents = xs ^.. t
    stuff    = evalState (traverseOf t (state . const go) xs)
      where
        go :: [a] -> (a,  [a])
        go []     = error "Numeric.Backprop.Internal: unexpected shape involved in gradient computation"
        go (y:ys) = (y, ys)
{-# INLINE ixt #-}

-- | @since 0.2.2.0
instance (Backprop a, Reifies s W) => Backprop (BVar s a) where
    zero = liftOp1 addFunc zeroFunc . op1 $ \x -> (zero x, zero)
    {-# INLINE zero #-}
    add  = liftOp2 addFunc addFunc zeroFunc . op2 $ \x y ->
        ( add x y
        , \d -> (d, d)
        )
    {-# INLINE add #-}
    one  = liftOp1 addFunc zeroFunc . op1 $ \x -> (one  x, zero)
    {-# INLINE one #-}

-- | The canonical 'ZeroFunc' for instances of 'Backprop'.
--
-- @since 0.2.0.0
zeroFunc :: Backprop a => ZeroFunc a
zeroFunc = ZF zero
{-# INLINE zeroFunc #-}

-- | The canonical 'AddFunc' for instances of 'Backprop'.
--
-- @since 0.2.0.0
addFunc :: Backprop a => AddFunc a
addFunc = AF add
{-# INLINE addFunc #-}

-- | The canonical 'OneFunc' for instances of 'Backprop'.
--
-- @since 0.2.0.0
oneFunc :: Backprop a => OneFunc a
oneFunc = OF one
{-# INLINE oneFunc #-}

p1 :: Lens' ((f :*: g) a) (f a)
p1 f (x :*: y) = (:*: y) <$> f x
{-# INLINE p1 #-}

p2 :: Lens' ((f :*: g) a) (g a)
p2 f (x :*: y) = (x :*:) <$> f y
{-# INLINE p2 #-}

s1 :: Traversal' ((f :+: g) a) (f a)
s1 f (L1 x) = L1 <$> f x
s1 _ (R1 y) = pure (R1 y)
{-# INLINE s1 #-}

s2 :: Traversal' ((f :+: g) a) (g a)
s2 _ (L1 x) = pure (L1 x)
s2 f (R1 y) = R1 <$> f y
{-# INLINE s2 #-}

class BVGroup s as i o | i o -> as where
    gsplitBV :: Prod AddFunc as -> Prod ZeroFunc as -> BVar s (i ()) -> o ()
    gjoinBV  :: Prod AddFunc as -> Prod ZeroFunc as -> o () -> BVar s (i ())

instance BVGroup s '[] (K1 i a) (K1 i (BVar s a)) where
    gsplitBV _ _ = K1 . coerceVar
    {-# INLINE gsplitBV #-}
    gjoinBV  _ _ = coerceVar . unK1
    {-# INLINE gjoinBV #-}

instance BVGroup s as (f a) (f (BVar s a))
        => BVGroup s as (M1 i c (f a)) (M1 i c (f (BVar s a))) where
    gsplitBV afs zfs = M1 . gsplitBV afs zfs . coerceVar @_ @(f a ())
    {-# INLINE gsplitBV #-}
    gjoinBV afs zfs = coerceVar @(f a ()) . gjoinBV afs zfs . unM1
    {-# INLINE gjoinBV #-}

instance BVGroup s '[] V1 V1 where
    gsplitBV _ _ v = case _bvVal v of
    {-# INLINE gsplitBV #-}
    gjoinBV _ _ = \case
    {-# INLINE gjoinBV #-}

instance BVGroup s '[] U1 U1 where
    gsplitBV _ _ _ = U1
    {-# INLINE gsplitBV #-}
    gjoinBV _ _ _ = constVar U1
    {-# INLINE gjoinBV #-}

instance ( Reifies s W
         , BVGroup s as (f a) (f (BVar s a))
         , BVGroup s bs (g b) (g (BVar s b))
         , cs ~ (as ++ bs)
         , Known Length as
         ) => BVGroup s (f a () ': g b () ': cs) (f a :*: g b) (f (BVar s a) :*: g (BVar s b)) where
    gsplitBV (afa :< afb :< afs) (zfa :< zfb :< zfs) xy = x :*: y
      where
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
        x = gsplitBV afas zfas . viewVar afa zfa p1 $ xy
        y = gsplitBV afbs zfbs . viewVar afb zfb p2 $ xy
    {-# INLINE gsplitBV #-}
    gjoinBV (afa :< afb :< afs) (zfa :< zfb :< zfs) (x :*: y)
        = liftOp2 afa afb zfab (opIso2 (:*:) unP)
            (gjoinBV afas zfas x)
            (gjoinBV afbs zfbs y)
      where
        zfab = ZF $ \(xx :*: yy) -> runZF zfa xx :*: runZF zfb yy
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
        unP (xx :*: yy) = (xx, yy)
    {-# INLINE gjoinBV #-}

instance ( Reifies s W
         , BVGroup s as (f a) (f (BVar s a))
         , BVGroup s bs (g b) (g (BVar s b))
         , cs ~ (as ++ bs)
         , Known Length as
         ) => BVGroup s (f a () ': g b () ': cs) (f a :+: g b) (f (BVar s a) :+: g (BVar s b)) where
    gsplitBV (afa :< afb :< afs) (zfa :< zfb :< zfs) xy =
        case previewVar afa zfa s1 xy of
          Just x -> L1 $ gsplitBV afas zfas x
          Nothing -> case previewVar afb zfb s2 xy of
            Just y -> R1 $ gsplitBV afbs zfbs y
            Nothing -> error "Numeric.Backprop.gsplitBV: Internal error occurred"
      where
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
    {-# INLINE gsplitBV #-}
    gjoinBV (afa :< afb :< afs) (zfa :< zfb :< zfs) = \case
        L1 x -> liftOp1 afa zf (op1 (\xx -> (L1 xx, \case L1 d -> d; R1 _ -> runZF zfa xx)))
                    (gjoinBV afas zfas x)
        R1 y -> liftOp1 afb zf (op1 (\yy -> (R1 yy, \case L1 _ -> runZF zfb yy; R1 d -> d)))
                    (gjoinBV afbs zfbs y)
      where
        (afas, afbs) = splitProd known afs
        (zfas, zfbs) = splitProd known zfs
        zf = ZF $ \case
            L1 xx -> L1 $ runZF zfa xx
            R1 yy -> R1 $ runZF zfb yy
    {-# INLINE gjoinBV #-}

splitBV
    :: forall z as s.
       ( Generic (z Identity)
       , Generic (z (BVar s))
       , BVGroup s as (Rep (z Identity)) (Rep (z (BVar s)))
       , Reifies s W
       )
    => AddFunc (Rep (z Identity) ())
    -> Prod AddFunc as
    -> ZeroFunc (Rep (z Identity) ())
    -> Prod ZeroFunc as
    -> BVar s (z Identity)
    -> z (BVar s)
splitBV af afs zf zfs =
        G.to
      . gsplitBV afs zfs
      . viewVar af zf (lens (from @(z Identity) @()) (const G.to))
{-# INLINE splitBV #-}

joinBV
    :: forall z as s.
       ( Generic (z Identity)
       , Generic (z (BVar s))
       , BVGroup s as (Rep (z Identity)) (Rep (z (BVar s)))
       , Reifies s W
       )
    => AddFunc (z Identity)
    -> Prod AddFunc as
    -> ZeroFunc (z Identity)
    -> Prod ZeroFunc as
    -> z (BVar s)
    -> BVar s (z Identity)
joinBV af afs zf zfs =
        viewVar af zf (lens G.to (const G.from))
      . gjoinBV afs zfs
      . from @(z (BVar s)) @()
{-# INLINE joinBV #-}
