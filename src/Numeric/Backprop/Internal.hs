{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK not-home     #-}

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
  , backpropWithN, evalBPN
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
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable
import           Data.Function
import           Data.IORef
import           Data.Kind
import           Data.Maybe
import           Data.Monoid hiding           (Any(..))
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Conjunction hiding ((:*:))
import           Data.Type.Product hiding     (toList)
import           Data.Type.Util
import           Data.Type.Vector hiding      (itraverse)
import           Data.Typeable
import           GHC.Exts                     (Any)
import           GHC.Generics                 as G
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric.Backprop.Class
import           Numeric.Backprop.Op
import           System.IO.Unsafe
import           Type.Class.Higher
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
-- '^^.' ('Numeric.Backprop.viewVar'):
--
-- @
-- ('^.')  ::        a -> 'Lens'' a b ->        b
-- ('^^.') :: 'BVar' s a -> 'Lens'' a b -> 'BVar' s b
-- @
--
-- There is also '^^?' ('Numeric.Backprop.previewVar'), to use a 'Prism''
-- or 'Traversal'' to extract a target that may or may not be present
-- (which can implement pattern matching), '^^..'
-- ('Numeric.Backprop.toListOfVar') to use a 'Traversal'' to extract /all/
-- targets inside a 'BVar', and '.~~' ('setVar') to set and update values
-- inside a 'BVar'.
--
-- If you have control over your data type definitions, you can also use
-- 'Numeric.Backprop.splitBV' and 'Numeric.Backprop.joinBV' to manipulate
-- data types by easily extracting fields out of a 'BVar' of data types and
-- creating 'BVar's of data types out of 'BVar's of their fields.  See
-- "Numeric.Backprop#hkd" for a tutorial on this use pattern.
--
-- For more complex operations, libraries can provide functions on 'BVar's
-- using 'Numeric.Backprop.liftOp' and related functions.  This is how you
-- can create primitive functions that users can use to manipulate your
-- library's values.  See
-- <https://backprop.jle.im/08-equipping-your-library.html> for a detailed
-- guide.
--
-- For example, the /hmatrix/ library has a matrix-vector multiplication
-- function, @#> :: L m n -> R n -> L m@.
--
-- A library could instead provide a function @#> :: 'BVar' (L m n) -> BVar
-- (R n) -> BVar (R m)@, which the user can then use to manipulate their
-- 'BVar's of @L m n@s and @R n@s, etc.
--
-- See "Numeric.Backprop#liftops" and documentation for
-- 'Numeric.Backprop.liftOp' for more information.
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
    IR :: { _irIx    :: !(BVar s b)
          , _irAdd   :: !(a -> b -> b)
          , _irEmbed :: !(a -> b)
          }
       -> InpRef a

forceInpRef :: InpRef a -> ()
forceInpRef (IR v !_ !_) = forceBVar v `seq` ()
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
    STN :: { _stnNode :: !(TapeNode a)
           }
        -> SomeTapeNode

-- | Debugging string for a 'SomeTapeMode'.
debugSTN :: SomeTapeNode -> String
debugSTN (STN TN{..}) = show . foldMap1 ((:[]) . debugIR) $ _tnInputs

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
    -> W
    -> IO (BVar s a)
insertNode tn !x !w = fmap ((`BV` x) . BRIx) . atomicModifyIORef' (wRef w) $ \(!n,!t) ->
    let n' = n + 1
        t' = STN tn : t
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
    -> Op as b
    -> Prod (BVar s) as
    -> IO (BVar s b)
liftOp_ afs o !vs = case traverse1 (fmap I . bvConst) vs of
    Just xs -> return $ constVar (evalOp o xs)
    Nothing -> insertNode tn y (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (map1 (I . _bvVal) vs)
    tn = TN { _tnInputs = map1 go (zipP afs vs)
            , _tnGrad   = g
            }
    go :: forall a. (AddFunc :&: BVar s) a -> InpRef a
    go (af :&: (!v)) = forceBVar v `seq` IR v (runAF af) id
    {-# INLINE go #-}
{-# INLINE liftOp_ #-}

-- | 'Numeric.Backprop.liftOp', but with explicit 'add' and 'zero'.
liftOp
    :: forall as b s. Reifies s W
    => Prod AddFunc as
    -> Op as b
    -> Prod (BVar s) as
    -> BVar s b
liftOp afs o !vs = unsafePerformIO $ liftOp_ afs o vs
{-# INLINE liftOp #-}

liftOp1_
    :: forall a b s. Reifies s W
    => AddFunc a
    -> Op '[a] b
    -> BVar s a
    -> IO (BVar s b)
liftOp1_ _  o (bvConst->Just x) = return . constVar . evalOp o $ (x ::< Ø)
liftOp1_ af o v = forceBVar v `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (_bvVal v ::< Ø)
    tn = TN { _tnInputs = IR v (runAF af) id :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp1_ #-}

-- | 'Numeric.Backprop.liftOp1', but with explicit 'add' and 'zero'.
liftOp1
    :: forall a b s. Reifies s W
    => AddFunc a
    -> Op '[a] b
    -> BVar s a
    -> BVar s b
liftOp1 af o !v = unsafePerformIO $ liftOp1_ af o v
{-# INLINE liftOp1 #-}

liftOp2_
    :: forall a b c s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> IO (BVar s c)
liftOp2_ _ _ o (bvConst->Just x) (bvConst->Just y)
    = return . constVar . evalOp o $ x ::< y ::< Ø
liftOp2_ afa afb o v u = forceBVar v
                   `seq` forceBVar u
                   `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y,g) = runOpWith o (_bvVal v ::< _bvVal u ::< Ø)
    tn = TN { _tnInputs = IR v (runAF afa) id :< IR u (runAF afb) id :< Ø
            , _tnGrad   = g
            }
{-# INLINE liftOp2_ #-}

-- | 'Numeric.Backprop.liftOp2', but with explicit 'add' and 'zero'.
liftOp2
    :: forall a b c s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> Op '[a,b] c
    -> BVar s a
    -> BVar s b
    -> BVar s c
liftOp2 afa afb o !v !u = unsafePerformIO $ liftOp2_ afa afb o v u
{-# INLINE liftOp2 #-}

liftOp3_
    :: forall a b c d s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> AddFunc c
    -> Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> IO (BVar s d)
liftOp3_ _ _ _ o (bvConst->Just x) (bvConst->Just y) (bvConst->Just z)
    = return . constVar . evalOp o $ x ::< y ::< z ::< Ø
liftOp3_ afa afb afc o v u w = forceBVar v
                         `seq` forceBVar u
                         `seq` forceBVar w
                         `seq` insertNode tn y (reflect (Proxy @s))
  where
    (y, g) = runOpWith o (_bvVal v ::< _bvVal u ::< _bvVal w ::< Ø)
    tn = TN { _tnInputs = IR v (runAF afa) id
                       :< IR u (runAF afb) id
                       :< IR w (runAF afc) id
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
    -> Op '[a,b,c] d
    -> BVar s a
    -> BVar s b
    -> BVar s c
    -> BVar s d
liftOp3 afa afb afc o !v !u !w = unsafePerformIO $ liftOp3_ afa afb afc o v u w
{-# INLINE liftOp3 #-}

-- TODO: can we get the zero and add func from the bvar?
viewVar_
    :: forall a b s. Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> Lens' b a
    -> BVar s b
    -> IO (BVar s a)
viewVar_ af z l v = forceBVar v `seq` insertNode tn y (reflect (Proxy @s))
  where
    x = _bvVal v
    y = x ^. l
    tn = TN { _tnInputs = IR v (over l . runAF af) (\g -> set l g (runZF z x))
                       :< Ø
            , _tnGrad   = only_
            }
{-# INLINE viewVar_ #-}

-- | 'Numeric.Backprop.viewVar', but with explicit 'add' and 'zero'.
viewVar
    :: forall a b s. Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> Lens' b a
    -> BVar s b
    -> BVar s a
viewVar af z l !v = unsafePerformIO $ viewVar_ af z l v
{-# INLINE viewVar #-}

-- TODO: can zero and add func be gotten from the input bvars?
setVar_
    :: forall a b s. Reifies s W
    => AddFunc a
    -> AddFunc b
    -> ZeroFunc a
    -> Lens' b a
    -> BVar s a
    -> BVar s b
    -> IO (BVar s b)
setVar_ afa afb za l w v = forceBVar v
                     `seq` forceBVar w
                     `seq` insertNode tn y (reflect (Proxy @s))
  where
    y = _bvVal v & l .~ _bvVal w
    tn = TN { _tnInputs = IR w (runAF afa) id
                       :< IR v (runAF afb) id
                       :< Ø
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
    -> Lens' b a
    -> BVar s a
    -> BVar s b
    -> BVar s b
setVar afa afb za l !w !v = unsafePerformIO $ setVar_ afa afb za l w v
{-# INLINE setVar #-}

-- | 'Numeric.Backprop.sequenceVar', but with explicit 'add' and 'zero'.
sequenceVar
    :: forall t a s. (Reifies s W, Traversable t)
    => AddFunc a
    -> ZeroFunc (t a)
    -> BVar s (t a)
    -> t (BVar s a)
sequenceVar af z !v = unsafePerformIO $
    traverseVar' af z id traverse v
{-# INLINE sequenceVar #-}

-- TODO: can add funcs and zeros be had from bvars and Functor instance?
collectVar_
    :: forall t a s. (Reifies s W, Foldable t, Functor t)
    => AddFunc a
    -> ZeroFunc a
    -> t (BVar s a)
    -> IO (BVar s (t a))
collectVar_ af z !vs = withV (toList vs) $ \(vVec :: Vec n (BVar s a)) -> do
    let tn :: TapeNode (t a)
        tn = TN
          { _tnInputs = vecToProd (vmap ((\v -> IR v (runAF af) id) . getI) vVec)
          , _tnGrad   = vecToProd
                      . zipVecList (\(I v) -> I . fromMaybe (runZF z (_bvVal v))) vVec
                      . toList
          }
    traverse_ (evaluate . forceBVar) vs
    insertNode tn (_bvVal <$> vs) (reflect (Proxy @s))
{-# INLINE collectVar_ #-}

-- | 'Numeric.Backprop.collectVar', but with explicit 'add' and 'zero'.
--
-- NOTE: Prior to v0.2.3, this required an extra @'ZeroFunc' (t a)@ input.
-- However, after v0.2.3, the 'ZeroFunc' is now derived from the 'Functor'
-- instance of @t@.  This makes the API a little more convenient, and it
-- enforces consistency with the @'ZeroFunc' a@, so people can't pass in
-- nonsense combinations.
--
-- Please submit an issue to the issue tracker if you find yourself in
-- a situation where you need the flexibility to provide a separte
-- @'ZeroFunc' a@ and @'ZeroFunc' (t a)@.
collectVar
    :: forall t a s. (Reifies s W, Foldable t, Functor t)
    => AddFunc a
    -> ZeroFunc a
    -> t (BVar s a)
    -> BVar s (t a)
collectVar af z !vs = unsafePerformIO $ collectVar_ af z vs
{-# INLINE collectVar #-}

traverseVar'
    :: forall b a f s. (Reifies s W, Traversable f)
    => AddFunc a
    -> ZeroFunc b
    -> (b -> f a)
    -> Traversal' b a
    -> BVar s b
    -> IO (f (BVar s a))
traverseVar' af z f t v = forceBVar v
                    `seq` itraverse go (f x)
  where
    x = _bvVal v
    go :: Int -> a -> IO (BVar s a)
    go i y = insertNode tn y (reflect (Proxy @s))
      where
        tn = TN { _tnInputs = IR v (over (ixt t i) . runAF af)
                                   (\g -> set (ixt t i) g (runZF z x))
                           :< Ø
                , _tnGrad   = only_
                }
    {-# INLINE go #-}
{-# INLINE traverseVar' #-}

-- | 'Numeric.Backprop.previewVar', but with explicit 'add' and 'zero'.
previewVar
    :: forall b a s. Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> Traversal' b a
    -> BVar s b
    -> Maybe (BVar s a)
previewVar af z t !v = unsafePerformIO $
    traverseVar' af z (preview t) t v
{-# INLINE previewVar #-}

-- | 'Numeric.Backprop.toListOfVar', but with explicit 'add' and 'zero'.
toListOfVar
    :: forall b a s. Reifies s W
    => AddFunc a
    -> ZeroFunc b
    -> Traversal' b a
    -> BVar s b
    -> [BVar s a]
toListOfVar af z t !v = unsafePerformIO $
    traverseVar' af z (toListOf t) t v
{-# INLINE toListOfVar #-}

-- | Coerce a 'BVar' contents.  Useful for things like newtype wrappers.
--
-- @since 0.1.5.2
coerceVar
    :: Coercible a b
    => BVar s a
    -> BVar s b
coerceVar v@(BV r x) = forceBVar v `seq` BV r (coerce x)

data Runner s = R { _rDelta  :: !(MV.MVector s (Maybe Any))
                  , _rInputs :: !(MV.MVector s (Maybe Any))
                  }

initRunner
    :: (Int, [SomeTapeNode])
    -> (Int, [Maybe Any])
    -> ST s (Runner s)
initRunner (n, stns) (nx,xs) = do
    delts <- MV.new n
    for_ (zip [n-1,n-2..] stns) $ \(i, STN (TN{..} :: TapeNode c)) ->
      MV.write delts i $ unsafeCoerce (Nothing @c)
    inps <- MV.new nx
    for_ (zip [0..] xs) . uncurry $ \i z ->
      MV.write inps i z
    return $ R delts inps
{-# INLINE initRunner #-}

gradRunner
    :: forall b s. ()
    => b                        -- ^ one
    -> Runner s
    -> (Int, [SomeTapeNode])
    -> ST s ()
gradRunner o R{..} (n,stns) = do
    when (n > 0) $
      MV.write _rDelta (n - 1) (unsafeCoerce (Just o))
    zipWithM_ go [n-1,n-2..] stns
  where
    go :: Int -> SomeTapeNode -> ST s ()
    go i (STN (TN{..} :: TapeNode c)) = do
      delt <- MV.read _rDelta i
      forM_ delt $ \d -> do
        let gs = _tnGrad (unsafeCoerce d)
        zipWithPM_ propagate _tnInputs gs
    {-# INLINE go #-}
    propagate :: forall x. InpRef x -> I x -> ST s ()
    propagate (IR v (+*) e) (I d) = case _bvRef v of
      BRInp i -> flip (MV.modify _rInputs) i $
        unsafeCoerce . bumpMaybe d (+*) e . unsafeCoerce
      BRIx i -> flip (MV.modify _rDelta) i $
        unsafeCoerce . bumpMaybe d (+*) e . unsafeCoerce
      BRC     -> return ()
    {-# INLINE propagate #-}
{-# INLINE gradRunner #-}

bumpMaybe
    :: a                -- ^ val
    -> (a -> b -> b)    -- ^ add
    -> (a -> b)         -- ^ embed
    -> Maybe b
    -> Maybe b
bumpMaybe x (+*) e = \case
    Nothing -> Just (e x)
    Just y  -> Just (x +* y)
{-# INLINE bumpMaybe #-}

-- | 'Numeric.Backprop.backpropWithN', but with explicit 'zero' and 'one'.
--
-- Note that argument order changed in v0.2.4.
--
-- @since 0.2.0.0
backpropWithN
    :: forall as b. ()
    => Prod ZeroFunc as
    -> (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, b -> Tuple as)
backpropWithN zfs f !xs = (y, g)
  where
    !(!tp@(!_,!_),!y) = unsafePerformIO $ fillWengert f xs
    g :: b -> Tuple as
    g o = runST $ do
        r <- initRunner tp $ bimap getSum (`appEndo` [])
                           . fst
                           $ traverse1_ go xs
                           -- zipWithPM_ go zfs xs
        gradRunner o r tp
        delts <- toList <$> V.freeze (_rInputs r)
        return . fromMaybe (internalError "backpropN") $
          fillProd (\(zf :&: I x) d -> I $ maybe (runZF zf x) unsafeCoerce d
                   )
            (zipP zfs xs)
            delts
      where
        go :: forall a. I a -> ((Sum Int, Endo [Maybe Any]),())
        go _ = ((1, Endo (unsafeCoerce (Nothing @a) :)), ())
        {-# INLINE go #-}
{-# INLINE backpropWithN #-}

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
    (+)         = liftOp2 afNum afNum (+.)
    {-# INLINE (+) #-}
    (-)         = liftOp2 afNum afNum (-.)
    {-# INLINE (-) #-}
    (*)         = liftOp2 afNum afNum (*.)
    {-# INLINE (*) #-}
    negate      = liftOp1 afNum negateOp
    {-# INLINE negate #-}
    signum      = liftOp1 afNum signumOp
    {-# INLINE signum #-}
    abs         = liftOp1 afNum absOp
    {-# INLINE abs #-}
    fromInteger = constVar . fromInteger
    {-# INLINE fromInteger #-}

instance (Fractional a, Reifies s W) => Fractional (BVar s a) where
    (/)          = liftOp2 afNum afNum (/.)
    {-# INLINE (/) #-}
    recip        = liftOp1 afNum recipOp
    {-# INLINE recip #-}
    fromRational = constVar . fromRational
    {-# INLINE fromRational #-}

instance (Floating a, Reifies s W) => Floating (BVar s a) where
    pi      = constVar pi
    {-# INLINE pi #-}
    exp     = liftOp1 afNum expOp
    {-# INLINE exp #-}
    log     = liftOp1 afNum logOp
    {-# INLINE log #-}
    sqrt    = liftOp1 afNum sqrtOp
    {-# INLINE sqrt #-}
    (**)    = liftOp2 afNum afNum (**.)
    {-# INLINE (**) #-}
    logBase = liftOp2 afNum afNum logBaseOp
    {-# INLINE logBase #-}
    sin     = liftOp1 afNum sinOp
    {-# INLINE sin #-}
    cos     = liftOp1 afNum cosOp
    {-# INLINE cos #-}
    tan     = liftOp1 afNum tanOp
    {-# INLINE tan  #-}
    asin    = liftOp1 afNum asinOp
    {-# INLINE asin #-}
    acos    = liftOp1 afNum acosOp
    {-# INLINE acos #-}
    atan    = liftOp1 afNum atanOp
    {-# INLINE atan #-}
    sinh    = liftOp1 afNum sinhOp
    {-# INLINE sinh #-}
    cosh    = liftOp1 afNum coshOp
    {-# INLINE cosh #-}
    tanh    = liftOp1 afNum tanhOp
    {-# INLINE tanh #-}
    asinh   = liftOp1 afNum asinhOp
    {-# INLINE asinh #-}
    acosh   = liftOp1 afNum acoshOp
    {-# INLINE acosh #-}
    atanh   = liftOp1 afNum atanhOp
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
ixi _ _ []     = internalError "ixi"
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
        go []     = internalError "ixt"
        go (y:ys) = (y, ys)
{-# INLINE ixt #-}

-- | @since 0.2.2.0
instance (Backprop a, Reifies s W) => Backprop (BVar s a) where
    zero = liftOp1 addFunc . op1 $ \x -> (zero x, zero)
    {-# INLINE zero #-}
    add  = liftOp2 addFunc addFunc . op2 $ \x y ->
        ( add x y
        , \d -> (d, d)
        )
    {-# INLINE add #-}
    one  = liftOp1 addFunc . op1 $ \x -> (one  x, zero)
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

internalError :: String -> a
internalError m = errorWithoutStackTrace $
    "Numeric.Backprop.Internal." ++ m ++ ": unexpected shape involved in gradient computation"
