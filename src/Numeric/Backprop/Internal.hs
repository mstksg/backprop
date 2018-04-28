{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
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
  , AddFunc(..), afNum
  , ZeroFunc(..), zfNum
  , OneFunc(..), ofNum
  , backpropN, evalBPN
  , constVar
  , liftOp, liftOp1, liftOp2, liftOp3
  , viewVar, setVar, sequenceVar, collectVar, previewVar, toListOfVar
  , coerceVar
  -- * Debug
  , debugSTN
  , debugIR
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
import           Data.IORef
import           Data.Kind
import           Data.Maybe
import           Data.Monoid hiding        (Any(..))
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Conjunction
import           Data.Type.Product hiding  (toList)
import           Data.Type.Util
import           Data.Type.Vector hiding   (itraverse)
import           Data.Typeable
import           GHC.Exts                  (Any)
import           GHC.Generics
import           Lens.Micro
import           Numeric.Backprop.Op
import           System.IO.Unsafe
import           Type.Class.Higher
import           Unsafe.Coerce
import qualified Data.Vector               as V
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
    IR :: { _irIx    :: !(BVar s b)
          , _irScale :: !(a -> b -> b)
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

-- | "Zero out" all components of a value.  For scalar values, this should
-- just be @'const' 0@.  For vectors and matrices, this should set all
-- components to zero, the additive identity.
--
-- Each type should ideally only have one 'ZeroFunc'.  This coherence
-- constraint is given by the typeclass 'Zero'.
newtype ZeroFunc a = ZF { runZF :: a -> a }

-- | Add together two values of a type.  To combine contributions of
-- gradients, so should ideally be information-preserving.  For any other
-- valid 'ZeroFunc', should ideally obey:
--
-- @
-- \af zf x y -> 'runAF' af x ('runZF' zf y) == x
--            && 'runAF' af ('runZF' zf x) y == y
-- @
--
-- Each type should ideally only have one 'AddFunc'.  This coherence
-- constraint is given by the typeclass 'Add'.
newtype AddFunc  a = AF { runAF :: a -> a -> a }

-- | "One" all components of a value.  For scalar values, this should
-- just be @'const' 1@.  For vectors and matrices, this should set all
-- components to one, the multiplicative identity.
--
-- Each type should ideally only have one 'ZeroFunc'.  This coherence
-- constraint is given by the typeclass 'One'.
newtype OneFunc  a = OF { runOF :: a -> a }

-- | If a type has a 'Num' instance, this is the canonical 'AddFunc'.
afNum :: Num a => AddFunc a
afNum = AF (+)
{-# INLINE afNum #-}

-- | If a type has a 'Num' instance, this is the canonical 'ZeroFunc'.
zfNum :: Num a => ZeroFunc a
zfNum = ZF (const 0)
{-# INLINE zfNum #-}

-- | If a type has a 'Num' instance, this is the canonical 'OneFunc'.
ofNum :: Num a => OneFunc a
ofNum = OF (const 1)
{-# INLINE ofNum #-}


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

-- | Lift an 'Op' with a single input to be a function on a single 'BVar'.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
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

-- | Lift an 'Op' with two inputs to be a function on a two 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
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

-- | Lift an 'Op' with three inputs to be a function on a three 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
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

-- TODO: can we get the zero and scale func from the lens?
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

-- | Using a 'Lens'', extract a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to 'view' from lens.
--
-- See documentation for '^^.' for more information.
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

-- | Using a 'Lens'', set a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to "set" from lens.
--
-- See documentation for '.~~' for more information.
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

-- | Extract all of the 'BVar's out of a 'Traversable' container of
-- 'BVar's.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the gradient is assumed to
-- correspond with the second item in the input, etc.; this can cause
-- unexpected behavior in 'Foldable' instances that don't have a fixed
-- number of items.
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

-- | Collect all of the 'BVar's in a container into a 'BVar' of that
-- container's contents.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the total derivative and
-- gradient is assumed to correspond with the second item in the input,
-- etc.; this can cause unexpected behavior in 'Foldable' instances that
-- don't have a fixed number of items.
--
-- Note that this requires @t a@ to have a 'Num' instance.  If you are
-- using a list, I recommend using
-- <https://hackage.haskell.org/package/vector-sized vector-sized> instead:
-- it's a fixed-length vector type with a very appropriate 'Num' instance!
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

-- | Using a 'Traversal'', extract a single value /inside/ a 'BVar', if it
-- exists.  If more than one traversal target exists, returns te first.
-- Meant to evoke parallels to 'preview' from lens.  Really only intended
-- to be used wth 'Prism''s, or up-to-one target traversals.
--
-- See documentation for '^^?' for more information.
previewVar
    :: forall b a s. Reifies s W
    => AddFunc a
    -> ZeroFunc a
    -> Traversal' b a
    -> BVar s b
    -> Maybe (BVar s a)
previewVar af z t !v = unsafePerformIO $ traverseVar' af z (listToMaybe . toListOf t) t v
{-# INLINE previewVar #-}

-- | Using a 'Traversal'', extract all targeted values /inside/ a 'BVar'.
-- Meant to evoke parallels to 'toListOf' from lens.
--
-- See documentation for '^^..' for more information.
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
    for_ (zip [0..] xs) $ \(i, z) ->
      MV.write inps i z
    return $ R delts inps
{-# INLINE initRunner #-}

gradRunner
    :: forall m b s. (PrimMonad m, PrimState m ~ s)
    => b                        -- ^ one
    -> Runner s
    -> (Int, [SomeTapeNode])
    -> m ()
gradRunner one R{..} (n,stns) = do
    when (n > 0) $
      MV.write _rDelta (n - 1) (unsafeCoerce one)
    zipWithM_ go [n-1,n-2..] stns
  where
    go :: Int -> SomeTapeNode -> m ()
    go i (STN _ TN{..}) = do
      delt <- MV.read _rDelta i
      let gs = _tnGrad (unsafeCoerce delt)
      zipWithPM_ propagate _tnInputs gs
    {-# INLINE go #-}
    propagate :: forall x. InpRef x -> I x -> m ()
    propagate = undefined
    -- propagate (IR v ln) (I d) = case _bvRef v of
    --   BRInp i -> flip (MV.modify _rInputs) i $
    --     unsafeCoerce . (ln %~ (+* d)) . unsafeCoerce
    --   BRIx i -> flip (MV.modify _rDelta) i $
    --     unsafeCoerce . (ln %~ (+* d)) . unsafeCoerce
    --   BRC     -> return ()
    {-# INLINE propagate #-}
{-# INLINE gradRunner #-}

-- | 'backprop' generalized to multiple inputs of different types.  See the
-- "Numeric.Backprop.Op#prod" for a mini-tutorial on heterogeneous lists.
--
-- Not strictly necessary, because you can always uncurry a function by
-- passing in all of the inputs in a data type containing all of the
-- arguments or a tuple from "Numeric.Backprop.Tuple".   You could also
-- pass in a giant tuple with
-- <https://hackage.haskell.org/package/NumInstances NumInstances>.
-- However, this can be convenient if you don't want to make a custom
-- larger tuple type or pull in orphan instances.  This could potentially
-- also be more performant.
--
-- A @'Prod' ('BVar' s) '[Double, Float, Double]@, for instance, is a tuple
-- of @'BVar' s 'Double'@, @'BVar' s 'Float'@, and @'BVar' s 'Double'@, and
-- can be pattern matched on using ':<' (cons) and 'Ø' (nil).
--
-- Tuples can be built and pattern matched on using '::<' (cons) and 'Ø'
-- (nil), as well.
--
-- The @'Every' 'Num' as@ in the constraint says that every value in the
-- type-level list @as@ must have a 'Num' instance.  This means you can
-- use, say, @'[Double, Float, Int]@, but not @'[Double, Bool, String]@.
--
-- If you stick to /concerete/, monomorphic usage of this (with specific
-- types, typed into source code, known at compile-time), then @'Every'
-- 'Num' as@ should be fulfilled automatically.
--
backpropN
    :: forall as b. ()
    => Prod AddFunc as
    -> OneFunc b
    -> (forall s. Reifies s W => Prod (BVar s) as -> BVar s b)
    -> Tuple as
    -> (b, Tuple as)
backpropN afs ofb f !xs = (y, g)
  where
    !(!tp@(!_,!_),!y) = unsafePerformIO $ fillWengert f xs
    g :: Tuple as
    g = runST $ do
        r <- initRunner tp $ bimap getSum (`appEndo` [])
                           . fst
                           $ zipWithPM_ go afs xs
        gradRunner (runOF ofb y) r tp
        delts <- toList <$> V.freeze (_rInputs r)
        return . fromMaybe (error "backpropN") $
          fillProd (\_ d -> I (unsafeCoerce d)) xs delts
      where
        go :: forall a. AddFunc a -> I a -> ((Sum Int, Endo [Any]),())
        go af (I x) = ((1, Endo (unsafeCoerce (runAF af x) :)), ())
        {-# INLINE go #-}
{-# INLINE backpropN #-}

-- | 'evalBP' generalized to multiple inputs of different types.  See
-- documentation for 'backpropN' for more details.
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
        go []     = error "asList"
        go (y:ys) = (y, ys)
{-# INLINE ixt #-}

instance Num a => Num (ZeroFunc a) where
    z1 + z2     = ZF $ (+) <$> runZF z1 <*> runZF z2
    {-# INLINE (+) #-}
    z1 - z2     = ZF $ (-) <$> runZF z1 <*> runZF z2
    {-# INLINE (-) #-}
    z1 * z2     = ZF $ (*) <$> runZF z1 <*> runZF z2
    {-# INLINE (*) #-}
    negate      = ZF . fmap negate . runZF
    {-# INLINE negate #-}
    abs         = ZF . fmap abs . runZF
    {-# INLINE abs #-}
    signum      = ZF . fmap signum . runZF
    {-# INLINE signum #-}
    fromInteger = ZF . const . fromInteger
    {-# INLINE fromInteger #-}

instance Fractional a => Fractional (ZeroFunc a) where
    z1 / z2      = ZF $ (/) <$> runZF z1 <*> runZF z2
    {-# INLINE (/) #-}
    recip        = ZF . fmap recip . runZF
    {-# INLINE recip #-}
    fromRational = ZF . const . fromRational
    {-# INLINE fromRational #-}

instance Floating a => Floating (ZeroFunc a) where
    pi      = ZF $ const pi
    {-# INLINE pi #-}
    exp     = ZF . fmap exp . runZF
    {-# INLINE exp #-}
    log     = ZF . fmap log . runZF
    {-# INLINE log #-}
    sqrt    = ZF . fmap sqrt . runZF
    {-# INLINE sqrt #-}
    z1 ** z2 = ZF $ (**) <$> runZF z1 <*> runZF z2
    {-# INLINE (**) #-}
    logBase z1 z2 = ZF $ logBase <$> runZF z1 <*> runZF z2
    {-# INLINE logBase #-}
    sin     = ZF . fmap sin . runZF
    {-# INLINE sin #-}
    cos     = ZF . fmap cos . runZF
    {-# INLINE cos #-}
    tan     =  ZF . fmap tan . runZF
    {-# INLINE tan  #-}
    asin    = ZF . fmap asin . runZF
    {-# INLINE asin #-}
    acos    = ZF . fmap acos . runZF
    {-# INLINE acos #-}
    atan    = ZF . fmap atan . runZF
    {-# INLINE atan #-}
    sinh    = ZF . fmap sinh . runZF
    {-# INLINE sinh #-}
    cosh    = ZF . fmap cosh . runZF
    {-# INLINE cosh #-}
    tanh    = ZF . fmap tanh . runZF
    {-# INLINE tanh #-}
    asinh   = ZF . fmap asinh . runZF
    {-# INLINE asinh #-}
    acosh   = ZF . fmap acosh . runZF
    {-# INLINE acosh #-}
    atanh   = ZF . fmap atanh . runZF
    {-# INLINE atanh #-}

