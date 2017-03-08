{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
-- building/backpropagation for the library.

module Numeric.Backprop.Internal
  ( Summer(..), summers, summers'
  , Unity(..), unities, unities'
  , OpB
  , BPState(..), bpsSources
  , BP(..)
  , BPInpRef(..)
  , BPNode(..), bpnOut, bpnRes, bpnGradFunc, bpnGradCache, bpnSummer
  , BPPipe(..), bppOut, bppRes, bppGradFunc, bppGradCache
  , BVar(..)
  , ForwardRefs(..), _FRInternal
  ) where

import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Kind
import           Data.STRef
import           Data.Type.Index
import           Data.Type.Product
import           Lens.Micro hiding                (ix)
import           Lens.Micro.TH
import           Numeric.Backprop.Internal.Helper
import           Numeric.Backprop.Op

-- | A subclass of 'OpM' (and superclass of 'Op'), representing 'Op's that
-- the /backprop/ library uses to perform backpropation.
--
-- An
--
-- @
-- 'OpB' s rs a
-- @
--
-- represents a differentiable function that takes a tuple of @rs@ and
-- produces an a @a@, which can be run on @'BVar' s@s and also inside @'BP'
-- s@s.  For example, an @'OpB' s '[ Int, Double ] Bool@ takes an 'Int' and
-- a 'Double' and produces a 'Bool', and does it in a differentiable way.
--
-- 'OpB' is a /superset/ of 'Op', so, if you see any function
-- that expects an 'OpB' (like 'Numeric.Backprop.opVar'' and
-- 'Numeric.Backprop.~$', for example), you can give them an 'Op', as well.
--
-- You can think of 'OpB' as a superclass/parent class of 'Op' in this
-- sense, and of 'Op' as a subclass of 'OpB'.
type OpB s as a = OpM (ST s) as a

-- | Reference to /usage sites/ for a given entity, used to get partial or
-- total derivatives.
data ForwardRefs s rs a
    -- | A list of 'BPInpRef's pointing to places that use the entity, to
    -- provide partial derivatives.
    = FRInternal ![BPInpRef s rs a]
    -- | The entity is the terminal result of a BP, so its total derivative
    -- is fixed.
    | FRTerminal !(Maybe a)

-- | Combines two 'FRInternal' lists.  If either input is an 'FRTerminal',
-- then throws away the other result and keeps the new terminal forced
-- total derivative.  (Biases to the left)
instance Monoid (ForwardRefs s rs a) where
    mempty  = FRInternal []
    mappend = \case
        FRInternal rs -> \case
          FRInternal rs'   -> FRInternal (rs ++ rs')
          t@(FRTerminal _) -> t
        FRTerminal _  -> id

-- | The "state" of a 'BP' action, which keeps track of what nodes, if any,
-- refer to any of the inputs.
data BPState :: Type -> [Type] -> Type where
    BPS :: { _bpsSources :: !(Prod (ForwardRefs s rs) rs)
           }
        -> BPState s rs

-- | A Monad allowing you to explicitly build hetereogeneous data
-- dependency graphs and that the library can perform backpropagation on.
--
-- A @'BP' s rs a@ is a 'BP' action that uses an environment of @rs@
-- returning a @a@.  When "run", it will compute a gradient that is a tuple
-- of @rs@.  (The phantom parameter @s@ is used to ensure that any 'BVar's
-- aren't leaked out of the monad)
--
-- Note that you can only "run" a @'BP' s rs@ that produces a 'BVar' --
-- that is, things of the form
--
-- @
-- 'BP' s rs ('BVar' s rs a)
-- @
--
-- The above is a 'BP' action that returns a 'BVar' containing an @a@.
-- When this is run, it'll produce a result of type @a@ and a gradient of
-- that is a tuple of @rs@.  (This form has a type synonym,
-- 'Numeric.Backprop.BPOp', for convenience)
--
-- For example, a @'BP' s '[ Int, Double, Double ]@ is a monad that
-- represents a computation with an 'Int', 'Double', and 'Double' as
-- inputs.   And, if you ran a
--
-- @
-- 'BP' s '[ Int, Double, Double ] ('BVar' s '[ Int, Double, Double ] Double)
-- @
--
-- Or, using the 'BPOp' type synonym:
--
-- @
-- 'Numeric.Backprop.BPOp' s '[ Int, Double, Double ] Double
-- @
--
-- with 'Numeric.Backprop.backprop' or 'Numeric.Backprop.gradBPOp', it'll
-- return a gradient on the inputs ('Int', 'Double', and 'Double') and
-- produce a value of type 'Double'.
newtype BP s rs a = BP { bpST :: ReaderT (Tuple rs) (StateT (BPState s rs) (ST s)) a }
      deriving ( Functor
               , Applicative
               , Monad
               )

-- | The basic unit of manipulation inside 'BP' (or inside an
-- implicit-graph backprop function).  Instead of directly working with
-- values, you work with 'BVar's contating those values.  When you work
-- with a 'BVar', the /backprop/ library can keep track of what values
-- refer to which other values, and so can perform backpropagation to
-- compute gradients.
--
-- A @'BVar' s rs a@ refers to a value of type @a@, with an environment
-- of values of the types @rs@.  The phantom parameter @s@ is used to
-- ensure that stray 'BVar's don't leak outside of the backprop process.
--
-- (That is, if you're using implicit backprop, it ensures that you interact
-- with 'BVar's in a polymorphic way.  And, if you're using explicit
-- backprop, it ensures that a @'BVar' s rs a@ never leaves the @'BP' s rs@
-- that it was created in.)
--
-- 'BVar's have 'Num', 'Fractional', 'Floating', etc. instances, so they
-- can be manipulated using polymorphic functions and numeric functions in
-- Haskell.  You can add them, subtract them, etc., in "implicit" backprop
-- style.
--
-- (However, note that if you directly manipulate 'BVar's using those
-- instances or using 'Numeric.Backprop.liftB', it delays evaluation, so every usage site
-- has to re-compute the result/create a new node.  If you want to re-use
-- a 'BVar' you created using '+' or '-' or 'Numeric.Backprop.liftB', use
-- 'Numeric.Backprop.bindVar' to force it first.  See documentation for
-- 'Numeric.Backprop.bindVar' for more details.)
data BVar :: Type -> [Type] -> Type -> Type where
    -- | A BVar referring to a 'BPNode'
    BVNode  :: !(Index bs a)
            -> !(STRef s (BPNode s rs as bs))
            -> BVar s rs a
    -- | A BVar referring to an environment input variable
    BVInp   :: !(Index rs a)
            -> BVar s rs a
    -- | A constant BVar that refers to a specific Haskell value
    BVConst :: !a
            -> BVar s rs a
    -- | A BVar that combines several other BVars using a function (an
    -- 'Op').  Essentially a branch of a tree.
    BVOp    :: !(Prod (BVar s rs) as)
            -> !(OpB s as a)
            -> BVar s rs a

-- | Used exclusively by 'ForwardRefs' to specify "where" and "how" to look
-- for partial derivatives at usage sites of a given entity.
data BPInpRef :: Type -> [Type] -> Type -> Type where
    -- | The entity is used in a 'BPNode', and as an Nth input
    IRNode  :: !(Index bs a)
            -> !(STRef s (BPNode s rs bs cs))
            -> BPInpRef s rs a
    -- | The entity is used in a 'BPPipe', and as an Nth input
    IRPipe  :: !(Index bs a)
            -> !(STRef s (BPPipe s rs bs cs))
            -> BPInpRef s rs a
    -- | The entity is used somehow in the terminal result of a 'BP', and
    -- so therefore has a fixed partial derivative contribution.
    IRConst :: !a
            -> BPInpRef s rs a

-- | A (stateful) node in the graph of operations/data dependencies in 'BP'
-- that the library uses.  'BVar's can refer to these to get results from
-- them, and 'BPInpRef's can refer to these to get partial derivatives from
-- them.
data BPNode :: Type -> [Type] -> [Type] -> [Type] -> Type where
    BPN :: { _bpnOut       :: !(Prod (ForwardRefs s rs) bs)
           , _bpnRes       :: !(Tuple bs)
           , _bpnGradFunc  :: !(Prod Maybe bs -> ST s (Tuple as))
           , _bpnGradCache :: !(Maybe (Tuple as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Prod Summer bs)
           }
        -> BPNode s rs as bs

-- | Essentially a "single-usage" 'BPNode'.  It's a stateful node, but only
-- ever has a single consumer (and so its total derivative comes from
-- a single partial derivative).  Used when keeping track of 'BVOp's.
data BPPipe :: Type -> [Type] -> [Type] -> [Type] -> Type where
    BPP :: { _bppOut       :: !(Prod (BPInpRef s rs) bs)
           , _bppRes       :: !(Tuple bs)
           , _bppGradFunc  :: !(Tuple bs -> ST s (Tuple as))
           , _bppGradCache :: !(Maybe (Tuple as))
           }
        -> BPPipe s rs as bs

makeLenses ''BPState
makeLenses ''BPNode
makeLenses ''BPPipe

-- | Traversal (fake prism) to refer to the list of internal refs if the
-- 'ForwardRef' isn't associated with a terminal entity.
_FRInternal
    :: Traversal (ForwardRefs s as a) (ForwardRefs t bs a)
                 [BPInpRef s as a]    [BPInpRef t bs a]
_FRInternal f = \case
    FRInternal xs -> FRInternal <$> f xs
    FRTerminal g  -> pure (FRTerminal g)




-- | Note that if you use the 'Num' instance to create 'BVar's, the
-- resulting 'BVar' is deferred/delayed.  At every location you use it, it
-- will be recomputed, and a separate graph node will be created.  If you
-- are using a 'BVar' you made with the 'Num' instance in multiple
-- locations, use 'Numeric.Backprop.bindVar' first to force it and prevent
-- recomputation.
instance Num a => Num (BVar s rs a) where
    r1 + r2       = BVOp (r1 :< r2 :< Ø) $ op2 (+)
    r1 - r2       = BVOp (r1 :< r2 :< Ø) $ op2 (-)
    r1 * r2       = BVOp (r1 :< r2 :< Ø) $ op2 (*)
    negate r      = BVOp (r :< Ø)        $ op1 negate
    signum r      = BVOp (r :< Ø)        $ op1 negate
    abs r         = BVOp (r :< Ø)        $ op1 abs
    fromInteger x = BVConst (fromInteger x)

-- | See note for 'Num' instance.
instance Fractional a => Fractional (BVar s rs a) where
    r1 / r2        = BVOp (r1 :< r2 :< Ø) $ op2 (/)
    recip r        = BVOp (r :< Ø)        $ op1 recip
    fromRational x = BVConst (fromRational x)

-- | See note for 'Num' instance.
instance Floating a => Floating (BVar s rs a) where
    pi            = BVConst pi
    exp   r       = BVOp (r :< Ø)        $ op1 exp
    log   r       = BVOp (r :< Ø)        $ op1 log
    sqrt  r       = BVOp (r :< Ø)        $ op1 sqrt
    r1 ** r2      = BVOp (r1 :< r2 :< Ø) $ op2 (**)
    logBase r1 r2 = BVOp (r1 :< r2 :< Ø) $ op2 logBase
    sin   r       = BVOp (r :< Ø)        $ op1 sin
    cos   r       = BVOp (r :< Ø)        $ op1 cos
    tan   r       = BVOp (r :< Ø)        $ op1 tan
    asin  r       = BVOp (r :< Ø)        $ op1 asin
    acos  r       = BVOp (r :< Ø)        $ op1 acos
    atan  r       = BVOp (r :< Ø)        $ op1 atan
    sinh  r       = BVOp (r :< Ø)        $ op1 sinh
    cosh  r       = BVOp (r :< Ø)        $ op1 cosh
    tanh  r       = BVOp (r :< Ø)        $ op1 tanh
    asinh r       = BVOp (r :< Ø)        $ op1 asinh
    acosh r       = BVOp (r :< Ø)        $ op1 acosh
    atanh r       = BVOp (r :< Ø)        $ op1 atanh

