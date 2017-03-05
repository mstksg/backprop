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
-- Provides the types used for the graph building/backpropagation for the
-- library.

module Numeric.Backprop.Internal
  ( Summer(..), summers, summers'
  , Unity(..), unities, unities'
  , OpB
  , BPState(..), bpsSources
  , BP(..)
  , BPInpRef(..)
  , BPNode(..), bpnOut, bpnRes, bpnGradFunc, bpnGradCache, bpnSummer
  , BPPipe(..), bppOut, bppRes, bppGradFunc, bppGradCache
  , BRef(..)
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
-- A @'BP' s rs b@ is a 'BP' action that uses an environment of @rs@
-- returning a @b@.  The @s@ parameter is used the same way that 'ST' uses
-- it, basically to enforce that nothing important "leaks" out of the
-- monad.  When "run", it will compute a gradient that is a tuple of @rs@.
--
-- For example, a @'BP' s '[ Int, Double, Double ]@ is a monad that
-- represents a computation with an 'Int', 'Double', and 'Double' as
-- inputs.
--
-- When run with 'backprop' or 'gradBPOp', it would return a /gradient/ on
-- the inputs.  So in the above example, the gradient would be a tuple of
-- an 'Int', 'Double', and 'Double'.
newtype BP s rs b = BP { bpST :: ReaderT (Tuple rs) (StateT (BPState s rs) (ST s)) b }
      deriving ( Functor
               , Applicative
               , Monad
               )

-- TODO: change BRef to BVal

-- | The basic unit of manipulation inside 'BP'.  Instead of directly
-- working with values, you work with /references/ to those values.  When
-- you work with a 'BRef', the /backprop/ library can keep track of what
-- values refer to which other values, and so can perform backpropagation
-- to compute gradients.
--
-- A @'BRef' s rs a@ lives inside a @'BP' s rs@ monad, and refers to
-- a value of type @a@.  (The @rs@ refers to the environment of the 'BP'
-- action that the 'BPRef' lives inside.)
--
-- 'BPRef's have 'Num', 'Fractional', 'Floating', etc. instances, so they
-- can be manipulated using polymorphic functions and numeric functions in
-- Haskell.  You can add them, subtract them, etc., in "implicit" backprop
-- style.
--
-- (However, note that if you directly manipulate 'BPRef's using those
-- instances or using 'liftR', it delays evaluation, so every usage site
-- has to re-compute the result/create a new node.  If you want to re-use
-- a 'BPRef' you created using '(+)' or '(-)' or 'liftR', use 'bindRef' to
-- force it first.)
data BRef :: Type -> [Type] -> Type -> Type where
    -- | A BRef referring to a 'BPNode'
    BRNode  :: !(Index bs a)
            -> !(STRef s (BPNode s rs as bs))
            -> BRef s rs a
    -- | A BRef referring to an environment input variable
    BRInp   :: !(Index rs a)
            -> BRef s rs a
    -- | A constant BRef that refers to a specific Haskell value
    BRConst :: !a
            -> BRef s rs a
    -- | A BRef that combines several other BRefs using a function (an
    -- 'Op').  Essentially a branch of a tree.
    BROp    :: !(Prod (BRef s rs) as)
            -> !(OpB s as a)
            -> BRef s rs a

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
-- that the library uses.  'BRef's can refer to these to get results from
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
-- a single partial derivative).  Used when keeping track of 'BROp's.
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




-- | Note that if you use the 'Num' instance to create 'BRef's, the
-- resulting 'BRef' is deferred/delayed.  At every location you use it, it
-- will be recomputed, and a separate graph node will be created.  If you
-- are using a 'BRef' you made with the 'Num' instance in multiple
-- locations, use 'bindRef' first to force it and prevent recomputation.
instance Num a => Num (BRef s rs a) where
    r1 + r2       = BROp (r1 :< r2 :< Ø) $ op2 (+)
    r1 - r2       = BROp (r1 :< r2 :< Ø) $ op2 (-)
    r1 * r2       = BROp (r1 :< r2 :< Ø) $ op2 (*)
    negate r      = BROp (r :< Ø)        $ op1 negate
    signum r      = BROp (r :< Ø)        $ op1 negate
    abs r         = BROp (r :< Ø)        $ op1 abs
    fromInteger x = BRConst (fromInteger x)

-- | See note for 'Num' instance.
instance Fractional a => Fractional (BRef s rs a) where
    r1 / r2        = BROp (r1 :< r2 :< Ø) $ op2 (/)
    recip r        = BROp (r :< Ø)        $ op1 recip
    fromRational x = BRConst (fromRational x)

-- | See note for 'Num' instance.
instance Floating a => Floating (BRef s rs a) where
    pi            = BRConst pi
    exp   r       = BROp (r :< Ø)        $ op1 exp
    log   r       = BROp (r :< Ø)        $ op1 log
    sqrt  r       = BROp (r :< Ø)        $ op1 sqrt
    r1 ** r2      = BROp (r1 :< r2 :< Ø) $ op2 (**)
    logBase r1 r2 = BROp (r1 :< r2 :< Ø) $ op2 logBase
    sin   r       = BROp (r :< Ø)        $ op1 sin
    cos   r       = BROp (r :< Ø)        $ op1 cos
    tan   r       = BROp (r :< Ø)        $ op1 tan
    asin  r       = BROp (r :< Ø)        $ op1 asin
    acos  r       = BROp (r :< Ø)        $ op1 acos
    atan  r       = BROp (r :< Ø)        $ op1 atan
    sinh  r       = BROp (r :< Ø)        $ op1 sinh
    cosh  r       = BROp (r :< Ø)        $ op1 cosh
    tanh  r       = BROp (r :< Ø)        $ op1 tanh
    asinh r       = BROp (r :< Ø)        $ op1 asinh
    acosh r       = BROp (r :< Ø)        $ op1 acosh
    atanh r       = BROp (r :< Ø)        $ op1 atanh

