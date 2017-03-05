{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Numeric.Backprop.Internal
  ( Summer(..), summers, summers'
  , Unity(..), unities, unities'
  , BPState(..), bpsSources
  , BP(..)
  , BPInpRef(..)
  -- , BPComp(..)
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

data ForwardRefs s rs a = FRInternal ![BPInpRef s rs a]
                        | FRTerminal !(Maybe a)

instance Monoid (ForwardRefs s rs a) where
    mempty  = FRInternal []
    mappend = \case
        FRInternal rs -> \case
          FRInternal rs'   -> FRInternal (rs ++ rs')
          t@(FRTerminal _) -> t
        t@(FRTerminal _)   -> \_ -> t

data BPState :: Type -> [Type] -> Type where
    BPS :: { _bpsSources :: !(Prod (ForwardRefs s rs) rs)
           }
        -> BPState s rs

newtype BP s rs b = BP { bpST :: ReaderT (Tuple rs) (StateT (BPState s rs) (ST s)) b }
      deriving ( Functor
               , Applicative
               , Monad
               )

data BRef :: Type -> [Type] -> Type -> Type where
    BRNode  :: !(Index bs a)
            -> !(STRef s (BPNode s rs as bs))
            -> BRef s rs a
    BRInp   :: !(Index rs a)
            -> BRef s rs a
    BRConst :: !a
            -> BRef s rs a
    BROp    :: !(Prod (BRef s rs) as)
            -> !(Op as a)
            -> BRef s rs a

data BPInpRef :: Type -> [Type] -> Type -> Type where
    IRNode  :: !(Index bs a)
            -> !(STRef s (BPNode s rs bs cs))
            -> BPInpRef s rs a
    IRPipe  :: !(Index bs a)
            -> !(STRef s (BPPipe s rs bs cs))
            -> BPInpRef s rs a
    IRConst :: !a
            -> BPInpRef s rs a

data BPNode :: Type -> [Type] -> [Type] -> [Type] -> Type where
    BPN :: { _bpnOut       :: !(Prod (ForwardRefs s rs) bs)
           , _bpnRes       :: !(Tuple bs)
           , _bpnGradFunc  :: !(Prod Maybe bs -> ST s (Tuple as))
           , _bpnGradCache :: !(Maybe (Tuple as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Prod Summer bs)
           }
        -> BPNode s rs as bs

data BPPipe :: Type -> [Type] -> [Type] -> [Type] -> Type where
    BPP :: { _bppOut       :: !(Prod (BPInpRef s rs) bs)
           , _bppRes       :: !(Tuple bs)
           , _bppGradFunc  :: !(Tuple bs -> Tuple as)
           , _bppGradCache :: !(Maybe (Tuple as))
           }
        -> BPPipe s rs as bs

makeLenses ''BPState
makeLenses ''BPNode
makeLenses ''BPPipe

_FRInternal
    :: Traversal (ForwardRefs s as a) (ForwardRefs t bs a)
                 [BPInpRef s as a]    [BPInpRef t bs a]
_FRInternal f = \case
    FRInternal xs -> FRInternal <$> f xs
    FRTerminal g  -> pure (FRTerminal g)




instance Num a => Num (BRef s rs a) where
    r1 + r2       = BROp (r1 :< r2 :< Ø) $ op2 (+)
    r1 - r2       = BROp (r1 :< r2 :< Ø) $ op2 (-)
    r1 * r2       = BROp (r1 :< r2 :< Ø) $ op2 (*)
    negate r      = BROp (r :< Ø)        $ op1 negate
    signum r      = BROp (r :< Ø)        $ op1 negate
    abs r         = BROp (r :< Ø)        $ op1 abs
    fromInteger x = BRConst (fromInteger x)

instance Fractional a => Fractional (BRef s rs a) where
    r1 / r2        = BROp (r1 :< r2 :< Ø) $ op2 (/)
    recip r        = BROp (r :< Ø)        $ op1 recip
    fromRational x = BRConst (fromRational x)

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

