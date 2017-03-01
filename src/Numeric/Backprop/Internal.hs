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

module Numeric.Backprop.Internal
 ( Op(..)
 , Summer(..), summers
 , Unity(..), unities
 , BPState(..), bpsSources
 , BP(..)
 , BPInpRef(..)
 , BPNode(..), bpnOut, bpnRes, bpnGradFunc, bpnGradCache, bpnSummer
 , BPRef(..)
 , ForwardRefs(..), _FRInternal
 ) where

import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Kind
import           Data.STRef
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Lens.Micro
import           Lens.Micro.TH
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

newtype Op as a = Op { runOp' :: Tuple as -> (a, Maybe a -> Tuple as) }

newtype Summer a = Summer { runSummer :: [a] -> a }
newtype Unity  a = Unity  { getUnity  :: a        }
    deriving (Functor, Show, Ord, Eq, Num, Fractional, Floating, Integral, Real, Enum, RealFloat, RealFrac)

instance Num a => Known Summer a where
    type KnownC Summer a = Num a
    known = Summer sum

instance Num a => Known Unity a where
    type KnownC Unity a = Num a
    known = Unity 1

summers
    :: (Every Num as, Known Length as)
    => Prod Summer as
summers = map1 ((// known) . every @_ @Num) indices

unities
    :: (Every Num as, Known Length as)
    => Prod Unity as
unities = map1 ((// known) . every @_ @Num) indices

data ForwardRefs s rs a = FRInternal ![BPInpRef s rs a]
                        | FRExternal a
                        | FRTerminal

data BPState :: Type -> [Type] -> Type where
    BPS :: { _bpsSources :: !(Prod (ForwardRefs s rs) rs)
           }
        -> BPState s rs

newtype BP s rs b = BP { bpST :: ReaderT (Tuple rs) (StateT (BPState s rs) (ST s)) b }
      deriving ( Functor
               , Applicative
               , Monad
               , MonadReader (Tuple rs)
               , MonadState (BPState s rs)
               , MonadBase (ST s)
               )

data BPRef :: Type -> [Type] -> Type -> Type where
    BPRNode :: !(STRef s (BPNode s rs as a))
            -> BPRef s rs a
    BPRInp  :: !(Index rs a)
            -> BPRef s rs a

data BPInpRef :: Type -> [Type] -> Type -> Type where
    BPIR :: { _bpirIndex :: !(Index bs a)
            , _bpirRef   :: !(STRef s (BPNode s rs bs b))
            }
         -> BPInpRef s rs a

data BPNode :: Type -> [Type] -> [Type] -> Type -> Type where
    BPN :: { _bpnOut       :: !(ForwardRefs s rs a)
           , _bpnRes       :: !a
           , _bpnGradFunc  :: !(Maybe a -> ST s (Tuple as))
           , _bpnGradCache :: !(Maybe (Maybe a, Tuple as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Summer a)
           }
        -> BPNode s rs as a

makeLenses ''BPState
makeLenses ''BPNode

_FRInternal
    :: Traversal (ForwardRefs s as a) (ForwardRefs t bs a)
                 [BPInpRef s as a]    [BPInpRef t bs a]
_FRInternal f = \case
    FRInternal xs -> FRInternal <$> f xs
    FRExternal x  -> pure (FRExternal x)
    FRTerminal    -> pure FRTerminal

