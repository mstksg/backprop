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
import           Data.Type.Util
import           Lens.Micro
import           Lens.Micro.TH
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.List

newtype Op f as a = Op { runOp' :: Prod f as -> (f a, Maybe (f a) -> Prod f as) }

newtype Summer :: (k -> Type) -> k -> Type where
    Summer :: { runSummer :: [f a] -> f a } -> Summer f a
newtype Unity :: (k -> Type) -> k -> Type where
    Unity  :: { getUnity  :: f a          } -> Unity f a

instance Num (f a) => Known (Summer f) a where
    type KnownC (Summer f) a = Num (f a)
    known = Summer sum

instance Num (f a) => Known (Unity f) a where
    type KnownC (Unity f) a = Num (f a)
    known = Unity 1

summers
    :: forall k (f :: k -> Type) (as :: [k]). (Every Num (f <$> as), Known Length as)
    => Prod (Summer f) as
summers = map1 ((// known) . every @_ @Num . reIndex @_ @f) indices

unities
    :: forall k (f :: k -> Type) (as :: [k]). (Every Num (f <$> as), Known Length as)
    => Prod (Unity f) as
unities = map1 ((// known) . every @_ @Num . reIndex @_ @f) indices

data ForwardRefs s f rs a = FRInternal ![BPInpRef s f rs a]
                          | FRExternal (f a)
                          | FRTerminal

data BPState :: Type -> (k -> Type) -> [k] -> Type where
    BPS :: { _bpsSources :: !(Prod (ForwardRefs s f rs) rs)
           }
        -> BPState s f rs

newtype BP s f rs b = BP { bpST :: ReaderT (Prod f rs) (StateT (BPState s f rs) (ST s)) b }
      deriving ( Functor
               , Applicative
               , Monad
               , MonadReader (Prod f rs)
               , MonadState (BPState s f rs)
               , MonadBase (ST s)
               )

data BPRef :: Type -> (k -> Type) -> [k] -> k -> Type where
    BPRNode :: !(STRef s (BPNode s f rs as a))
            -> BPRef s f rs a
    BPRInp  :: !(Index rs a)
            -> BPRef s f rs a

data BPInpRef :: Type -> (k -> Type) -> [k] -> k -> Type where
    BPIR :: { _bpirIndex :: !(Index bs a)
            , _bpirRef   :: !(STRef s (BPNode s f rs bs b))
            }
         -> BPInpRef s f rs a

data BPNode :: Type -> (k -> Type) -> [k] -> [k] -> k -> Type where
    BPN :: { _bpnOut       :: !(ForwardRefs s f rs a)
           , _bpnRes       :: !(f a)
           , _bpnGradFunc  :: !(Maybe (f a) -> ST s (Prod f as))
           , _bpnGradCache :: !(Maybe (Maybe (f a), Prod f as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Summer f a)
           }
        -> BPNode s f rs as a

makeLenses ''BPState
makeLenses ''BPNode

_FRInternal
    :: Traversal (ForwardRefs s f as a) (ForwardRefs t f bs a)
                 [BPInpRef s f as a]    [BPInpRef t f bs a]
_FRInternal f = \case
    FRInternal xs -> FRInternal <$> f xs
    FRExternal x  -> pure (FRExternal x)
    FRTerminal    -> pure FRTerminal

