{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
 , BPNode(..), bpnInp, bpnOut, bpnOp, bpnResCache, bpnGradCache, bpnSummer
 , BPRef(..)
 , ForwardRefs(..), _FRInternal, frMaybe
 ) where

import           Control.Monad.Base
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

newtype Summer a   = Summer { runSummer :: [a] -> a }
newtype Unity  a   = Unity  { getUnity  :: a }

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


data ForwardRefs s as a = FRInternal ![BPInpRef s as a]
                        | FRTerminal

data BPState :: Type -> [Type] -> Type where
    BPS :: { _bpsSources :: !(Prod (ForwardRefs s as) as)
           }
        -> BPState s as

newtype BP s as b = BP { bpST :: StateT (BPState s as) (ST s) b }
      deriving (Functor, Applicative, Monad, MonadState (BPState s as), MonadBase (ST s))

data BPRef :: Type -> [Type] -> Type -> Type where
    BPRNode :: !(STRef s (BPNode s as bs a))
            -> BPRef s as a
    BPRInp  :: !(Index as a)
            -> BPRef s as a

data BPInpRef :: Type -> [Type] -> Type -> Type where
    BPIR :: { _bpirIndex :: !(Index bs a)
            , _bpirRef   :: !(STRef s (BPNode s as bs b))
            }
         -> BPInpRef s as a

data BPNode :: Type -> [Type] -> [Type] -> Type -> Type where
    BPN :: { _bpnInp       :: !(Prod (BPRef s bs) as)
           , _bpnOut       :: !(ForwardRefs s bs a)
           , _bpnOp        :: !(Op as a)
           , _bpnResCache  :: !(Maybe (a, Maybe a -> Tuple as))
           , _bpnGradCache :: !(Maybe (Maybe a, Tuple as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Summer a)
           }
        -> BPNode s bs as a

makeLenses ''BPState
makeLenses ''BPNode

_FRInternal
    :: Traversal (ForwardRefs s as a) (ForwardRefs t bs b)
                 [BPInpRef s as a]    [BPInpRef t bs b]
_FRInternal f = \case
    FRInternal xs -> FRInternal <$> f xs
    FRTerminal    -> pure FRTerminal

frMaybe
    :: Lens (ForwardRefs s as a) (ForwardRefs t bs b)
            (Maybe [BPInpRef s as a]) (Maybe [BPInpRef t bs b])
frMaybe f = \case
    FRInternal xs -> maybeFr <$> f (Just xs)
    FRTerminal    -> maybeFr <$> f Nothing
  where
    maybeFr (Just xs) = FRInternal xs
    maybeFr Nothing   = FRTerminal


