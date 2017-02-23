{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module Numeric.Backprop.Internal
 ( Op(..)
 , Scaler(..)
 , Summer(..)
 , BPState(..), bpsSources
 , BP(..)
 , BPInpRef(..)
 , BPNode(..), bpnInp, bpnOut, bpnOp, bpnResCache, bpnGradCache, bpnScaler, bpnSummer
 , BPRef(..)
 ) where

import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Base
import           Data.Kind
import           Data.STRef
import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Product
import           Lens.Micro.TH
import           Type.Class.Known


newtype Op as a = Op { runOp' :: Tuple as -> (a, Tuple as) }
    deriving (Functor)

newtype Scaler a b = Scaler { runScaler :: a -> b -> b }
newtype Summer a   = Summer { runSummer :: [a] -> a }

instance Num a => Known (Scaler a) a where
    type KnownC (Scaler a) a = Num a
    known = Scaler (*)

instance Num a => Known Summer a where
    type KnownC Summer a = Num a
    known = Summer sum


data BPState :: Type -> [Type] -> Type where
    BPS :: { _bpsSources :: !(Prod (Maybe :.: [] :.: BPInpRef s as) as)
           }
        -> BPState s as

newtype BP s as b = BP { bpST :: StateT (BPState s as) (ST s) b }
      deriving (Functor, Applicative, Monad, MonadState (BPState s as), MonadBase (ST s))

data BPRef :: Type -> [Type] -> Type -> Type where
    BPRNode :: STRef s (BPNode s as bs a)
            -> BPRef s as a
    BPRInp  :: Index as a
            -> BPRef s as a

data BPInpRef :: Type -> [Type] -> Type -> Type where
    BPIR :: { _bpirIndex :: Index bs a
            , _bpirRef   :: STRef s (BPNode s as bs b)
            }
         -> BPInpRef s as a

data BPNode :: Type -> [Type] -> [Type] -> Type -> Type where
    BPN :: { _bpnInp       :: !(Prod (BPRef s bs) as)
           , _bpnOut       :: !(Maybe [BPInpRef s bs a])    -- nothing: is the "final output"
           , _bpnOp        :: !(Op as a)
           , _bpnResCache  :: !(Maybe (a, Tuple as))
           , _bpnGradCache :: !(Maybe (Maybe a, Tuple as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Summer a)
           , _bpnScaler    :: !(Prod (Scaler a) as)
           }
        -> BPNode s bs as a

makeLenses ''BPState
makeLenses ''BPNode
