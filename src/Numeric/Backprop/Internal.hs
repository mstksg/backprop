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
 ( Op(..), composeOp
 , Summer(..), summers, summers'
 , Unity(..), unities, unities'
 , BPState(..), bpsSources
 , BP(..)
 , BPInpRef(..)
 -- , BPComp(..)
 , BPNode(..), bpnOut, bpnRes, bpnGradFunc, bpnGradCache, bpnSummer
 , BPPipe(..), bppOut, bppRes, bppGradFunc, bppGradCache
 , BPRef(..)
 , ForwardRefs(..), _FRInternal
 ) where

import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Kind
import           Data.STRef
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Lens.Micro hiding     (ix)
import           Lens.Micro.TH
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

-- instead of Tuple as, Prod Diff as, where Diff can be a value, or zero,
-- or one?
newtype Op as a = Op { runOp' :: Tuple as -> (a, Maybe a -> Tuple as) }

newtype OpCont as a = OC { runOpCont :: Maybe a -> Tuple as }

composeOp :: Prod Summer as -> Prod (Op as) bs -> Op bs c -> Op as c
composeOp ss os o = Op $ \xs ->
    let (ys, conts) = unzipP
                    . map1 ((\(x, c) -> I x :&: OC c) . flip runOp' xs)
                    $ os
        (z, gFz) = runOp' o ys
    in  (z, map1 (\(s :&: gs) -> I $ runSummer s gs)
          . zipP ss
          . foldr (\x -> map1 (uncurryFan (\(I y) -> (y:))) . zipP x) (map1 (const []) ss)
          . toList (\(oc :&: I g) -> runOpCont oc (Just g))
          . zipP conts . gFz
        )


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
summers = summers' known

summers'
    :: Every Num as
    => Length as
    -> Prod Summer as
summers' l = withEvery' @Num l known

unities
    :: (Every Num as, Known Length as)
    => Prod Unity as
unities = unities' known

unities'
    :: Every Num as
    => Length as
    -> Prod Unity as
unities' l = withEvery' @Num l known

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
               )

data BPRef :: Type -> [Type] -> Type -> Type where
    BPRNode  :: !(Index bs a)
             -> !(STRef s (BPNode s rs as bs))
             -> BPRef s rs a
    BPRInp   :: !(Index rs a)
             -> BPRef s rs a
    BPRConst :: !a
             -> BPRef s rs a
    BPROp    :: !(Prod (BPRef s rs) as)
             -> !(Op as a)
             -> BPRef s rs a

data BPInpRef :: Type -> [Type] -> Type -> Type where
    IRNode :: !(Index bs a)
           -> !(STRef s (BPNode s rs bs cs))
           -> BPInpRef s rs a
    IRPipe :: !(Index bs a)
           -> !(STRef s (BPPipe s rs bs cs))
           -> BPInpRef s rs a

-- data BPComp :: Type -> [Type] -> [Type] -> [Type] -> Type where
--     BPCNode :: !(BPNode s rs as bs)
--             -> BPComp s rs as bs
--     BPCPipe :: !(BPPipe s rs as bs)
--             -> BPComp s rs as bs

data BPNode :: Type -> [Type] -> [Type] -> [Type] -> Type where
    BPN :: { _bpnOut       :: !(Prod (ForwardRefs s rs) bs)
           , _bpnRes       :: !(Tuple bs)
           , _bpnGradFunc  :: !(Prod Maybe bs -> ST s (Tuple as))
           , _bpnGradCache :: !(Maybe (Tuple as))  -- nothing if is the "final output"
           , _bpnSummer    :: !(Prod Summer bs)
           }
        -> BPNode s rs as bs
-- data ForwardRefs s rs a = FRInternal ![BPInpRef s rs a]

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
    FRExternal x  -> pure (FRExternal x)
    FRTerminal    -> pure FRTerminal







-- internal helpers, which are done easily using functions in
-- Numeric.Backprop.Op, but are duplicated here to prevent cyclic
-- dependencies

opPlus :: Num a => Op '[a, a] a
opPlus = Op $ \(I x :< I y :< Ø) ->
    (x + y, \case Nothing -> 1 ::< 1 ::< Ø
                  Just g  -> g ::< g ::< Ø
    )

opMinus :: Num a => Op '[a, a] a
opMinus = Op $ \(I x :< I y :< Ø) ->
    (x - y, \case Nothing -> 1 ::< (-1) ::< Ø
                  Just g  -> g ::< (-g) ::< Ø
    )

opTimes :: Num a => Op '[a, a] a
opTimes = Op $ \(I x :< I y :< Ø) ->
    (x * y, \case Nothing -> y     ::< x     ::< Ø
                  Just g  -> (g*y) ::< (x*g) ::< Ø
    )

opNegate :: Num a => Op '[a] a
opNegate = Op $ \(I x :< Ø) ->
    (negate x, \case Nothing -> (-1) ::< Ø
                     Just g  -> (-g) ::< Ø
    )

opAbs :: Num a => Op '[a] a
opAbs = Op $ \(I x :< Ø) ->
    (abs x   , \case Nothing -> signum x       ::< Ø
                     Just g  -> (g * signum x) ::< Ø
    )

opSignum :: Num a => Op '[a] a
opSignum = Op $ \(I x :< Ø) -> (signum x, const (0 ::< Ø))

opRecip :: Fractional a => Op '[a] a
opRecip = Op $ \(I x :< Ø) ->
    (recip x, \case Nothing -> (-1/(x*x)) ::< Ø
                    Just g  -> (-g/(x*x)) ::< Ø
    )





instance (Known Length as, Every Num as, Num a) => Num (Op as a) where
    o1 + o2       = composeOp summers (o1 :< o2 :< Ø) opPlus
    o1 - o2       = composeOp summers (o1 :< o2 :< Ø) opMinus
    o1 * o2       = composeOp summers (o1 :< o2 :< Ø) opTimes
    negate o      = composeOp summers (o :< Ø) opNegate
    signum o      = composeOp summers (o :< Ø) opSignum
    abs    o      = composeOp summers (o :< Ø) opAbs
    fromInteger x = Op $ \xs -> (fromInteger x, const (imap1 (\ix _ -> 0 \\ every @_ @Num ix) xs))

instance (Known Length as, Every Fractional as, Every Num as, Fractional a) => Fractional (Op as a) where
    recip o        = composeOp summers (o :< Ø) . Op $ \(I x :< Ø) ->
                       (1/x, \case Nothing -> (-1 / (x * x)) ::< Ø
                                   Just g  -> (-g / (x * x)) ::< Ø
                       )
    fromRational x = Op $ \xs -> (fromRational x, const (imap1 (\ix _ -> 0 \\ every @_ @Fractional ix) xs))

instance Num a => Num (BPRef s rs a) where
    r1 + r2       = BPROp (r1 :< r2 :< Ø) opPlus
    r1 - r2       = BPROp (r1 :< r2 :< Ø) opMinus
    r1 * r2       = BPROp (r1 :< r2 :< Ø) opTimes
    negate r      = BPROp (r :< Ø) opNegate
    signum r      = BPROp (r :< Ø) opSignum
    abs    r      = BPROp (r :< Ø) opAbs
    fromInteger x = BPRConst (fromIntegral x)

instance Fractional a => Fractional (BPRef s rs a) where
    recip r        = BPROp (r :< Ø) opRecip
    fromRational x = BPRConst (fromRational x)
