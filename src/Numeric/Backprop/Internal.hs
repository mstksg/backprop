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
    FRTerminal g  -> pure (FRTerminal g)














-- internal helpers, which are done easily using functions in
-- Numeric.Backprop.Op, but are duplicated here to prevent cyclic
-- dependencies

op0 :: (Known Length as, Every Num as) => a -> Op as a
op0 x = Op $ \xs -> (x, const (imap1 (\ix _ -> 0 \\ every @_ @Num ix) xs))

op1 :: (a -> b) -> (a -> a) -> (a -> b -> a) -> Op '[a] b
op1 f df df' = Op $ \(I x :< Ø) -> (f x, only_ . maybe (df x) (df' x))

op2 :: (a -> b -> c) -> (a -> b -> (a, b)) -> (a -> b -> c -> (a, b)) -> Op '[a, b] c
op2 f df df' = Op $ \(I x :< I y :< Ø) -> (f x y, maybe (tup $ df x y) (tup . df' x y))
  where
    tup (x, y) = x ::< y ::< Ø

instance (Known Length as, Every Num as, Num a) => Num (Op as a) where
    o1 + o2       = composeOp summers (o1 :< o2 :< Ø) $ op2 (+) (\_ _ -> (1,  1)) (\_ _ g -> (g,  g))
    o1 - o2       = composeOp summers (o1 :< o2 :< Ø) $ op2 (-) (\_ _ -> (1, -1)) (\_ _ g -> (g, -g))
    o1 * o2       = composeOp summers (o1 :< o2 :< Ø) $ op2 (*) (\x y -> (y,  x)) (\x y g -> (g*y, x*g))
    negate o      = composeOp summers (o :< Ø) $ op1 negate (const (-1)) (const negate)
    signum o      = composeOp summers (o :< Ø) $ op1 signum (const 0)    (const (const 0))
    abs    o      = composeOp summers (o :< Ø) $ op1 abs    signum       (\x g -> signum x * g)
    fromInteger x = op0 (fromInteger x)

instance Num a => Num (BPRef s rs a) where
    r1 + r2       = BPROp (r1 :< r2 :< Ø) $ op2 (+) (\_ _ -> (1,  1)) (\_ _ g -> (g,  g))
    r1 - r2       = BPROp (r1 :< r2 :< Ø) $ op2 (-) (\_ _ -> (1, -1)) (\_ _ g -> (g, -g))
    r1 * r2       = BPROp (r1 :< r2 :< Ø) $ op2 (*) (\x y -> (y,  x)) (\x y g -> (g*y, x*g))
    negate r      = BPROp (r :< Ø) $ op1 negate (const (-1)) (const negate)
    signum r      = BPROp (r :< Ø) $ op1 signum (const 0)    (const (const 0))
    abs    r      = BPROp (r :< Ø) $ op1 abs    signum       (\x g -> signum x * g)
    fromInteger x = BPRConst (fromIntegral x)

instance (Known Length as, Every Fractional as, Every Num as, Fractional a) => Fractional (Op as a) where
    recip o        = composeOp summers (o :< Ø) $ op1 recip (\x -> -1/(x*x)) (\x g -> -g/(x*x))
    fromRational x = op0 (fromRational x)

instance Fractional a => Fractional (BPRef s rs a) where
    recip r        = BPROp (r :< Ø) $ op1 recip (\x -> -1/(x*x)) (\x g -> -g/(x*x))
    fromRational x = BPRConst (fromRational x)

-- TODO: logBase and (**)
instance (Known Length as, Every Floating as, Every Fractional as, Every Num as, Floating a) => Floating (Op as a) where
    pi      = op0 pi
    exp   o = composeOp summers (o :< Ø) $ op1 exp  exp                        (\x g -> exp x * g)
    log   o = composeOp summers (o :< Ø) $ op1 log  recip                      (\x g -> g / x    )
    sqrt  o = composeOp summers (o :< Ø) $ op1 sqrt (\x -> 1 / (2 * sqrt x))   (\x g -> g / (2 * sqrt x))
    sin   o = composeOp summers (o :< Ø) $ op1 sin  cos                        (\x g ->  g * cos x)
    cos   o = composeOp summers (o :< Ø) $ op1 cos  (negate . sin)             (\x g -> -g * sin x)
    tan   o = composeOp summers (o :< Ø) $ op1 tan  (\x -> 1 / (cos x ** 2))   (\x g ->  g / (cos x ** 2))
    asin  o = composeOp summers (o :< Ø) $ op1 asin (\x ->  1 / sqrt(1 - x*x)) (\x g ->  g / sqrt(1 - x*x))
    acos  o = composeOp summers (o :< Ø) $ op1 acos (\x -> -1 / sqrt(1 - x*x)) (\x g -> -g / sqrt(1 - x*x))
    atan  o = composeOp summers (o :< Ø) $ op1 atan (\x ->  1 / (1 + x*x)    ) (\x g ->  g / (1 + x*x))
    sinh  o = composeOp summers (o :< Ø) $ op1 sinh cosh                       (\x g -> g * cosh x )
    cosh  o = composeOp summers (o :< Ø) $ op1 cosh sinh                       (\x g -> g * sinh x )
    asinh o = composeOp summers (o :< Ø) $ op1 asinh (\x -> 1 / sqrt(1 + x*x)) (\x g -> g / sqrt(1 + x*x))
    acosh o = composeOp summers (o :< Ø) $ op1 acosh (\x -> 1 / (sqrt(x-1)*sqrt(x+1))) (\x g -> g / (sqrt(x-1)*sqrt(x+1)))
    atanh o = composeOp summers (o :< Ø) $ op1 atanh (\x ->  1 / (1 - x*x)) (\x g ->  g / (1 - x*x))

instance Floating a => Floating (BPRef s rs a) where
    pi      = BPRConst pi
    exp   r = BPROp (r :< Ø) $ op1 exp  exp                        (\x g -> exp x * g)
    log   r = BPROp (r :< Ø) $ op1 log  recip                      (\x g -> g / x    )
    sqrt  r = BPROp (r :< Ø) $ op1 sqrt (\x -> 1 / (2 * sqrt x))   (\x g -> g / (2 * sqrt x))
    sin   r = BPROp (r :< Ø) $ op1 sin  cos                        (\x g ->  g * cos x)
    cos   r = BPROp (r :< Ø) $ op1 cos (negate . sin)              (\x g -> -g * sin x)
    tan   r = BPROp (r :< Ø) $ op1 tan  (\x -> 1 / (cos x ** 2))   (\x g ->  g / (cos x ** 2))
    asin  r = BPROp (r :< Ø) $ op1 asin (\x ->  1 / sqrt(1 - x*x)) (\x g ->  g / sqrt(1 - x*x))
    acos  r = BPROp (r :< Ø) $ op1 acos (\x -> -1 / sqrt(1 - x*x)) (\x g -> -g / sqrt(1 - x*x))
    atan  r = BPROp (r :< Ø) $ op1 atan (\x ->  1 / (1 + x*x)    ) (\x g ->  g / (1 + x*x))
    sinh  r = BPROp (r :< Ø) $ op1 sinh cosh                       (\x g -> g * cosh x )
    cosh  r = BPROp (r :< Ø) $ op1 cosh sinh                       (\x g -> g * sinh x )
    asinh r = BPROp (r :< Ø) $ op1 asinh (\x -> 1 / sqrt(1 + x*x)) (\x g -> g / sqrt(1 + x*x))
    acosh r = BPROp (r :< Ø) $ op1 acosh (\x -> 1 / (sqrt(x-1)*sqrt(x+1))) (\x g -> g / (sqrt(x-1)*sqrt(x+1)))
    atanh r = BPROp (r :< Ø) $ op1 atanh (\x -> 1 / (1 - x*x))     (\x g ->  g / (1 - x*x))







