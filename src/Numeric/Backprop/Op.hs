{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Numeric.Backprop.Op (
  -- * Type
    Op, OpM(.., Op, runOp')
  -- ** Running
  , runOp, gradOp, gradOpWith, gradOpWith'
  , runOpM, gradOpWithM, gradOpWithM'
  -- ** Manipulation
  , composeOp
  -- * Creation
  , op0
  , op1, op2, op3, opN, opCoerce, opTup, opIso
  , op1', op2', op3', opN', opCoerce', opTup', opIso'
  -- * Utility
  , Prod(..), pattern (:>), only, head'
  , Tuple, pattern (::<), only_
  , Replicate
  ) where

import           Data.Bifunctor
import           Data.Coerce
import           Data.Maybe
import           Data.Reflection                  (Reifies)
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Util
import           Data.Type.Vector hiding          (head')
import           Lens.Micro.Extras
import           Numeric.AD
import           Numeric.AD.Internal.Reverse      (Reverse, Tape)
import           Numeric.AD.Mode.Forward hiding   (grad')
import           Numeric.Backprop.Internal.Helper
import           Numeric.Backprop.Iso
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

-- instead of Tuple as, Prod Diff as, where Diff can be a value, or zero,
-- or one?

-- newtype Op as a = Op { runOp' :: Tuple as -> (a, Maybe a -> Tuple as) }

type Op  as a = forall m. Monad m => OpM m as a

newtype OpM m as a = OpM { runOpM' :: Tuple as -> m (a, Maybe a -> m (Tuple as)) }

newtype OpCont m as a = OC { runOpCont :: Maybe a -> m (Tuple as) }

composeOp
    :: Monad m
    => Prod Summer as
    -> Prod (OpM m as) bs
    -> OpM m bs c
    -> OpM m as c
composeOp ss os o = OpM $ \xs -> do
    (ys, conts) <- fmap unzipP
                 . traverse1 (fmap (\(x, c) -> I x :&: OC c) . flip runOpM' xs)
                 $ os
    (z, gFz) <- runOpM' o ys
    let gFunc g0 = do
          g1  <- gFz g0
          g2s <- sequenceA
                    . toList (\(oc :&: I g) -> runOpCont oc (Just g))
                    $ conts `zipP` g1
          return $ map1 (\(s :&: gs) -> I (runSummer s gs))
                 . zipP ss
                 . foldr (\x -> map1 (uncurryFan (\(I y) -> (y:))) . zipP x)
                         (map1 (const []) ss)
                 $ g2s
    return (z, gFunc)

pattern Op :: (Tuple as -> (a, Maybe a -> Tuple as)) -> Op as a
pattern Op { runOp' }  <- OpM ((\f -> (second . fmap) getI . getI . f) -> runOp')
  where
    Op f = OpM (pure . (second . fmap) pure . f)

runOp :: Op as a -> Tuple as -> a
runOp o = fst . runOp' o

runOpM :: Functor m => OpM m as a -> Tuple as -> m a
runOpM o = fmap fst . runOpM' o

gradOpWith' :: Op as a -> Tuple as -> Maybe a -> Tuple as
gradOpWith' o = snd . runOp' o

gradOpWithM' :: Monad m => OpM m as a -> Tuple as -> Maybe a -> m (Tuple as)
gradOpWithM' o xs g = do
    (_, f) <- runOpM' o xs
    f g

gradOpWith :: Op as a -> Tuple as -> a -> Tuple as
gradOpWith o i = gradOpWith' o i . Just

gradOpWithM :: Monad m => OpM m as a -> Tuple as -> a -> m (Tuple as)
gradOpWithM o i = gradOpWithM' o i . Just

gradOp :: Op as a -> Tuple as -> Tuple as
gradOp o i = gradOpWith' o i Nothing

opCoerce' :: Coercible a b => Unity a -> Op '[a] b
opCoerce' u = opIso' u coercible

opCoerce :: (Coercible a b, Num a) => Op '[a] b
opCoerce = opIso coercible

opTup'
    :: Prod Unity as
    -> Op as (Tuple as)
opTup' u = Op $ \xs -> (xs, fromMaybe (map1 (I . getUnity) u))

opTup
    :: (Every Num as, Known Length as)
    => Op as (Tuple as)
opTup = opTup' (map1 ((// known) . every @_ @Num) indices)

opIso' :: Unity a -> Iso' a b -> Op '[ a ] b
opIso' u i = op1' $ \x -> (view i x, maybe (getUnity u) (review i))

opIso :: Num a => Iso' a b -> Op '[ a ] b
opIso = opIso' known

opConst' :: Prod Summer as -> a -> Op as a
opConst' ss x = Op $ \_ ->
    (x , const $ map1 (\s -> I $ runSummer s []) ss)

opConst :: (Every Num as, Known Length as) => a -> Op as a
opConst = opConst' summers

op0 :: a -> Op '[] a
op0 x = Op $ \case
    Ø -> (x, const Ø)

op1'
    :: (a -> (b, Maybe b -> a))
    -> Op '[a] b
op1' f = Op $ \case
    I x :< Ø ->
      let (y, dx) = f x
      in  (y, only_ . dx)

op2'
    :: (a -> b -> (c, Maybe c -> (a, b)))
    -> Op '[a,b] c
op2' f = Op $ \case
    I x :< I y :< Ø ->
      let (z, dxdy) = f x y
      in  (z, (\(dx,dy) -> dx ::< dy ::< Ø) . dxdy)

op3'
    :: (a -> b -> c -> (d, Maybe d -> (a, b, c)))
    -> Op '[a,b,c] d
op3' f = Op $ \case
    I x :< I y :< I z :< Ø ->
      let (q, dxdydz) = f x y z
      in  (q, (\(dx, dy, dz) -> dx ::< dy ::< dz ::< Ø) . dxdydz)

opN' :: (Known Nat n)
     => (Vec n a -> (b, Maybe b -> Vec n a))
     -> Op (Replicate n a) b
opN' f = Op $ (second . fmap) vecToProd
            . f
            . prodToVec' known

op1 :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Op '[a] a
op1 f = op1' $ \x ->
    let (z, dx) = diff' f x
    in  (z, maybe dx (* dx))

op2 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a] a
op2 f = opN $ \case I x :* I y :* ØV -> f x y

op3 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a,a] a
op3 f = opN $ \case I x :* I y :* I z :* ØV -> f x y z

opN :: (Num a, Known Nat n)
    => (forall s. Reifies s Tape => Vec n (Reverse s a) -> Reverse s a)
    -> Op (Replicate n a) a
opN f = opN' $ \xs ->
    let (y, dxs) = grad' f xs
    in  (y, maybe dxs (\q -> (q *) <$> dxs))

instance (Monad m, Known Length as, Every Num as, Num a) => Num (OpM m as a) where
    o1 + o2       = composeOp summers (o1 :< o2 :< Ø) $ op2 (+)
    o1 - o2       = composeOp summers (o1 :< o2 :< Ø) $ op2 (-)
    o1 * o2       = composeOp summers (o1 :< o2 :< Ø) $ op2 (*)
    negate o      = composeOp summers (o :< Ø)        $ op1 negate
    signum o      = composeOp summers (o :< Ø)        $ op1 signum
    abs    o      = composeOp summers (o :< Ø)        $ op1 abs
    fromInteger x = opConst (fromInteger x)

instance (Monad m, Known Length as, Every Fractional as, Every Num as, Fractional a) => Fractional (OpM m as a) where
    o1 / o2        = composeOp summers (o1 :< o2 :< Ø) $ op2 (/)
    recip o        = composeOp summers (o :< Ø)        $ op1 recip
    fromRational x = opConst (fromRational x)

instance (Monad m, Known Length as, Every Floating as, Every Fractional as, Every Num as, Floating a) => Floating (OpM m as a) where
    pi            = opConst pi
    exp   o       = composeOp summers (o :< Ø)        $ op1 exp
    log   o       = composeOp summers (o :< Ø)        $ op1 log
    sqrt  o       = composeOp summers (o :< Ø)        $ op1 sqrt
    o1 ** o2      = composeOp summers (o1 :< o2 :< Ø) $ op2 (**)
    logBase o1 o2 = composeOp summers (o1 :< o2 :< Ø) $ op2 logBase
    sin   o       = composeOp summers (o :< Ø)        $ op1 sin
    cos   o       = composeOp summers (o :< Ø)        $ op1 cos
    tan   o       = composeOp summers (o :< Ø)        $ op1 tan
    asin  o       = composeOp summers (o :< Ø)        $ op1 asin
    acos  o       = composeOp summers (o :< Ø)        $ op1 acos
    atan  o       = composeOp summers (o :< Ø)        $ op1 atan
    sinh  o       = composeOp summers (o :< Ø)        $ op1 sinh
    cosh  o       = composeOp summers (o :< Ø)        $ op1 cosh
    asinh o       = composeOp summers (o :< Ø)        $ op1 asinh
    acosh o       = composeOp summers (o :< Ø)        $ op1 acosh
    atanh o       = composeOp summers (o :< Ø)        $ op1 atanh

