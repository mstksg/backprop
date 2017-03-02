{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Backprop.Iso
  ( Iso, Iso'
  , iso
  , review
  , gTuple
  ) where

import           Data.Functor.Identity
import           Data.Profunctor.Unsafe
import           Data.Tagged
import           Data.Type.Combinator
import           Data.Type.Product
import           Data.Type.Sum
import           Lens.Micro.Extras
import           Type.Class.Higher
import qualified Generics.SOP           as SOP

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso to from = dimap to (fmap from)

review :: Iso s t a b -> b -> t
review i = runIdentity #. unTagged #. i .# Tagged .# Identity

gTuple :: (SOP.Generic a, SOP.Code a ~ '[as]) => Iso' a (Tuple as)
gTuple = sop
       . sopTC
       . iso (map1 (map1 (I . SOP.unI))) (map1 (map1 (SOP.I . getI)))
       . sum1

sum1 :: Iso' (Sum f '[a]) (f a)
sum1 = iso (\case InL x -> x
                  InR _ -> error "inaccessible?"
           ) InL

sop :: SOP.Generic a => Iso' a (SOP.SOP SOP.I (SOP.Code a))
sop = iso SOP.from SOP.to

sopTC :: Iso' (SOP.SOP f as) (Sum (Prod f) as)
sopTC = iso SOP.unSOP SOP.SOP
      . nsSum
      . iso (map1 (view npProd)) (map1 (review npProd))

npProd :: Iso' (SOP.NP f as) (Prod f as)
npProd = iso to from
  where
    to :: SOP.NP f as -> Prod f as
    to = \case
      SOP.Nil     -> Ø
      x SOP.:* xs -> x :< to xs
    from :: Prod f as -> SOP.NP f as
    from = \case
      Ø       -> SOP.Nil
      x :< xs -> x SOP.:* from xs

nsSum :: Iso' (SOP.NS f as) (Sum f as)
nsSum = iso to from
  where
    to :: SOP.NS f as -> Sum f as
    to = \case
      SOP.Z x  -> InL x
      SOP.S xs -> InR (to xs)
    from :: Sum f as -> SOP.NS f as
    from = \case
      InL x  -> SOP.Z x
      InR xs -> SOP.S (from xs)
