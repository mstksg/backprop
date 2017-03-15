{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Numeric.Backprop.Iso
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- A poor substitute for the "Control.Lens.Iso" module in /lens/, providing
-- the 'Iso' type synonym and some sample useful 'Iso's for usage with
-- /backprop/, without incuring a lens dependency.
--
-- If you also import lens, you should only use this module for the
-- 'Iso's it exports, and not import the redefined 'Iso' type synonym or
-- 'from' \/ 'iso' \/ 'review'.
--

module Numeric.Backprop.Iso (
  -- * Isomorphisms
    Iso, Iso'
  -- ** Construction and usage
  , iso
  , from, review, view
  -- * Useful Isos
  , coerced
  , tup2, tup3
  , gTuple, gSOP
  , sum1, resum1
  -- * Utility types
  -- | See "Numeric.Backprop#prod" for a mini-tutorial on 'Prod' and
  -- 'Tuple', and "Numeric.Backprop#sum" for a mini-tutorial on 'Sum'.
  , Prod(..), Tuple, Sum(..), I(..)
  ) where

import           Data.Coerce
import           Data.Functor.Identity
import           Data.Profunctor.Unsafe
import           Data.Tagged
import           Data.Type.Combinator
import           Data.Type.Product
import           Data.Type.Sum
import           Lens.Micro.Extras
import           Type.Class.Higher
import qualified Generics.SOP           as SOP

-- | A family of isomorphisms.  See 'Iso''.
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | An @'Iso'' s a@ encodes an isomorphism between an 's' and an 'a'.  It
-- basically lets you go from @s -> a@ and back (from @a -> s@) while
-- preserving structure.  You can basically imagine an @'Iso'' s a@ to be
-- an @(s -> a, a -> s)@ tuple.
--
-- You can get the "forward" direction of an 'Iso'' with 'view':
--
-- @
-- 'view' :: Iso'' s a -> (s -> a)
-- @
--
-- And the "backwards" direction with 'review':
--
-- @
-- 'review' :: Iso'' s a -> (a -> s)
-- @
--
-- You can construct an 'Iso'' using 'iso', giving the forward and
-- backwards functions:
--
-- >>> myIso :: Iso' (Identity a) a
--     myIso = iso runIdentity Identity
-- >>> view myIso (Identity "hello")
-- "hello"
-- >>> review myIso "hello"
-- Identity "hello"
--
-- One powerful thing about 'Iso''s is that they're /composable/ using '.':
--
-- @
-- ('.') :: 'Iso'' c b -> 'Iso'' b a -> 'Iso'' c a
-- @
--
-- This is basically provided here so that this package doesn't incurr
-- a /lens/ dependecy, but if you already depend on /lens/, you should use
-- the version from "Control.Lens.Iso" instead.
type Iso' s a = Iso s s a a

-- | Construct an 'Iso' by giving the "forward" and "backward" direction
-- functions:
--
-- >>> myIso :: Iso' (Identity a) a
--     myIso = iso runIdentity Identity
-- >>> view myIso (Identity "hello")
-- "hello"
-- >>> review myIso "hello"
-- Identity "hello"
--
-- This is basically provided here so that this package doesn't incurr
-- a /lens/ dependecy, but if you already depend on /lens/, you should use
-- the version from "Control.Lens.Iso" instead.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso to_ from_ = dimap to_ (fmap from_)

-- | Get the "reverse" direction function from an 'Iso'.
--
-- This is basically provided here so that this package doesn't incurr
-- a /lens/ dependecy, but if you already depend on /lens/, you should use
-- the version from "Control.Lens.Review" instead.
review :: Iso s t a b -> b -> t
review i = runIdentity #. unTagged #. i .# Tagged .# Identity

-- | A useful 'Iso' between two types with the same runtime representation.
coerced :: Coercible s a => Iso' s a
coerced = iso coerce coerce

-- | An 'Iso' between a type that is a product type, and a tuple that
-- contains all of its components.  Uses "Generics.SOP" and the
-- 'SOP.Generic' typeclass.
--
-- >>> import qualified Generics.SOP as SOP
-- >>> data Foo = A Int Bool      deriving Generic
-- >>> instance SOP.Generic Foo
-- >>> view gTuple (A 10 True)
-- 10 ::< True ::< Ø
-- >>> review gTuple (15 ::< False ::< Ø)
-- A 15 False
--
gTuple :: (SOP.Generic a, SOP.Code a ~ '[as]) => Iso' a (Tuple as)
gTuple = gSOP . sum1

-- | An 'Iso' between a sum type whose constructors are products, and a sum
-- ('Sum') of products ('Tuple').  Uses "Generics.SOP" and the
-- 'SOP.Generic' typeclass.
--
-- >>> import qualified Generics.SOP as SOP
-- >>> data Bar = A Int Bool | B String Double
-- >>> instance SOP.Generic Bar
-- >>> 'view' 'gSOP' (A 10 True)
-- 'InL' (10 ::< True ::< Ø)
-- >>> 'view' 'gSOP' (B "hello" 3.4)
-- 'InR' ('InL' ("hello" ::< 3.4 ::< Ø))
-- >>> 'review' 'gTuple' ('InL' (15 ::< False ::< Ø))
-- A 15 False
-- >>> 'review' 'gTuple' ('InR' ('InL' ("bye" ::< 9.8 ::< Ø)))
-- B "bye" 9.8
gSOP :: SOP.Generic a => Iso' a (Sum Tuple (SOP.Code a))
gSOP = sop . sopTC
     . iso (map1 (map1 (I . SOP.unI))) (map1 (map1 (SOP.I . getI)))

-- | An iso between a single-type 'Sum' and the single type.
sum1 :: Iso' (Sum f '[a]) (f a)
sum1 = iso (\case InL x -> x
                  InR _ -> error "inaccessible?"
           ) InL

-- | An iso between a single type and a single-type 'Sum'.
resum1 :: Iso' (f a) (Sum f '[a])
resum1 = iso InL
             (\case InL x -> x
                    InR _ -> error "inaccessible?"
             )

-- | Reverse an 'Iso''.  The forward function becomes the backwards
-- function, and the backwards function becomes the forward function.
--
-- This is basically provided here so that this package doesn't incurr
-- a /lens/ dependecy, but if you already depend on /lens/, you should use
-- the version from "Control.Lens.Review" instead.
from :: Iso' s a -> Iso' a s
from i = iso (review i) (view i)

sop :: SOP.Generic a => Iso' a (SOP.SOP SOP.I (SOP.Code a))
sop = iso SOP.from SOP.to

sopTC :: Iso' (SOP.SOP f as) (Sum (Prod f) as)
sopTC = iso SOP.unSOP SOP.SOP
      . nsSum
      . iso (map1 (view npProd)) (map1 (review npProd))

npProd :: Iso' (SOP.NP f as) (Prod f as)
npProd = iso to_ from_
  where
    to_ :: SOP.NP f as -> Prod f as
    to_ = \case
      SOP.Nil     -> Ø
      x SOP.:* xs -> x :< to_ xs
    from_ :: Prod f as -> SOP.NP f as
    from_ = \case
      Ø       -> SOP.Nil
      x :< xs -> x SOP.:* from_ xs

nsSum :: Iso' (SOP.NS f as) (Sum f as)
nsSum = iso to_ from_
  where
    to_ :: SOP.NS f as -> Sum f as
    to_ = \case
      SOP.Z x  -> InL x
      SOP.S xs -> InR (to_ xs)
    from_ :: Sum f as -> SOP.NS f as
    from_ = \case
      InL x  -> SOP.Z x
      InR xs -> SOP.S (from_ xs)

tup2 :: forall a b. Iso' (a, b) (Tuple '[a, b])
tup2 = iso to_ from_
  where
    to_   :: (a, b) -> Tuple '[a, b]
    to_   (x, y)            = x ::< y ::< Ø
    from_ :: Tuple '[a, b] -> (a, b)
    from_ (I x :< I y :< Ø) = (x, y)

tup3 :: forall a b c. Iso' (a, b, c) (Tuple '[a, b, c])
tup3 = iso to_ from_
  where
    to_   :: (a, b, c) -> Tuple '[a, b, c]
    to_   (x, y, z)                = x ::< y ::< z ::< Ø
    from_ :: Tuple '[a, b, c] -> (a, b, c)
    from_ (I x :< I y :< I z :< Ø) = (x, y, z)
