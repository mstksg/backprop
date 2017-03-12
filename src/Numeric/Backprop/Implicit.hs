{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      : Numeric.Backprop.Implicit
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Offers full functionality for implicit-graph back-propagation.  The
-- intended usage is to write a 'BPOp', which is a normal Haskell
-- function from 'BVar's to a result 'BVar'. These 'BVar's can be
-- manipulated using their 'Num' \/ 'Fractional' \/ 'Floating' instances.
--
-- The library can then perform back-propagation on the function (using
-- 'backprop' or 'grad') by using an implicitly built graph.
--
-- This should actually be powerful enough for most use cases, but falls
-- short for a couple of situations:
--
-- 1. If the result of a function on 'BVar's is used twice
-- (like @z@ in @let z = x * y in z + z@), this will allocate a new
-- redundant graph node for every usage site of @z@.  You can explicitly
-- /force/ @z@, but only using an explicit graph description using
-- "Numeric.Backprop".
--
-- 2. This can't handle sum types, like "Numeric.Backprop" can.  You can
-- never pattern match on the constructors of a value inside a 'BVar'.  I'm
-- not sure if this is a fundamental limitation (I suspect it might be) or
-- if I just can't figure out how to implement it.  Suggestions welcome!
--
-- As a comparison, this module offers functionality and an API very
-- similar to "Numeric.AD.Mode.Reverse" from the /ad/ library, except for
-- the fact that it can handle /heterogeneous/ values.
--
-- Note that every type involved has to be an instance of 'Num'.  This is
-- because gradients all need to be "summable" (which is implemented using
-- 'sum' and '+'), and we also need to able to generate gradients of '1'
-- and '0'.


module Numeric.Backprop.Implicit (
  -- * Types
  -- ** Backprop types
    BPOp, BVar, Op, OpB
  -- ** Tuple types
  -- | See "Numeric.Backprop#prod" for a mini-tutorial on 'Prod' and
  -- 'Tuple'
  , Prod(..), Tuple, I(..)
  -- * back-propagation
  , backprop, grad, eval
  -- * Var manipulation
  , BP.constVar, BP.liftB, (BP..$), BP.liftB1, BP.liftB2, BP.liftB3
  -- ** As Parts
  , partsVar, withParts
  , splitVars, gSplit, gTuple
  , partsVar', withParts'
  , splitVars', gSplit'
  -- * Op
  , BP.op1, BP.op2, BP.op3, BP.opN
  , BP.op1', BP.op2', BP.op3'
  -- * Utility
  , pattern (:>), only, head'
  , pattern (::<), only_
  -- ** Numeric Ops
  -- | Optimized ops for numeric functions.  See
  -- "Numeric.Backprop.Op#numops" for more information.
  , (+.), (-.), (*.), negateOp, absOp, signumOp
  , (/.), recipOp
  , expOp, logOp, sqrtOp, (**.), logBaseOp
  , sinOp, cosOp, tanOp, asinOp, acosOp, atanOp
  , sinhOp, coshOp, tanhOp, asinhOp, acoshOp, atanhOp
  ) where

import           Data.Type.Combinator
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Util
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Extras
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Generics.SOP              as SOP
import qualified Numeric.Backprop          as BP

-- | An operation on 'BVar's that can be backpropagated. A value of type:
--
-- @
-- 'BPOp' rs a
-- @
--
-- takes a bunch of 'BVar's containg @rs@ and uses them to (purely) produce
-- a 'BVar' containing an @a@.
--
-- @
-- foo :: 'BPOp' '[ Double, Double ] Double
-- foo (x ':<' y ':<' 'Ø') = x + sqrt y
-- @
--
-- 'BPOp' here is related to 'Numeric.Backprop.BPOpI' from the normal
-- explicit-graph backprop module "Numeric.Backprop".
type BPOp rs a = forall s. Prod (BVar s rs) rs -> BVar s rs a

-- | Run back-propagation on a 'BPOp' function, getting both the result and
-- the gradient of the result with respect to the inputs.
--
-- @
-- foo :: 'BPOp' '[Double, Double] Double
-- foo (x :< y :< Ø) =
--   let z = x * sqrt y
--   in  z + x ** y
-- @
--
-- >>> 'backprop' foo (2 ::< 3 ::< Ø)
-- (11.46, 13.73 ::< 6.12 ::< Ø)
backprop
    :: Every Num rs
    => BPOp rs a
    -> Tuple rs
    -> (a, Tuple rs)
backprop f xs = BP.backprop (BP.withInps' (prodLength xs) (return . f)) xs

-- | Run the 'BPOp' on an input tuple and return the gradient of the result
-- with respect to the input tuple.
--
-- @
-- foo :: 'BPOp' '[Double, Double] Double
-- foo (x :< y :< Ø) =
--   let z = x * sqrt y
--   in  z + x ** y
-- @
--
-- >>> grad foo (2 ::< 3 ::< Ø)
-- 13.73 ::< 6.12 ::< Ø
grad
    :: Every Num rs
    => BPOp rs a
    -> Tuple rs
    -> Tuple rs
grad f = snd . backprop f

-- | Simply run the 'BPOp' on an input tuple, getting the result without
-- bothering with the gradient or with back-propagation.
--
-- @
-- foo :: 'BPOp' '[Double, Double] Double
-- foo (x :< y :< Ø) =
--   let z = x * sqrt y
--   in  z + x ** y
-- @
--
-- >>> eval foo (2 ::< 3 ::< Ø)
-- 11.46
eval
    :: (Known Length rs, Num a)
    => BPOp rs a
    -> Tuple rs
    -> a
eval f = BP.evalBPOp $ BP.implicitly f

-- | A version of 'partsVar' taking explicit 'Length', indicating the
-- number of items in the input tuple and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the internal tuple.  If you ever actually explicitly write down @bs@ as
-- a list of types, you should be able to just use 'partsVar'.
partsVar'
    :: forall s rs bs a. Every Num bs
    => Length bs
    -> Iso' a (Tuple bs)
    -> BVar s rs a
    -> Prod (BVar s rs) bs
partsVar' l i r = map1 (\ix -> every @_ @Num ix //
                                 BP.liftB1 (BP.op1' (f ix)) r
                       ) ixes
  where
    f :: Num b
      => Index bs b
      -> a
      -> (b, Maybe b -> a)
    f ix x = ( getI . index ix . view i $ x
             , review i
             . flip (set (indexP ix)) zeroes
             . maybe (I 1) I
             )
    zeroes :: Tuple bs
    zeroes = map1 (\ix -> I 0 \\ every @_ @Num ix) ixes
    ixes :: Prod (Index bs) bs
    ixes = indices' l

-- | Use an 'Iso' (or compatible 'Control.Lens.Iso.Iso' from the lens
-- library) to "pull out" the parts of a data type and work with each part
-- as a 'BVar'.
--
-- If there is an isomorphism between a @b@ and a @'Tuple' as@ (that is, if
-- an @a@ is just a container for a bunch of @as@), then it lets you break
-- out the @as@ inside and work with those.
--
-- @
-- data Foo = F Int Bool
--
-- fooIso :: 'Iso'' Foo (Tuple '[Int, Bool])
-- fooIso = 'iso' (\\(F i b)         -\> i ::\< b ::\< Ø)
--              (\\(i ::\< b ::\< Ø) -\> F i b        )
--
-- 'partsVar' fooIso :: 'BVar' rs Foo -> 'Prod' ('BVar' s rs) '[Int, Bool]
--
-- stuff :: 'BPOp' s '[Foo] a
-- stuff (foo :< Ø) =
--     case 'partsVar' fooIso foo of
--       i :< b :< Ø ->
--         -- now, i is a 'BVar' pointing to the 'Int' inside foo
--         -- and b is a 'BVar' pointing to the 'Bool' inside foo
--         -- you can do stuff with the i and b here
-- @
--
-- You can use this to pass in product types as the environment to a 'BP',
-- and then break out the type into its constituent products.
--
-- Note that for a type like @Foo@, @fooIso@ can be generated automatically
-- with 'GHC.Generics.Generic' from "GHC.Generics" and
-- 'Generics.SOP.Generic' from "Generics.SOP" and /generics-sop/, using the
-- 'gTuple' iso.  See 'gSplit' for more information.
--
-- Also, if you are literally passing a tuple (like
-- @'BP' s '[Tuple '[Int, Bool]@) then you can give in the identity
-- isomorphism ('id') or use 'splitVars'.
--
-- At the moment, this implicit 'partsVar' is less efficient than the
-- explicit 'Numeric.Backprop.partsVar', but this might change in the
-- future.
partsVar
    :: forall s rs bs a. (Every Num bs, Known Length bs)
    => Iso' a (Tuple bs)
    -> BVar s rs a
    -> Prod (BVar s rs) bs
partsVar = partsVar' known

-- | A version of 'withParts' taking explicit 'Length', indicating the
-- number of internal items and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the internal tuple.  If you ever actually explicitly write down @bs@ as
-- a list of types, you should be able to just use 'withParts'.
withParts'
    :: forall s rs bs a r. Every Num bs
    => Length bs
    -> Iso' a (Tuple bs)
    -> BVar s rs a
    -> (Prod (BVar s rs) bs -> r)
    -> r
withParts' l i r f = f (partsVar' l i r)

-- | A continuation-based version of 'partsVar'.  Instead of binding the
-- parts and using it in the rest of the block, provide a continuation to
-- handle do stuff with the parts inside.
--
-- Building on the example from 'partsVar':
--
-- @
-- data Foo = F Int Bool
--
-- fooIso :: 'Iso'' Foo (Tuple '[Int, Bool])
-- fooIso = 'iso' (\\(F i b)         -\> i ::\< b ::\< Ø)
--              (\\(i ::\< b ::\< Ø) -\> F i b        )
--
-- stuff :: 'BPOp' s '[Foo] a
-- stuff (foo :< Ø) = 'withParts' fooIso foo $ \\case
--     i :\< b :< Ø -\>
--       -- now, i is a 'BVar' pointing to the 'Int' inside foo
--       -- and b is a 'BVar' pointing to the 'Bool' inside foo
--       -- you can do stuff with the i and b here
-- @
--
-- Mostly just a stylistic alternative to 'partsVar'.
withParts
    :: forall s rs bs a r. (Every Num bs, Known Length bs)
    => Iso' a (Tuple bs)
    -> BVar s rs a
    -> (Prod (BVar s rs) bs -> r)
    -> r
withParts = withParts' known

-- | A version of 'splitVars' taking explicit 'Length', indicating the
-- number of internal items and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the internal tuple.  If you ever actually explicitly write down @as@ as
-- a list of types, you should be able to just use 'splitVars'.
splitVars'
    :: forall s rs as. Every Num as
    => Length as
    -> BVar s rs (Tuple as)
    -> Prod (BVar s rs) as
splitVars' l = partsVar' l id

-- | Split out a 'BVar' of a tuple into a tuple ('Prod') of 'BVar's.
--
-- @
-- -- the environment is a single Int-Bool tuple, tup
-- stuff :: 'BPOp' s '[ Tuple '[Int, Bool] ] a
-- stuff (tup :< Ø) =
--   case 'splitVar' tup of
--     i :< b :< Ø <- 'splitVars' tup
--     -- now, i is a 'BVar' pointing to the 'Int' inside tup
--     -- and b is a 'BVar' pointing to the 'Bool' inside tup
--     -- you can do stuff with the i and b here
-- @
--
-- Note that
--
-- @
-- 'splitVars' = 'partsVar' 'id'
-- @
splitVars
    :: forall s rs as. (Every Num as, Known Length as)
    => BVar s rs (Tuple as)
    -> Prod (BVar s rs) as
splitVars = splitVars' known

-- | A version of 'gSplit' taking explicit 'Length', indicating the
-- number of internal items and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the internal tuple.  If you ever actually explicitly write down @as@ as
-- a list of types, you should be able to just use 'gSplit'.
gSplit'
    :: forall s rs as a. (SOP.Generic a, SOP.Code a ~ '[as], Every Num as)
    => Length as
    -> BVar s rs a
    -> Prod (BVar s rs) as
gSplit' l = partsVar' l gTuple

-- | Using 'GHC.Generics.Generic' from "GHC.Generics" and
-- 'Generics.SOP.Generic' from "Generics.SOP", /split/ a 'BVar' containing
-- a product type into a tuple ('Prod') of 'BVar's pointing to each value
-- inside.
--
-- Building on the example from 'partsVar':
--
-- @
-- import qualified Generics.SOP as SOP
--
-- data Foo = F Int Bool
--   deriving Generic
--
-- instance SOP.Generic Foo
--
-- 'gSplit' :: 'BVar' rs Foo -> 'Prod' ('BVar' s rs) '[Int, Bool]
--
-- stuff :: 'BPOp' s '[Foo] a
-- stuff (foo :< Ø) =
--     case 'gSplit' foo of
--       i :< b :< Ø ->
--         -- now, i is a 'BVar' pointing to the 'Int' inside foo
--         -- and b is a 'BVar' pointing to the 'Bool' inside foo
--         -- you can do stuff with the i and b here
-- @
--
-- Because @Foo@ is a straight up product type, 'gSplit' can use
-- "GHC.Generics" and take out the items inside.
--
-- Note that
--
-- @
-- 'gSplit' = 'splitVars' 'gTuple'
-- @
gSplit
    :: forall s rs as a. (SOP.Generic a, SOP.Code a ~ '[as], Every Num as, Known Length as)
    => BVar s rs a
    -> Prod (BVar s rs) as
gSplit = gSplit' known

-- TODO: figure out how to split sums
-- TODO: refactor these out to not need Known Length
