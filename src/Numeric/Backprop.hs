{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Backprop (
  -- * Types
  -- ** Backprop types
    BP, BPOp, BPOpI, BVar, Op, OpB
  -- ** Tuple types
  , Prod(..), Tuple
  -- * BP
  -- ** Backprop
  , backprop, evalBPOp, gradBPOp
  , backprop', gradBPOp'
  -- ** Utility combinators
  , withInps, implicitly
  , withInps', implicitly'
  -- * Vars
  , constVar
  , inpVar, inpVars
  , bpOp
  , bindVar
  , inpVars'
  , bpOp'
  , bindVar'
  -- ** From Ops
  , opVar, (~$)
  , opVar1, opVar2, opVar3
  , (-$)
  , opVar'
  , opVar1', opVar2', opVar3'
  -- ** Var manipulation
  -- *** As parts
  , partsVar, (#<~), withParts
  , splitVars, gSplit, gTuple
  , partsVar', withParts'
  , splitVars', gSplit'
  -- *** As sums
  , choicesVar, (?<~), withChoices
  , choicesVar', withChoices'
  -- *** As sums of products
  , sopVar, gSplits, gSOP
  , sopVar', gSplits'
  -- ** Combining
  , liftB, (.$), liftB1, liftB2, liftB3
  -- * Op
  , op1, op2, op3, opN
  , op1', op2', op3'
  -- * Utility
  , pattern (:>), only, head'
  , pattern (::<), only_
  , Summer(..), Unity(..)
  , summers, unities
  , summers', unities'
  ) where

import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.STRef
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Product
import           Data.Type.Sum hiding      (index)
import           Data.Type.Util
import           Lens.Micro hiding         (ix)
import           Lens.Micro.Mtl hiding     (view)
import           Numeric.Backprop.Internal
import           Numeric.Backprop.Iso
import           Numeric.Backprop.Op
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import qualified Generics.SOP              as SOP

-- | A handy type synonym representing a 'BP' action that returns a 'BVar'.
-- This is handy because this is the form of 'BP' actions that
-- 'backprop' and 'gradBPOp' (etc.) expects.
--
-- A value of type:
--
-- @
-- 'BPOp' s rs a
-- @
--
-- is an action that takes an input environment of @rs@ and produces
-- a 'BVar' containing a value of type @a@.  Because it returns a 'BVar',
-- the library can track the data dependencies between the 'BVar' and the
-- input environment and perform backpropagation.
--
-- See documentation for 'BP' for an explanation of the phantom type
-- parameter @s@.
type BPOp s rs a  = BP s rs (BVar s rs a)

-- | An "implicit" operation on 'BVar's that can be backpropagated.
-- A value of type:
--
-- @
-- 'BPOpI' s rs a
-- @
--
-- takes a bunch of 'BVar's containg @rs@ and uses them to (purely) produce
-- a 'BVar' containing an @a@.
--
-- @
-- foo :: BPOpI s '[ Double, Double ] Float
-- foo (x :< y :< Ø) = x + sqrt y
-- @
--
-- If you are exclusively doing implicit backpropagation by combining
-- 'BVar's and using 'BPOpI's, you are probably better off just importing
-- "Numeric.Backprop.Implicit", which provides better tools.  This type
-- synonym exists in "Numeric.Backprop" just for the 'implicitly' function,
-- which can convert "implicit" backprop functions like a @'BPOpI' s rs a@
-- into an "explicit" graph backprop function, a @'BPOp' s rs a@.
type BPOpI s rs a = Prod (BVar s rs) rs -> BVar s rs a


-- | A version of 'opVar' taking an explicit 'Summer', so can be used on
-- values of types that aren't instances of 'Num'.
opVar'
    :: forall s rs as a. ()
    => Summer a
    -> OpB s as a
    -> Prod (BVar s rs) as
    -> BP s rs (BVar s rs a)
opVar' s o i = do
    xs <- traverse1 (fmap I . BP . resolveVar) i
    (res, gf) <- BP . liftBase $ runOpM' o xs
    let bp = BPN { _bpnOut       = only $ FRInternal []
                 , _bpnRes       = only_ res
                 , _bpnGradFunc  = gf . head'
                 , _bpnGradCache = Nothing
                 , _bpnSummer    = only s
                 }
    r <- BP . liftBase $ newSTRef bp
    itraverse1_ (registerVar . flip IRNode r) i
    return (BVNode IZ r)

-- | A version of 'splitVars' taking explicit 'Summer's and 'Unity's, so it
-- can be run with types that aren't instances of 'Num'.
splitVars'
    :: forall s rs as. ()
    => Prod Summer as
    -> Prod Unity as
    -> BVar s rs (Tuple as)
    -> BP s rs (Prod (BVar s rs) as)
splitVars' ss us = partsVar' ss us id

-- | Split out a 'BVar' of a tuple into a tuple ('Prod') of 'BVar's.
--
-- @
-- -- the environment is a single Int-Bool tuple, tup
-- stuff :: 'BP' s '[ Tuple '[Int, Bool] ] a
-- stuff = 'withInps' $ \\(tup :< Ø) -\> do
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
    -> BP s rs (Prod (BVar s rs) as)
splitVars = partsVar id

-- | A version of 'partsVar' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
partsVar'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
partsVar' ss us i =
    fmap (view sum1) . sopVar' (only ss) (only us) (i . resum1)

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
-- 'partsVar' fooIso :: 'BVar' rs Foo -> 'BP' s rs ('Prod' ('BVar' s rs) '[Int, Bool])
--
-- stuff :: 'BP' s '[Foo] a
-- stuff = 'withInps' $ \\(foo :< Ø) -\> do
--     i :< b :< Ø <- partsVar fooIso foo
--     -- now, i is a 'BVar' pointing to the 'Int' inside foo
--     -- and b is a 'BVar' pointing to the 'Bool' inside foo
--     -- you can do stuff with the i and b here
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
partsVar
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
partsVar = partsVar' summers unities

-- | A useful infix alias for 'partsVar'.
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
-- stuff :: 'BP' s '[Foo] a
-- stuff = 'withInps' $ \\(foo :< Ø) -\> do
--     i :< b :< Ø <- fooIso '#<~' foo
--     -- now, i is a 'BVar' pointing to the 'Int' inside foo
--     -- and b is a 'BVar' pointing to the 'Bool' inside foo
--     -- you can do stuff with the i and b here
-- @
--
-- See 'gSplit' for an example usage of splitting up an arbitrary product
-- type (like @Foo@) using "GHC.Geneics" and "Generics.SOP".
infixr 1 #<~
(#<~)
    :: (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
(#<~) = partsVar

-- | A version of 'withParts' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
withParts'
    :: Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Tuple bs)
    -> BVar s rs b
    -> (Prod (BVar s rs) bs -> BP s rs a)
    -> BP s rs a
withParts' ss us i r f = do
    p <- partsVar' ss us i r
    f p

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
-- stuff :: 'BP' s '[Foo] a
-- stuff = 'withInps' $ \\(foo :< Ø) -\> do
--     'withParts' fooIso foo $ \\(i :< b :< Ø) -\> do
--       -- now, i is a 'BVar' pointing to the 'Int' inside foo
--       -- and b is a 'BVar' pointing to the 'Bool' inside foo
--       -- you can do stuff with the i and b here
-- @
--
-- Useful so that you can work with the internal parts of the data type
-- in a closure, so the parts don't leak out to the rest of your 'BP'.
-- But, mostly just a stylistic choice.
withParts
    :: (Every Num bs, Known Length bs)
    => Iso' b (Tuple bs)
    -> BVar s rs b
    -> (Prod (BVar s rs) bs -> BP s rs a)
    -> BP s rs a
withParts i r f = do
    p <- partsVar i r
    f p

-- | A version of 'gSplit' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
gSplit'
    :: (SOP.Generic b, SOP.Code b ~ '[bs])
    => Prod Summer bs
    -> Prod Unity bs
    -> BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
gSplit' ss us = partsVar' ss us gTuple

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
-- 'gSplit' :: 'BVar' rs Foo -> 'BP' s rs ('Prod' ('BVar' s rs) '[Int, Bool])
--
-- stuff :: 'BP' s '[Foo] a
-- stuff = 'withInps' $ \\(foo :< Ø) -\> do
--     i :< b :< Ø <- 'gSplit' foo
--     -- now, i is a 'BVar' pointing to the 'Int' inside foo
--     -- and b is a 'BVar' pointing to the 'Bool' inside foo
--     -- you can do stuff with the i and b here
-- @
--
-- Because @Foo@ is a straight up product type, 'gSplit' can use
-- "GHC.Generics" and take out the items inside.
--
-- Note that because
--
-- @
-- 'gSplit' = 'splitVars' 'gTuple'
-- @
--
-- Then, you can also use 'gTuple' with '#<~':
--
-- @
-- stuff :: 'BP' s '[Foo] a
-- stuff = 'withInps' $ \\(foo :< Ø) -\> do
--     i :< b :< Ø <- 'gTuple' '#<~' foo
--     -- now, i is a 'BVar' pointing to the 'Int' inside foo
--     -- and b is a 'BVar' pointing to the 'Bool' inside foo
--     -- you can do stuff with the i and b here
-- @
--
gSplit
    :: (Every Num bs, Known Length bs, SOP.Generic b, SOP.Code b ~ '[bs])
    => BVar s rs b
    -> BP s rs (Prod (BVar s rs) bs)
gSplit = gSplit' summers unities

-- | A version of 'choicesVar' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
choicesVar'
    :: forall s rs bs b. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Sum I bs)
    -> BVar s rs b
    -> BP s rs (Sum (BVar s rs) bs)
choicesVar' ss us i r = do
    x <- BP $ resolveVar r
    let xs :: Sum I bs
        xs = view i x
    ifor1 ((ss `zipP` us) `tagSum` xs) $ \ix ((s :&: u) :&: I (y :: c)) -> do
      let bp :: BPNode s rs '[b] '[c]
          bp = BPN { _bpnOut       = only $ FRInternal []
                   , _bpnRes       = only_ y
                   , _bpnGradFunc  = return . only_ . review i
                                   . injectSum ix
                                   . maybe (I (getUnity u)) I
                                   . head'
                   , _bpnGradCache = Nothing
                   , _bpnSummer    = only s
                   }
      r' <- BP . liftBase $ newSTRef bp
      registerVar (IRNode IZ r') r
      return $ BVNode IZ r'
-- TODO: cannot implement via sopVar?  oh well.

-- | Use an 'Iso' (or compatible 'Control.Lens.Iso.Iso' from the lens
-- library) to "pull out" the different constructors of a sum type and
-- return a (choice) sum of 'BVar's that you can pattern match on.
--
-- If there is an isomorphism between a @b@ and a @'Sum' 'I' as@ (that is,
-- if an @a@ is just a sum type for every type in @as@), then it lets you
-- /branch/ on which constructor is used inside the @b@.
--
-- Essentially implements pattern matching on 'BVar' values.
--
-- @
-- data Bar = A Int | B Bool | C String
--
-- barIso :: 'Iso'' Bar ('Sum' I '[Int, Bool, String])
-- barIso = 'iso' (\\case A i -> 'InL' (I i)
--                       B b -> 'InR' ('InL' (I b))
--                       C s -> 'InR' ('InR' ('InL' (I s))
--                )
--                (\\case 'InL' (I i)           -> A i
--                       'InR' ('InL' (I b))       -> B b
--                       'InR' ('InR' ('InL' (I s))) -> C s
--                )
--
-- choicesVar barIso :: BVar rs Bar -> BP s rs (Sum I (BVar s rs) '[Int, Bool, String])
--
-- stuff :: 'BP' s '[Bar] a
-- stuff = 'withInps' $ \\(bar :< Ø) -\> do
--     c <- 'choicesVar' barIso bar
--     case c of
--       'InL' i -> do
--          -- in this branch, bar was made with the A constructor
--          -- i is the Int inside it
--       'InR' ('InL' b) -> do
--          -- in this branch, bar was made with the B constructor
--          -- b is the Bool inside it
--       'InR' ('InR' ('InL' s)) -> do
--          -- in this branch, bar was made with the B constructor
--          -- s is the String inside it
-- @
--
-- You can use this to pass in sum types as the environment to a 'BP', and
-- then branch on which constructor the value was made with.
choicesVar
    :: forall s rs bs b. (Every Num bs, Known Length bs)
    => Iso' b (Sum I bs)
    -> BVar s rs b
    -> BP s rs (Sum (BVar s rs) bs)
choicesVar = choicesVar' summers unities

-- | A version of 'withChoices' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
withChoices'
    :: forall s rs bs b a. ()
    => Prod Summer bs
    -> Prod Unity bs
    -> Iso' b (Sum I bs)
    -> BVar s rs b
    -> (Sum (BVar s rs) bs -> BP s rs a)
    -> BP s rs a
withChoices' ss us i r f = do
    c <- choicesVar' ss us i r
    f c

-- | A continuation-based version of 'choicesVar'.  Instead of binding the
-- parts and using it in the rest of the block, provide a continuation that
-- will handle every possible constructor/case of the type of the value the
-- 'BVar' points to.
--
-- Building on the example from 'choicesVar':
--
-- @
-- data Bar = A Int | B Bool | C String
--
-- barIso :: 'Iso'' Bar ('Sum' I '[Int, Bool, String])
-- barIso = 'iso' (\\case A i -> 'InL' (I i)
--                       B b -> 'InR' ('InL' (I b))
--                       C s -> 'InR' ('InR' ('InL' (I s))
--                )
--                (\\case 'InL' (I i)           -> A i
--                       'InR' ('InL' (I b))       -> B b
--                       'InR' ('InR' ('InL' (I s))) -> C s
--                )
--
-- 'choicesVar' barIso :: BVar rs Bar -> BP s rs (Sum I (BVar s rs) '[Int, Bool, String])
--
-- stuff :: 'BP' s '[Bar] a
-- stuff = 'withInps' $ \\(bar :< Ø) -\> do
--     'withChoices' barIso bar $ \case
--       'InL' i -> do
--          -- in this branch, bar was made with the A constructor
--          -- i is the Int inside it
--       'InR' ('InL' b) -> do
--          -- in this branch, bar was made with the B constructor
--          -- b is the Bool inside it
--       'InR' ('InR' ('InL' s)) -> do
--          -- in this branch, bar was made with the B constructor
--          -- s is the String inside it
-- @
--
-- Nicer than 'choicesVar' directly, because you don't have to give the
-- result a superfluous name before pattern matching on it.  You can just
-- directly pattern match in the lambda, so there's a lot less syntactical
-- noise.
withChoices
    :: forall s rs bs b a. (Every Num bs, Known Length bs)
    => Iso' b (Sum I bs)
    -> BVar s rs b
    -> (Sum (BVar s rs) bs -> BP s rs a)
    -> BP s rs a
withChoices i r f = do
    c <- choicesVar i r
    f c

-- | A useful infix alias for 'choicesVar'.
--
-- Building on the example from 'choicesVar':
--
-- @
-- data Bar = A Int | B Bool | C String
--
-- barIso :: 'Iso'' Bar ('Sum' I '[Int, Bool, String])
-- barIso = 'iso' (\\case A i -> 'InL' (I i)
--                       B b -> 'InR' ('InL' (I b))
--                       C s -> 'InR' ('InR' ('InL' (I s))
--                )
--                (\\case 'InL' (I i)           -> A i
--                       'InR' ('InL' (I b))       -> B b
--                       'InR' ('InR' ('InL' (I s))) -> C s
--                )
--
-- stuff :: 'BP' s '[Bar] a
-- stuff = 'withInps' $ \\(bar :< Ø) -\> do
--     c <- barIso '?<~' bar
--     case c of
--       'InL' i -> do
--          -- in this branch, bar was made with the A constructor
--          -- i is the Int inside it
--       'InR' ('InL' b) -> do
--          -- in this branch, bar was made with the B constructor
--          -- b is the Bool inside it
--       'InR' ('InR' ('InL' s)) -> do
--          -- in this branch, bar was made with the B constructor
--          -- s is the String inside it
-- @
infixr 1 ?<~
(?<~)
    :: (Every Num bs, Known Length bs)
    => Iso' b (Sum I bs)
    -> BVar s rs b
    -> BP s rs (Sum (BVar s rs) bs)
(?<~) = choicesVar

-- | A version of 'sopVar' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
sopVar'
    :: forall s rs bss b. ()
    => Prod (Prod Summer) bss
    -> Prod (Prod Unity) bss
    -> Iso' b (Sum Tuple bss)
    -> BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) bss)
sopVar' sss uss i r = do
    x <- BP $ resolveVar r
    let xs :: Sum Tuple bss
        xs = view i x
    ifor1 ((sss `zipP` uss) `tagSum` xs) $ \ix ((ss :&: us) :&: (ys :: Tuple bs)) -> do
      let bp :: BPNode s rs '[b] bs
          bp = BPN { _bpnOut       = map1 (const (FRInternal [])) ys
                   , _bpnRes       = ys
                   , _bpnGradFunc  = return . only_
                                   . review i . injectSum ix
                                   . map1 (uncurryFan $ \u ->
                                             maybe (I (getUnity u)) I
                                          )
                                   . zipP us
                   , _bpnGradCache = Nothing
                   , _bpnSummer    = ss
                   }
      r' <- BP . liftBase $ newSTRef bp
      registerVar (IRNode IZ r') r
      return $ imap1 (\ix' _ -> BVNode ix' r') ys

-- | A combination of 'partsVar' and 'choicesVar', that lets you split
-- a type into a sum of products.  Using an 'Iso' (or compatible
-- 'Control.Lens.Iso.Iso' from the lens library), you can pull out a type
-- that is a sum of products into a sum of products of 'BVar's.
--
-- Implements branching on the constructors of a value that a 'BVar'
-- contains, and also splitting out the different items inside each
-- constructor.
--
-- @
-- data Baz = A Int    Bool
--          | B String Double
--
--
-- bazIso :: 'Iso'' Baz ('Sum' 'Tuple' '[ '[Int, Bool], '[String, Double] ])
-- bazIso = 'iso' (\\case A i b -> 'InL' (I (i ::< b ::< Ø))
--                       B s d -> 'InR' ('InL' (I (s ::< d ::< Ø)))
--                )
--                (\\case 'InL' (I (i ::< b ::< Ø))     -> A i b
--                       'InR' ('InL' (I (s ::< d ::< Ø))) -> B s d
--                )
--
-- 'sopVar' bazIso :: 'BVar' rs Baz -> 'BP' s rs ('Sum' ('Prod' ('BVar' s rs)) '[ '[Int, Bool], '[String, Double] ])
--
-- stuff :: 'BP' s '[Baz] a
-- stuff = 'withInps' $ \\(baz :< Ø) -\> do
--     c <- 'sopVar' barIso baz
--     case c of
--       'InL' (i :< b :< Ø) -> do
--          -- in this branch, baz was made with the A constructor
--          -- i and b are the Int and Bool inside it
--       'InR' ('InL' (s :< d :< Ø)) -> do
--          -- in this branch, baz was made with the B constructor
--          -- s and d are the String and Double inside it
-- @
--
-- Essentially exists to implement "pattern matching" on multiple
-- constructors and fields for the value inside a 'BVar'.
--
-- Note that for a type like @Baz@, @bazIso@ can be generated automatically
-- with 'GHC.Generics.Generic' from "GHC.Generics" and
-- 'Generics.SOP.Generic' from "Generics.SOP" and /generics-sop/, with
-- 'gSOP'.  See 'gSplits' for more information.
sopVar
    :: forall s rs bss b. (Known Length bss, Every (Every Num ∧ Known Length) bss)
    => Iso' b (Sum Tuple bss)
    -> BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) bss)
sopVar = sopVar' (withEvery @(Every Num ∧ Known Length) summers)
                 (withEvery @(Every Num ∧ Known Length) unities)

-- | A version of 'gSplits' taking explicit 'Summer's and 'Unity's, so it
-- can be run with internal types that aren't instances of 'Num'.
gSplits'
    :: forall s rs b. SOP.Generic b
    => Prod (Prod Summer) (SOP.Code b)
    -> Prod (Prod Unity) (SOP.Code b)
    -> BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) (SOP.Code b))
gSplits' sss uss = sopVar' sss uss gSOP

-- | Using 'GHC.Generics.Generic' from "GHC.Generics" and
-- 'Generics.SOP.Generic' from "Generics.SOP", /split/ a 'BVar' containing
-- a sum of products (any simple ADT, essentialy) into a 'Sum' of each
-- constructor, each containing a tuple ('Prod') of 'BVar's pointing to
-- each value inside.
--
-- Building on the example from 'sopVar':
--
-- @
-- import qualified Generics.SOP as SOP
-- 
-- data Baz = A Int    Bool
--          | B String Double
--   deriving Generic
--
-- instance SOP.Generic Baz
--
-- 'gSplits' :: 'BVar' rs Baz -> 'BP' s rs ('Sum' ('Prod' ('BVar' s rs)) '[ '[Int, Bool], '[String, Double] ])
--
-- stuff :: 'BP' s '[Baz] a
-- stuff = 'withInps' $ \\(baz :< Ø) -\> do
--     c <- gSplits baz
--     case c of
--       'InL' (i :< b :< Ø) -> do
--          -- in this branch, baz was made with the A constructor
--          -- i and b are the Int and Bool inside it
--       'InR' ('InL' (s :< d :< Ø)) -> do
--          -- in this branch, baz was made with the B constructor
--          -- s and d are the String and Double inside it
-- @
--
-- Because @Foo@ is a straight up sum-of-products type, 'gSplits' can use
-- "GHC.Generics" and take out the items inside.
--
-- Note:
--
-- @
-- 'gSplit' = 'splitVars' 'gSOP'
-- @
gSplits
    :: forall s rs b.
      ( SOP.Generic b
      , Known Length (SOP.Code b)
      , Every (Every Num ∧ Known Length) (SOP.Code b)
      )
    => BVar s rs b
    -> BP s rs (Sum (Prod (BVar s rs)) (SOP.Code b))
gSplits = sopVar gSOP


resolveVar
    :: (MonadReader (Tuple rs) m, MonadBase (ST s) m)
    => BVar s rs a
    -> m a
resolveVar = \case
    BVNode  ix r -> getI . index ix . _bpnRes <$> liftBase (readSTRef r)
    BVInp   ix   -> getI . index ix <$> ask
    BVConst    x -> return x
    BVOp    rs o -> do
      xs <- traverse1 (fmap I . resolveVar) rs
      liftBase $ runOpM o xs

registerVar
    :: forall s rs a. ()
    => BPInpRef s rs a
    -> BVar s rs a
    -> BP s rs ()
registerVar bpir = \case
    BVNode  ix' r' -> BP . liftBase . modifySTRef r' $
                        over (bpnOut . indexP ix' . _FRInternal) (bpir :)
    BVInp   ix'    -> BP $ modifying (bpsSources . indexP ix' . _FRInternal) (bpir :)
    BVConst _      -> return ()
    -- This independently makes a new BPPipe for every usage site of the
    -- BVOp, so it's a bit inefficient.
    BVOp    (rs :: Prod (BVar s rs) ds) (o :: OpM (ST s) ds a) -> do
      xs :: Tuple ds <- traverse1 (fmap I . BP . resolveVar) rs
      (res, gF) <- BP . liftBase $ runOpM' o xs
      let bpp :: BPPipe s rs ds '[a]
          bpp = BPP { _bppOut       = only bpir
                    , _bppRes       = only_ res
                    , _bppGradFunc  = gF . Just . getI . head'
                    , _bppGradCache = Nothing
                    }
      r' <- BP . liftBase $ newSTRef bpp
      ifor1_ rs $ \ix' (bpr :: BVar s rs d) ->
        registerVar (IRPipe ix' r') bpr

-- | Apply an 'OpB' to a 'Prod' (tupling) of 'BVar's.
--
-- If you had an @'OpB' s '[a, b, c] d@, this function will expect a 3-Prod
-- of a @'BVar' s rs a@, a @'BVar' s rs b@, and a @'BVar' s rs c@, and the
-- result will be a @'BVar' s rs d@:
--
-- @
-- myOp :: 'OpB' s '[a, b, c] d
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
-- z    :: 'BVar' s rs c
--
-- x :< y :< z :< Ø              :: 'Prod' ('BVar' s rs) '[a, b, c]
-- 'opVar' myOp (x :< y :< z :< Ø) :: 'BP' s rs ('BVar' s rs d)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can provide any 'Op'
-- here, as well (like those created by 'op1', 'op2', 'constOp', 'op0'
-- etc.)
--
-- 'opVar' has an infix alias, '~$', so the above example can also be
-- written as:
--
-- @
-- myOp '~$' (x :< y :< z :< Ø) :: 'BP' s rs ('BVar' s rs d)
-- @
--
-- to let you pretend that you're applying the 'myOp' function to three
-- inputs.
--
-- Also note the relation between 'opVar' and 'liftB' and 'bindVar':
--
-- @
-- 'opVar' o xs = 'bindVar' ('liftB' o xs)
-- @
--
-- 'opVar' can be thought of as a "binding" version of 'liftB'.
opVar
    :: Num a
    => OpB s as a
    -> Prod (BVar s rs) as
    -> BP s rs (BVar s rs a)
opVar = opVar' known

-- | Infix synonym for 'opVar', which lets you pretend that you're applying
-- 'OpB's as if they were functions:
--
-- @
-- myOp :: 'OpB' s '[a, b, c] d
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
-- z    :: 'BVar' s rs c
--
-- x :< y :< z :< Ø           :: 'Prod' ('BVar' s rs) '[a, b, c]
-- myOp '~$' (x :< y :< z :< Ø) :: 'BP' s rs ('BVar' s rs d)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in any 'Op'
-- here, as well (like those created by 'op1', 'op2', 'constOp', 'op0'
-- etc.)
--
-- '~$' can also be thought of as a "binding" version of '.$':
--
-- @
-- o '~$' xs = 'bindVar' (o '.$' xs)
-- @
--
infixr 1 ~$
(~$)
    :: Num a
    => OpB s as a
    -> Prod (BVar s rs) as
    -> BP s rs (BVar s rs a)
(~$) = opVar

-- | Lets you treat a @'BPOp s as b@ as an @'Op' as b@, and "apply"
-- arguments to it just like you would with an 'Op' and '~$' / 'opVar'.
--
-- Basically a convenient wrapper over 'bpOp':
--
-- @
-- o '-$' xs = bpOp o '~$' xs
-- @
--
-- So for a @'BPOp' s as b@, you can "plug in" 'BVar's to @as@, and get
-- a @b@ as a result.
--
-- Useful for running a @'BPOp' s rs a@ that you got from a different function, and
-- "plugging in" its @rs@ inputs with 'BVar's from your current
-- environment.
infixr 1 -$
(-$)
    :: (Every Num as, Known Length as, Num a)
    => BPOp s as a
    -> Prod (BVar s rs) as
    -> BPOp s rs a
o -$ xs = bpOp o ~$ xs

-- | Create a 'BVar' that represents just a specific value, that doesn't
-- depend on any other 'BVar's.
constVar :: a -> BVar s rs a
constVar = BVConst

-- | A version of 'opVar1' taking an explicit 'Summer', so can be used on
-- values of types that aren't instances of 'Num'.
opVar1'
    :: Summer b
    -> OpB s '[a] b
    -> BVar s rs a
    -> BP s rs (BVar s rs b)
opVar1' s o = opVar' s o . only

-- | Convenient wrapper over 'opVar' that takes an 'OpB' with one argument
-- and a single 'BVar' argument.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'opVar1' o x = 'opVar' o (x ':<' 'Ø')
--
-- myOp :: 'Op' '[a] b
-- x    :: 'BVar' s rs a
--
-- 'opVar1' myOp x :: 'BP' s rs ('BVar' s rs b)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op1') as well.
opVar1
    :: Num b
    => OpB s '[a] b
    -> BVar s rs a
    -> BP s rs (BVar s rs b)
opVar1 = opVar1' known

-- | A version of 'opVar2' taking an explicit 'Summer', so can be used on
-- values of types that aren't instances of 'Num'.
opVar2'
    :: Summer c
    -> OpB s '[a,b] c
    -> BVar s rs a
    -> BVar s rs b
    -> BP s rs (BVar s rs c)
opVar2' s o rx ry = opVar' s o (rx :< ry :< Ø)

-- | Convenient wrapper over 'opVar' that takes an 'OpB' with two arguments
-- and two 'BVar' arguments.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'opVar2' o x y = 'opVar' o (x ':<' y ':<' 'Ø')
--
-- myOp :: 'Op' '[a, b] c
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
--
-- 'opVar2' myOp x y :: 'BP' s rs ('BVar' s rs c)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op2') as well.
opVar2
    :: Num c
    => OpB s '[a,b] c
    -> BVar s rs a
    -> BVar s rs b
    -> BP s rs (BVar s rs c)
opVar2 = opVar2' known

-- | A version of 'opVar3' taking an explicit 'Summer', so can be used on
-- values of types that aren't instances of 'Num'.
opVar3'
    :: Summer d
    -> OpB s '[a,b,c] d
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
    -> BP s rs (BVar s rs d)
opVar3' s o rx ry rz = opVar' s o (rx :< ry :< rz :< Ø)

-- | Convenient wrapper over 'opVar' that takes an 'OpB' with three arguments
-- and three 'BVar' arguments.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'opVar3' o x y z = 'opVar' o (x ':<' y ':<' z ':<' 'Ø')
--
-- myOp :: 'Op' '[a, b, c] d
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
-- z    :: 'BVar' s rs c
--
-- 'opVar3' myOp x y z :: 'BP' s rs ('BVar' s rs d)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op3') as well.
opVar3
    :: Num d
    => OpB s '[a,b,c] d
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
    -> BP s rs (BVar s rs d)
opVar3 = opVar3' known

-- | A version of 'bindVar' that requires an explicit 'Summer', so that you
-- can use it on values whose types aren't instances of 'Num'.
bindVar'
    :: Summer a
    -> BVar s rs a
    -> BP s rs (BVar s rs a)
bindVar' s r = case r of
    BVNode  _  _ -> return r
    BVInp   _    -> return r
    BVConst _    -> return r
    BVOp    rs o -> opVar' s o rs

-- | Concretizes a delayed 'BVar'.  If you build up a 'BVar' using numeric
-- functions like '+' or '*' or using 'liftR', it'll defer the evaluation,
-- and all of its usage sites will create a separate graph node.
--
-- Use 'bindVar' if you ever intend to use a 'BVar' in more than one
-- location.
--
-- @
-- -- bad
-- errSquared :: Num a => 'BP' s '[a, a] a
-- errSquared = 'withInp' $ \\(r :< t :< Ø) -\> do
--     let err = r - t
--     'return' (err * err)   -- err is used twice!
--
-- -- good
-- errSquared :: Num a => 'BP' s '[a, a] a
-- errSquared = 'withInps' $ \\(r :< t :< Ø) -\> do
--     let err = r - t
--     e <- 'bindVar' err     -- force e, so that it's safe to use twice!
--     'return' (e * e)
--
-- -- better
-- errSquared :: Num a => 'BP' s '[a, a] a
-- errSquared = 'withInps' $ \\(r :< t :< Ø) -\> do
--     let err = r - t
--     e <- 'bindVar' err
--     'bindVar' (e * e)      -- result is forced so user doesn't have to worry
-- @
--
-- Note the relation to 'opVar' / '~$' / 'liftR' / '.$':
--
-- @
-- 'opVar' o xs    = 'bindVar' ('liftR' o xs)
-- o '~$' xs       = 'bindVar' (o '.$' xs)
-- 'op2' (*) '~$' (x :< y :< Ø) = 'bindVar' (x * y)
-- @
--
-- So you can avoid 'bindVar' altogether if you use the explicitly binding
-- '~$' and 'opVar' etc.
--
-- Note that 'bindVar' on 'BVar's that are already forced is a no-op.
bindVar
    :: Num a
    => BVar s rs a
    -> BP s rs (BVar s rs a)
bindVar = bindVar' known



backwardPass
    :: forall s rs a. ()
    => BPInpRef s rs a
    -> ST s a
backwardPass = \case
    IRNode  ix r' -> getI . index ix <$> pullNode r'
    IRPipe  ix r' -> getI . index ix <$> pullPipe r'
    IRConst g     -> return g
  where
    pullNode
        :: forall as bs. ()
        => STRef s (BPNode s rs as bs)
        -> ST s (Tuple as)
    pullNode r = caching bpnGradCache r $ \BPN{..} -> do
        totdervs <- for1 (_bpnSummer `zipP` _bpnOut) $ \case
          s :&: FRInternal rs -> Just . runSummer s
              <$> traverse backwardPass rs
          _ :&: FRTerminal g   -> return g
        g <- _bpnGradFunc totdervs
        return g
    pullPipe
        :: forall as bs. ()
        => STRef s (BPPipe s rs as bs)
        -> ST s (Tuple as)
    pullPipe r = caching bppGradCache r $ \BPP{..} ->
        _bppGradFunc =<< traverse1 (fmap I . backwardPass) _bppOut

-- | A version of 'backprop' taking explicit 'Summer's and 'Unity's, so it
-- can be run with types that aren't instances of 'Num'.
backprop'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop' ss us bp env = runST $ do
    (res, gFunc) <- backpropWith ss us bp env
    grad <- gFunc Nothing
    return (res, grad)

-- | Perform backpropagation on the given 'BPOp'.  Returns the result of
-- the operation it represents, as well as the gradient of the result with
-- respect to its inputs.  See module header for "Numeric.Backprop" and
-- package documentation for examples and usages.
backprop
    :: forall rs a. Every Num rs
    => (forall s. BPOp s rs a)
    -> Tuple rs
    -> (a, Tuple rs)
backprop bp xs = backprop' (summers' l) (unities' l) bp xs
  where
    l :: Length rs
    l = prodLength xs

-- | 'bpOp', but taking explicit 'Summer's and 'Unity's, for the situation
-- where the @rs@ are not instance of 'Num'.
bpOp'
    :: Prod Summer rs
    -> Prod Unity rs
    -> BPOp s rs a
    -> OpB s rs a
bpOp' ss us bp = OpM $ backpropWith ss us bp

-- | Turn a 'BPOp' into an 'OpB'.  Basically converts a 'BP' taking an @rs@
-- and producing an @a@ into an 'Op' taking an @rs@ and returning an @a@,
-- with all of the powers and utility of an 'Op', including all of its
-- gradient-finding glory.
--
-- Handy because an 'OpB' can be used with almost all of
-- the 'Op'-related functions in this moduel, including 'opVar', '~$', etc.
bpOp
    :: (Every Num rs, Known Length rs)
    => BPOp s rs a
    -> OpB s rs a
bpOp = bpOp' summers unities

-- | Simply run the 'BPOp' on an input tuple, getting the result without
-- bothering with the gradient or with backpropagation.
evalBPOp
    :: (forall s. BPOp s rs a)  -- ^ 'BPOp' to run
    -> Tuple rs                 -- ^ input
    -> a                        -- ^ output
evalBPOp bp env = runST $ do
    r <- evalStateT (runReaderT (bpST bp) env)
                    (BPS (map1 (\_ -> FRInternal []) env))
    runReaderT (resolveVar r) env

-- | A version of 'gradBPOp' taking explicit 'Summer's and 'Unity's, so it
-- can be run with types that aren't instances of 'Num'.
gradBPOp'
    :: Prod Summer rs
    -> Prod Unity rs
    -> (forall s. BPOp s rs a)  -- ^ 'BPOp' to differentiate'
    -> Tuple rs                 -- ^ input
    -> Tuple rs                 -- ^ gradient
gradBPOp' ss us bp = snd . backprop' ss us bp

-- | Run the 'BPOp' on an input tuple and return the gradient of the result
-- with respect to the input tuple.
gradBPOp
    :: Every Num rs
    => (forall s. BPOp s rs a)  -- ^ 'BPOp' to differentiate
    -> Tuple rs                 -- ^ input
    -> Tuple rs                 -- ^ gradient
gradBPOp bp = snd . backprop bp


closeOff
    :: (MonadReader (Tuple rs) m, MonadState (BPState s rs) m, MonadBase (ST s) m)
    => Bool
    -> Maybe a
    -> BVar s rs a
    -> m ()
closeOff isTerminal gOut = \case
    BVNode  ix sr -> liftBase $ modifySTRef sr (over (bpnOut . indexP ix) (<> fr))
    BVInp   ix'   -> modifying (bpsSources . indexP ix') (<> fr)
    BVConst _     -> return ()
    BVOp    rs o  -> do
      xs <- traverse1 (fmap I . resolveVar) rs
      gs <- liftBase $ gradOpWithM' o xs gOut
      for1_ (gs `zipP` rs) $ \(I g :&: r) ->
        closeOff False (Just g) r
  where
    fr | isTerminal = FRTerminal gOut
       | otherwise  = FRInternal (IRConst <$> maybeToList gOut)

backpropWith
    :: Prod Summer rs
    -> Prod Unity rs
    -> BPOp s rs a
    -> Tuple rs
    -> ST s (a, Maybe a -> ST s (Tuple rs))
backpropWith ss us bp env = do
    (r, bps0) <- runStateT (runReaderT (bpST bp) env)
                           (BPS (map1 (\_ -> FRInternal []) env))
    res <- runReaderT (resolveVar r) env
    let gradFunc gradOut = do
          BPS{..} <- execStateT (runReaderT (closeOff True gradOut r) env) bps0
          for1 (ss `zipP` us `zipP` _bpsSources) $ \((s :&: u) :&: rs) -> do
            I <$> case rs of
              FRInternal rs' -> runSummer s <$> traverse backwardPass rs'
              FRTerminal g   -> return $ fromMaybe (getUnity u) g
    return (res, gradFunc)

-- | A version of 'implicitly' taking explicit 'Length', indicating the
-- number of inputs required and their types.
--
-- Mostly useful for rare "extremely polymorphic" situations, where GHC
-- can't infer the type and length of the list of inputs.  If you ever
-- actually explicitly write down @rs@ as a list of types, you should be
-- able to just use 'implicitly'.
implicitly'
    :: Length rs
    -> BPOpI s rs a
    -> BPOp s rs a
implicitly' l f = withInps' l (return . f)

-- | Convert a 'BPOpI' into a 'BPOp'.  That is, convert a function on
-- a bundle of 'BVar's (generating an implicit graph) into a fully fledged
-- 'BPOp' that you can run 'backprop' on.  See 'BPOpI' for more
-- information.
--
-- If you are going to write exclusively using implicit 'BVar' operations,
-- it might be more convenient to use "Numeric.Backprop.Implicit" instead,
-- which is geared around that use case.
implicitly
    :: Known Length rs
    => BPOpI s rs a
    -> BPOp s rs a
implicitly = implicitly' known

-- | Create a 'BVar' given an index into the input environment.  For an
-- example,
--
-- @
-- 'inpVar' 'IZ'
-- @
--
-- would refer to the /first/ input variable (the 'Int' in a
-- @'BP' s '[Int, Bool]@), and
--
-- @
-- 'inpVar' ('IS' 'IZ')
-- @
--
-- Would refer to the /second/ input variable (the 'Bool' in a
-- @'BP' s '[Int, Bool]@)
--
-- Typically, there shouldn't be any reason to use 'inpVar' directly.  It's
-- cleaner to get all of your input 'BVar's together using 'withInps' or
-- 'inpVars'.
inpVar
    :: Index rs a
    -> BVar s rs a
inpVar = BVInp

-- | Get a 'Prod' (tupling) of 'BVar's for all of the input environment
-- (@rs@) of the @'BP' s rs@
--
-- For example, if your 'BP' has an 'Int' and 'Double' in its input
-- environment (a @'BP' s '[Int, Double]@), this would return a 'BVar'
-- pointing to the 'Int' and a 'BVar' pointing to the 'Double'.
--
-- @
-- let x :< y :< Ø = 'inpVars' :: 'Prod' ('BVar' s '[Int, Double]) '[Int, Double]
--
-- -- the first item, x, is a var to the input 'Int'
-- x :: 'BVar' s '[Int, Double] Int
-- -- the second item, y, is a var to the input 'Double'
-- y :: 'BVar' s '[Int, Double] Double
-- @
inpVars
    :: Known Length rs
    => Prod (BVar s rs) rs
inpVars = inpVars' known

-- | A version of 'inpVars' taking explicit 'Length', indicating the
-- number of inputs required and their types.
--
-- Mostly useful for rare "extremely polymorphic" situations, where GHC
-- can't infer the type and length of the list of inputs.  If you ever
-- actually explicitly write down @rs@ as a list of types, you should be
-- able to just use 'inpVars'.
inpVars'
    :: Length rs
    -> Prod (BVar s rs) rs
inpVars' = map1 inpVar . indices'

-- | A version of 'withInps' taking explicit 'Length', indicating the
-- number of inputs required and their types.
--
-- Mostly useful for rare "extremely polymorphic" situations, where GHC
-- can't infer the type and length of the list of inputs.  If you ever
-- actually explicitly write down @rs@ as a list of types, you should be
-- able to just use 'withInps'.
withInps'
    :: Length rs
    -> (Prod (BVar s rs) rs -> BP s rs a)
    -> BP s rs a
withInps' l f = f (inpVars' l)

-- | Runs a continuation on a 'Prod' of all of the input 'BVar's.
--
-- Handy for bringing the environment into scope and doing stuff with it:
--
-- @
-- foo :: 'BPOp' '[Double, Int] a
-- foo = 'withInps' $ \\(x :< y :< Ø) -\> do
--     -- do stuff with inputs
-- @
--
-- Looks kinda like @foo (x :< y :< Ø) = -- ...@, don't it?
--
-- Note that the above is the same as
--
-- @
-- foo :: 'BPOp' '[Double, Int] a
-- foo = do
--     x :< y :< Ø <- 'inpVars'
--     -- do stuff with inputs
-- @
--
-- But just a little nicer!
withInps
    :: Known Length rs
    => (Prod (BVar s rs) rs -> BP s rs a)
    -> BP s rs a
withInps = withInps' known

-- | Apply 'OpB' over a 'Prod' of 'BVar's, as inputs.
-- Provides "implicit" backpropagation, with deferred evaluation.
--
-- If you had an @'OpB' s '[a, b, c] d@, this function will expect a 3-Prod
-- of a @'BVar' s rs a@, a @'BVar' s rs b@, and a @'BVar' s rs c@, and the
-- result will be a @'BVar' s rs d@:
--
-- @
-- myOp :: 'OpB' s '[a, b, c] d
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
-- z    :: 'BVar' s rs c
--
-- x :< y :< z :< Ø              :: 'Prod' ('BVar' s rs) '[a, b, c]
-- 'liftB' myOp (x :< y :< z :< Ø) :: 'BVar' s rs d
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can provide any 'Op'
-- here, as well (like those created by 'op1', 'op2', 'constOp', 'op0'
-- etc.)
--
-- 'opVar' has an infix alias, '.$', so the above example can also be
-- written as:
--
-- @
-- myOp '.$' (x :< y :< z :< Ø) :: 'BVar' s rs d
-- @
--
-- to let you pretend that you're applying the 'myOp' function to three
-- inputs.
--
-- The result is a new /deferred/ 'BVar'.  This should be fine in most
-- cases, unless you use the result in more than one location.  This will
-- cause evaluation to be duplicated and multiple redundant graph nodes to
-- be created.  If you need to use it in two locations, you should use
-- 'opVar' instead of 'liftB', or use 'bindVar':
--
-- @
-- 'opVar' o xs = 'bindVar' ('liftB' o xs)
-- @
--
-- 'liftB' can be thought of as a "deferred evaluation" version of 'liftB'.
liftB
    :: OpB s as a
    -> Prod (BVar s rs) as
    -> BVar s rs a
liftB = flip BVOp


-- | Infix synonym for 'liftB', which lets you pretend that you're applying
-- 'OpB's as if they were functions:
--
-- @
-- myOp :: 'OpB' s '[a, b, c] d
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
-- z    :: 'BVar' s rs c
--
-- x :< y :< z :< Ø           :: 'Prod' ('BVar' s rs) '[a, b, c]
-- myOp '.$' (x :< y :< z :< Ø) :: 'BVar' s rs d
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in any 'Op'
-- here, as well (like those created by 'op1', 'op2', 'constOp', 'op0'
-- etc.)
--
-- See the documentation for 'liftB' for all the caveats of this usage.
--
-- '.$' can also be thought of as a "deferred evaluation" version of '~$':
--
-- @
-- o '~$' xs = 'bindVar' (o '.$' xs)
-- @
--
infixr 1 .$
(.$)
    :: OpB s as a
    -> Prod (BVar s rs) as
    -> BVar s rs a
(.$) = liftB


-- | Convenient wrapper over 'liftB' that takes an 'OpB' with one argument
-- and a single 'BVar' argument.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'liftB1' o x = 'liftB' o (x ':<' 'Ø')
--
-- myOp :: 'Op' '[a] b
-- x    :: 'BVar' s rs a
--
-- 'liftB1' myOp x :: 'BVar' s rs b
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op1') as well.
--
-- See the documentation for 'liftB' for caveats and potential problematic
-- situations with this.
liftB1
    :: OpB s '[a] b
    -> BVar s rs a
    -> BVar s rs b
liftB1 o = liftB o . only

-- | Convenient wrapper over 'liftB' that takes an 'OpB' with two arguments
-- and two 'BVar' arguments.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'liftB2' o x y = 'liftB' o (x ':<' y ':<' 'Ø')
--
-- myOp :: 'Op' '[a, b] c
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
--
-- 'liftB2' myOp x y :: 'BVar' s rs c
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op2') as well.
--
-- See the documentation for 'liftB' for caveats and potential problematic
-- situations with this.
liftB2
    :: OpB s '[a,b] c
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
liftB2 o x y = liftB o (x :< y :< Ø)

-- | Convenient wrapper over 'liftB' that takes an 'OpB' with three arguments
-- and three 'BVar' arguments.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'liftB3' o x y z = 'liftB' o (x ':<' y ':<' z ':<' 'Ø')
--
-- myOp :: 'Op' '[a, b, c] d
-- x    :: 'BVar' s rs a
-- y    :: 'BVar' s rs b
-- z    :: 'BVar' s rs c
--
-- 'liftB3' myOp x y z :: 'BVar' s rs d
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op3') as well.
--
-- See the documentation for 'liftB' for caveats and potential problematic
-- situations with this.
liftB3
    :: OpB s '[a,b,c] d
    -> BVar s rs a
    -> BVar s rs b
    -> BVar s rs c
    -> BVar s rs d
liftB3 o x y z = liftB o (x :< y :< z :< Ø)











-- | Apply a function to the contents of an STRef, and cache the results
-- using the given lens.  If already calculated, simply returned the cached
-- result.
caching
    :: Lens' a (Maybe b)
    -> STRef s a
    -> (a -> ST s b)
    -> ST s b
caching l r f = do
    x <- readSTRef r
    let y = view l x
    case y of
      Just z ->
        return z
      Nothing -> do
        z <- f x
        modifySTRef r (set l (Just z))
        return z

