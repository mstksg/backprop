{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

-- |
-- Module      : Numeric.Backprop.Mono
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
--
-- Provides a monomorphic interface to the library and to the
-- "Numeric.Backprop" module.
--
-- They are monomorphic in the sense that all of the /inputs/ have to be of
-- the same type.  So, something like
--
-- @
-- 'Numeric.Backprop.BP' s '[Double, Double, Double] Int
-- @
--
-- From "Numeric.Backprop" would, in this module, be:
--
-- @
-- 'BP' s 'N3' Double Int
-- @
--
-- Instead of dealing with 'Prod's and 'Tuple's, this module works with
-- 'VecT's and 'Vec's, respectively.  These are fixed-length vectors whose
-- length are encoded in their types, constructed with ':*' (for 'VecT') or
-- ':+' (for 'Vec').
--
-- Most of the concepts in normal heterogeneous backprop (for
-- "Numeric.Backprop") should apply here as well, so you can look at any of
-- the tutorials or examples and repurpose them to work here.  Just
-- remember to convert something like @'Numeric.Backprop.Op.Op' '[a, a] b@
-- to @'Op' 'N2' a b@.
--
-- As a comparison, this implements something similar in functionality to
-- "Numeric.AD" and "Numeric.AD.Mode.Reverse" from the /ad/ package, in
-- that they both offer monomorphic automatic differentiation through
-- backpropagation.  This module doesn't allow the computation of jacobians
-- or generalized gradients for \(\mathbb{R}^N \rightarrow \mathbb{R}^M\)
-- functions.  This module only computs gradients for \(\mathbb{R}^N
-- \rightarrow \mathbb{R}\)-like functions.  This is more of a conscious
-- design decision in the API of this module rather than a fundamental
-- limitation of the implementation.
--
-- This module also allows you to build explicit data dependency graphs so
-- the library can reduce duplication and perform optimizations, which may
-- or may not provide advantages over "Numeric.AD.Mode.Reverse"'s
-- 'System.IO.Unsafe.unsafePerformIO'-based implicit graph building.
--

module Numeric.Backprop.Mono (
  -- * Types
  -- ** Backprop types
    BP, BPOp, BPOpI, BVar
  , Op, OpB
  -- ** Vector types
  -- ** Vectors
  , VecT(..), Vec, I(..)
  -- * BP
  -- ** Backprop
  , backprop, evalBPOp, gradBPOp
  -- ** Utility combinators
  , withInps, implicitly
  -- * Vars
  , constVar
  , inpVar, inpVars
  , bpOp
  , bindVar
  -- ** From Ops
  , opVar, (~$)
  , opVar1, opVar2, opVar3
  , (-$)
  -- ** Combining
  , liftB, (.$), liftB1, liftB2, liftB3
  -- * Op
  , op1, op2, op3, opN
  , op1', op2', op3'
  -- * Utility
  , pattern (:+), (*:), (+:), head'
  -- ** 'Nat' type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  ) where

import           Data.Type.Fin
import           Data.Type.Nat
import           Data.Type.Product hiding         (head')
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.Backprop.Internal.Helper
import           Numeric.Backprop.Op.Mono
import           Type.Class.Known
import qualified Numeric.Backprop                 as BP

-- | A Monad allowing you to explicitly build hetereogeneous data
-- dependency graphs and that the library can perform backpropagation on.
--
-- A @'BP' s n r a@ is a 'BP' action that uses an environment @n@ values of
-- type @r@, and returns an @a@. When "run", it will compute a gradient that
-- is a vector ('Vec') of @n@ @r@s.  (The phantom parameter @s@ is used to
-- ensure that any 'BVar's aren't leaked out of the monad)
--
-- Note that you can only "run" a @'BP' s n r@ that produces a 'BVar' --
-- that is, things of the form
--
-- @
-- 'BP' s n r ('BVar' n r a)
-- @
--
-- The above is a 'BP' action that returns a 'BVar' containing an @a@.
-- When this is run, it'll produce a result of type @a@ and a gradient of
-- that is a vector of @n@ values of type @r@.  (This form has a type
-- synonym, 'BPOp', for convenience)
--
-- For example, @'BP' s 'N3' Double@ is a monad that represents
-- a computation with three 'Double's as inputs.  And, if you ran a
--
-- @
-- 'BP' s 'N3' Double ('BVar' N3 Double Int)
-- @
--
-- Or, using the 'BPOp' type synonym:
--
-- @
-- 'BPOp' s 'N3' Double Int
-- @
--
-- with 'backprop' or 'gradBPOp', it'll return a gradient on the inputs (a
-- vector of three 'Double's) and produce a value of type 'Int'.
type BP s n r      = BP.BP s (Replicate n r)

-- | The basic unit of manipulation inside 'BP' (or inside an
-- implicit-graph backprop function).  Instead of directly working with
-- values, you work with 'BVar's contating those values.  When you work
-- with a 'BVar', the /backprop/ library can keep track of what values
-- refer to which other values, and so can perform backpropagation to
-- compute gradients.
--
-- A @'BVar' s n r a@ refers to a value of type @a@, with an environment
-- of @n@ values of type @r@.  The phantom parameter @s@ is used to
-- ensure that stray 'BVar's don't leak outside of the backprop process.
--
-- (That is, if you're using implicit backprop, it ensures that you interact
-- with 'BVar's in a polymorphic way.  And, if you're using explicit
-- backprop, it ensures that a @'BVar' s n r a@ never leaves the @'BP'
-- s n r@ that it was created in.)
--
-- 'BVar's have 'Num', 'Fractional', 'Floating', etc. instances, so they
-- can be manipulated using polymorphic functions and numeric functions in
-- Haskell.  You can add them, subtract them, etc., in "implicit" backprop
-- style.
--
-- (However, note that if you directly manipulate 'BVar's using those
-- instances or using 'liftB', it delays evaluation, so every usage site
-- has to re-compute the result/create a new node.  If you want to re-use
-- a 'BVar' you created using '+' or '-' or 'liftB', use
-- 'bindVar' to force it first.  See documentation for
-- 'bindVar' for more details.)
type BVar s n a    = BP.BVar s (Replicate n a)

-- | A handy type synonym representing a 'BP' action that returns a 'BVar'.
-- This is handy because this is the form of 'BP' actions that
-- 'backprop' and 'gradBPOp' (etc.) expects.
--
-- A value of type:
--
-- @
-- 'BPOp' s n r a
-- @
--
-- is an action that takes an input environment of @n@ values of type @r@
-- and produces a 'BVar' containing a value of type @a@.  Because it
-- returns a 'BVar', the library can track the data dependencies between
-- the 'BVar' and the input environment and perform backpropagation.
--
-- See documentation for 'BP' for an explanation of the phantom type
-- parameter @s@.
type BPOp s n r a  = BP s n r (BVar s n r a)

-- | An "implicit" operation on 'BVar's that can be backpropagated.
-- A value of type:
--
-- @
-- 'BPOpI' s n r a
-- @
--
-- takes a vector ('Vec') of @n@ of 'BVar's containg @r@s and uses them to (purely)
-- produce a 'BVar' containing an @a@.
--
-- @
-- foo :: BPOpI s 'N2' Double Double
-- foo (x :* y :* ØV) = x + sqrt y
-- @
--
-- If you are exclusively doing implicit backpropagation by combining
-- 'BVar's and using 'BPOpI's, you are probably better off just importing
-- "Numeric.Backprop.Mono.Implicit", which provides better tools.  This
-- type synonym exists in "Numeric.Backprop.Mono" just for the 'implicitly'
-- function, which can convert "implicit" backprop functions like
-- a @'BPOpI' s rs a@ into an "explicit" graph backprop function, a @'BPOp'
-- s rs a@.
type BPOpI s n r a = VecT n (BVar s n r) r -> BVar s n r a

-- | A subclass of 'Numeric.Backprop.Op.Mono.OpM' (and superclass of 'Op'),
-- representing 'Op's that the /backprop/ library uses to perform
-- backpropation.
--
-- An
--
-- @
-- 'OpB' s n a b
-- @
--
-- represents a differentiable function that takes a @n@ values of type @a@
-- produces an a @b@, which can be run on @'BVar' s@s and also inside
-- @'BP' s@s.  For example, an @'OpB' s 'N2' Double Bool@ takes two 'Double's
-- and produces a 'Bool', and does it in a differentiable way.
--
-- 'OpB' is a /superset/ of 'Op', so, if you see any function that expects
-- an 'OpB' (like 'Numeric.Backprop.opVar'' and 'Numeric.Backprop.~$', for
-- example), you can give them an 'Op', as well.
--
-- You can think of 'OpB' as a superclass/parent class of 'Op' in this
-- sense, and of 'Op' as a subclass of 'OpB'.
type OpB s n a b   = BP.OpB s (Replicate n a) b

-- | Apply an 'OpB' to a 'VecT' (vector) of 'BVar's.
--
-- If you had an @'OpB' s N3 a b@, this function will expect a vector of of
-- three @'BVar' s n r a@s, and the result will be a @'BVar' s n r b@:
--
-- @
-- myOp :: 'OpB' s N3 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r a
-- z    :: 'BVar' s n r a
--
-- x ':*' y :* z :* 'ØV'              :: 'VecT' N3 ('BVar' s n r) a
-- 'opVar' myOp (x :* y :* z :* ØV) :: 'BP' s n r ('BVar' s n r b)
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
-- myOp '~$' (x :* y :* z :* ØV) :: 'BP' s n r ('BVar' s n r b)
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
    :: forall s m n r a b. Num b
    => OpB s m a b
    -> VecT m (BVar s n r) a
    -> BP s n r (BVar s n r b)
opVar o = BP.opVar o . vecToProd

-- | Infix synonym for 'opVar', which lets you pretend that you're applying
-- 'OpB's as if they were functions:
--
-- @
-- myOp :: 'OpB' s N3 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r a
-- z    :: 'BVar' s n r a
--
-- x ':*' y :* z :* 'ØV'              :: 'VecT' N3 ('BVar' s n r) a
-- myOp '~$' (x :* y :* z :* ØV) :: 'BP' s n r ('BVar' s n r b)
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
    :: forall s m n r a b. Num b
    => OpB s m a b
    -> VecT m (BVar s n r) a
    -> BP s n r (BVar s n r b)
(~$) = opVar @_ @_ @_ @r

-- | Lets you treat a @'BPOp' s n a b@ as an @'Op' n a b@, and "apply"
-- arguments to it just like you would with an 'Op' and '~$' / 'opVar'.
--
-- Basically a convenient wrapper over 'bpOp' and '~$':
--
-- @
-- o '-$' xs = bpOp o '~$' xs
-- @
--
-- So for a @'BPOp' s n a b@, you can "plug in" 'BVar's to each @a@, and
-- get a @b@ as a result.
--
-- Useful for running a @'BPOp' s n a b@ that you got from a different function, and
-- "plugging in" its @a@ inputs with 'BVar's from your current
-- environment.
infixr 1 -$
(-$)
    :: forall s m n r a b. (Num a, Num b, Known Nat m)
    => BPOp s m a b
    -> VecT m (BVar s n r) a
    -> BP s n r (BVar s n r b)
o -$ xs = opVar @_ @_ @_ @r (bpOp @_ @_ @a @b o) xs

-- | Create a 'BVar' that represents just a specific value, that doesn't
-- depend on any other 'BVar's.
constVar
    :: a
    -> BVar s n r a
constVar = BP.constVar

-- | Convenient wrapper over 'opVar' that takes an 'OpB' with one argument
-- and a single 'BVar' argument.  Lets you not have to type out the entire
-- 'VecT'.
--
-- @
-- 'opVar1' o x = 'opVar' o (x ':*' 'ØV')
--
-- myOp :: 'Op' N2 a b
-- x    :: 'BVar' s n r a
--
-- 'opVar1' myOp x :: 'BP' s n r ('BVar' s n r b)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op1') as well.
opVar1
    :: forall s n r a b. Num b
    => OpB s N1 a b
    -> BVar s n r a
    -> BP s n r (BVar s n r b)
opVar1 o x = opVar @_ @_ @n @r o (x :* ØV)

-- | Convenient wrapper over 'opVar' that takes an 'OpB' with two arguments
-- and two 'BVar' arguments.  Lets you not have to type out the entire
-- 'VecT'.
--
-- @
-- 'opVar2' o x y = 'opVar' o (x ':*' y ':*' 'ØV')
--
-- myOp :: 'Op' N2 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r b
--
-- 'opVar2' myOp x y :: 'BP' s n r ('BVar' s n r b)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op2') as well.
opVar2
    :: forall s n r a b. Num b
    => OpB s N2 a b
    -> BVar s n r a
    -> BVar s n r a
    -> BP s n r (BVar s n r b)
opVar2 o x y = opVar @_ @_ @n @r o (x :* y :* ØV)

-- | Convenient wrapper over 'opVar' that takes an 'OpB' with three arguments
-- and three 'BVar' arguments.  Lets you not have to type out the entire
-- 'VecT'.
--
-- @
-- 'opVar3' o x y z = 'opVar' o (x ':*' y ':*' z ':*' 'ØV')
--
-- myOp :: 'Op' N3 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r a
-- z    :: 'BVar' s n r a
--
-- 'opVar3' myOp x y z :: 'BP' s n r ('BVar' s n r b)
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op3') as well.
opVar3
    :: forall s n r a b. Num b
    => OpB s N3 a b
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
    -> BP s n r (BVar s n r b)
opVar3 o x y z = opVar @_ @_ @n @r o (x :* y :* z :* ØV)

-- | Concretizes a delayed 'BVar'.  If you build up a 'BVar' using numeric
-- functions like '+' or '*' or using 'liftB', it'll defer the evaluation,
-- and all of its usage sites will create a separate graph node.
--
-- Use 'bindVar' if you ever intend to use a 'BVar' in more than one
-- location.
--
-- @
-- -- bad
-- errSquared :: Num a => 'BP' s N2 a a
-- errSquared = 'withInp' $ \\(x :* y :* Ø) -\> do
--     let err = r - t
--     'return' (err * err)   -- err is used twice!
--
-- -- good
-- errSquared :: Num a => 'BP' s N2 a a
-- errSquared = 'withInp' $ \\(x :* y :* Ø) -\> do
--     let err = r - t
--     e <- 'bindVar' err     -- force e, so that it's safe to use twice!
--     'return' (e * e)
--
-- -- better
-- errSquared :: Num a => 'BP' s N2 a a
-- errSquared = 'withInp' $ \\(x :* y :* Ø) -\> do
--     let err = r - t
--     e <- 'bindVar' err
--     'bindVar' (e * e)      -- result is forced so user doesn't have to worry
-- @
--
-- Note the relation to 'opVar' / '~$' / 'liftB' / '.$':
--
-- @
-- 'opVar' o xs    = 'bindVar' ('liftB' o xs)
-- o '~$' xs       = 'bindVar' (o '.$' xs)
-- 'op2' (*) '~$' (x :< y :< Ø) = 'bindVar' (x * y)
-- @
--
-- So you can avoid 'bindVar' altogether if you use the explicitly binding
-- '~$' and 'opVar' etc.
--
-- Note that 'bindVar' on 'BVar's that are already forced is a no-op.
bindVar
    :: forall s n r a. Num a
    => BVar s n r a
    -> BP s n r (BVar s n r a)
bindVar = BP.bindVar

-- | Perform backpropagation on the given 'BPOp'.  Returns the result of
-- the operation it represents, as well as the gradient of the result with
-- respect to its inputs.  See module header for "Numeric.Backprop.Mono"
-- and package documentation for examples and usages.
backprop
    :: forall n r a. Num r
    => (forall s. BPOp s n r a)
    -> Vec n r
    -> (a, Vec n r)
backprop bp i = (x, prodAlong i g)
  where
    (x, g) = BP.backprop' (toSummers i) (toUnities i) bp (vecToProd i)

-- | Simply run the 'BPOp' on an input vector, getting the result without
-- bothering with the gradient or with backpropagation.
evalBPOp
    :: forall n r a. ()
    => (forall s. BPOp s n r a)
    -> Vec n r
    -> a
evalBPOp bp = BP.evalBPOp bp . vecToProd

-- | Run the 'BPOp' on an input vector and return the gradient of the result
-- with respect to the input vector
gradBPOp
    :: forall n r a. Num r
    => (forall s. BPOp s n r a)
    -> Vec n r
    -> Vec n r
gradBPOp bp = snd . backprop bp

-- | Turn a 'BPOp' into an 'OpB'.  Basically converts a 'BP' taking @n@
-- @r@s and producing an @a@ into an 'Op' taking an @n@ @r@s and returning
-- an @a@, with all of the powers and utility of an 'Op', including all of
-- its gradient-finding glory.
--
-- Handy because an 'OpB' can be used with almost all of
-- the 'Op'-related functions in this moduel, including 'opVar', '~$', etc.
bpOp
    :: forall s n r a. (Num r, Known Nat n)
    => BPOp s n r a
    -> OpB s n r a
bpOp b = BP.bpOp' (nSummers' @n @r n) (nUnities' @n @r n) b
  where
    n :: Nat n
    n = known


-- | Create a 'BVar' given an index ('Fin') into the input environment.  For an
-- example,
--
-- @
-- 'inpVar' 'FZ'
-- @
--
-- would refer to the /first/ input variable, Bool]@), and
--
-- @
-- 'inpVar' ('FS' 'FZ')
-- @
--
-- Would refer to the /second/ input variable.
--
-- Typically, there shouldn't be any reason to use 'inpVar' directly.  It's
-- cleaner to get all of your input 'BVar's together using 'withInps' or
-- 'inpVars'.
inpVar
    :: Fin n
    -> BVar s n r r
inpVar = BP.inpVar . finIndex

-- | Get a 'VecT' (vector) of 'BVar's for all of the input environment
-- (the @n@ @r@s) of the @'BP' s n r@
--
-- For example, if your 'BP' has two 'Double's inside its input
-- environment (a @'BP' s 'N2' Double@), this would return two 'BVar's,
-- pointing to each input 'Double'.
--
-- @
-- case ('inpVars' :: 'VecT' 'N2' ('BVar' s 'N2' Double) Double) of
--   x :* y :* ØV -> do
--     -- the first item, x, is a var to the first input
--     x :: 'BVar' s N2 Double
--     -- the second item, y, is a var to the second input
--     y :: 'BVar' s N2 Double
-- @
inpVars
    :: Known Nat n
    => VecT n (BVar s n r) r
inpVars = vgen_ inpVar

-- | Runs a continuation on a 'Vec' of all of the input 'BVar's.
--
-- Handy for bringing the environment into scope and doing stuff with it:
--
-- @
-- foo :: 'BPOp' 'N2' Double Int
-- foo = 'withInps' $ \\(x :* y :* ØV) -\> do
--     -- do stuff with inputs
-- @
--
-- Looks kinda like @foo (x :* y *+ ØV) = -- ...@, don't it?
--
-- Note that the above is the same as
--
-- @
-- foo :: 'BPOp' 'N2' Double Int
-- foo = do
--     case 'inpVars' of
--       x :* y :* ØV -> do
--         -- do stuff with inputs
-- @
--
-- But just a little nicer!
withInps
    :: Known Nat n
    => (VecT n (BVar s n r) r -> BP s n r a)
    -> BP s n r a
withInps f = f inpVars

-- | Convert a 'BPOpI' into a 'BPOp'.  That is, convert a function on
-- a bundle of 'BVar's (generating an implicit graph) into a fully fledged
-- 'BPOp' that you can run 'backprop' on.  See 'BPOpI' for more
-- information.
--
-- If you are going to write exclusively using implicit 'BVar' operations,
-- it might be more convenient to use "Numeric.Backprop.Mono.Implicit"
-- instead, which is geared around that use case.
implicitly
    :: Known Nat n
    => BPOpI s n r a
    -> BPOp s n r a
implicitly f = withInps (return . f)

-- | Apply 'OpB' over a 'VecT' of 'BVar's, as inputs. Provides "implicit"
-- backpropagation, with deferred evaluation.
--
-- If you had an @'OpB' s N3 a b@, this function will expect a vector of of
-- three @'BVar' s n r a@s, and the result will be a @'BVar' s n r b@:
--
-- @
-- myOp :: 'OpB' s N3 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r a
-- z    :: 'BVar' s n r a
--
-- x ':*' y :* z :* 'ØV'              :: 'VecT' N3 ('BVar' s n r) a
-- 'liftB' myOp (x :* y :* z :* ØV) :: 'BVar' s n r b
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can provide any 'Op'
-- here, as well (like those created by 'op1', 'op2', 'constOp', 'op0'
-- etc.)
--
-- 'liftB' has an infix alias, '.$', so the above example can also be
-- written as:
--
-- @
-- myOp '.$' (x :* y :* z :* ØV) :: 'BVar' s n r b
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
-- 'liftB' can be thought of as a "deferred evaluation" version of 'opVar'.
liftB
    :: forall s m n a b r. ()
    => OpB s m a b
    -> VecT m (BVar s n r) a
    -> BVar s n r b
liftB o = BP.liftB o . vecToProd

-- | Infix synonym for 'liftB', which lets you pretend that you're applying
-- 'OpB's as if they were functions:
--
-- @
-- myOp :: 'OpB' s N3 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r a
-- z    :: 'BVar' s n r a
--
-- x ':*' y :* z :* 'ØV'              :: 'VecT' N3 ('BVar' s n r) a
-- myOp '.$' (x :* y :* z :* ØV) :: 'BVar' s n r b
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
(.$)
    :: forall s m n a b r. ()
    => OpB s m a b
    -> VecT m (BVar s n r) a
    -> BVar s n r b
o .$ x = liftB @_ @_ @_ @_ @_ @r o x

-- | Convenient wrapper over 'liftB' that takes an 'OpB' with one argument
-- and a single 'BVar' argument.  Lets you not have to type out the entire
-- 'VecT'.
--
-- @
-- 'liftB1' o x = 'liftB' o (x ':*' 'ØV')
--
-- myOp :: 'Op' N2 a b
-- x    :: 'BVar' s n r a
--
-- 'liftB1' myOp x :: 'BVar' s n r b
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op1') as well.
--
-- See the documentation for 'liftB' for caveats and potential problematic
-- situations with this.
liftB1
    :: OpB s N1 a a
    -> BVar s n r a
    -> BVar s n r a
liftB1 = BP.liftB1

-- | Convenient wrapper over 'liftB' that takes an 'OpB' with two arguments
-- and two 'BVar' arguments.  Lets you not have to type out the entire
-- 'VecT'.
--
-- @
-- 'liftB2' o x y = 'liftB' o (x ':*' y ':*' 'ØV')
--
-- myOp :: 'Op' N2 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r b
--
-- 'liftB2' myOp x y :: 'BVar' s n r b
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op2') as well.
--
-- See the documentation for 'liftB' for caveats and potential problematic
-- situations with this.
liftB2
    :: OpB s N2 a a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
liftB2 = BP.liftB2

-- | Convenient wrapper over 'liftB' that takes an 'OpB' with three arguments
-- and three 'BVar' arguments.  Lets you not have to type out the entire
-- 'Prod'.
--
-- @
-- 'liftB3' o x y z = 'liftB' o (x ':*' y ':*' z ':*' 'ØV')
--
-- myOp :: 'Op' N3 a b
-- x    :: 'BVar' s n r a
-- y    :: 'BVar' s n r b
-- z    :: 'BVar' s n r b
--
-- 'liftB3' myOp x y z :: 'BVar' s n r b
-- @
--
-- Note that 'OpB' is a superclass of 'Op', so you can pass in an 'Op' here
-- (like one made with 'op3') as well.
--
-- See the documentation for 'liftB' for caveats and potential problematic
-- situations with this.
liftB3
    :: OpB s N3 a a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
    -> BVar s n r a
liftB3 = BP.liftB3








toSummers
    :: Num a
    => VecT n f a
    -> Prod BP.Summer (Replicate n a)
toSummers = \case
    ØV      -> Ø
    _ :* xs -> BP.Summer sum :< toSummers xs

toUnities
    :: Num a
    => VecT n f a
    -> Prod BP.Unity (Replicate n a)
toUnities = \case
    ØV      -> Ø
    _ :* xs -> BP.Unity 1 :< toUnities xs

