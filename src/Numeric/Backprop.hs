{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Numeric.Backprop
-- Copyright   : (c) Justin Le 2023
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Automatic differentation and backpropagation.
--
-- Main idea: Write a function computing what you want, and the library
-- automatically provies the gradient of that function as well, for usage
-- with gradient descent and other training methods.
--
-- See the <https://backprop.jle.im homepage> for an introduction and
-- walkthrough.
--
-- In more detail: instead of working directly with values to produce your
-- result, you work with 'BVar's containing those values.  Working with
-- these 'BVar's is made smooth with the usage of lenses and other
-- combinators, and libraries can offer operatons on 'BVar's instead of
-- those on normal types directly.
--
-- Then, you can use:
--
-- @
-- 'evalBP' :: (forall s. 'Reifies' s 'W' => 'BVar' s a -> BVar s b) -> (a -> b)
-- @
--
-- to turn a 'BVar' function into the function on actual values @a -> b@.
-- This has virtually zero overhead over writing the actual function
-- directly.
--
-- Then, there's:
--
-- @
-- 'gradBP' :: (forall s. 'Reifies' s 'W' => 'BVar' s a -> BVar s b) -> (a -> a)
-- @
--
-- to automatically get the /gradient/, as well, for a given input.
--
-- Refer to the <https://backprop.jle.im homepage> for more information and
-- links to demonstrations and tutorials, or dive striaght in by reading
-- the docs for 'BVar'.
--
-- If you are writing a library, see
-- <https://backprop.jle.im/08-equipping-your-library.html> for a guide for
-- equipping your library with backpropatable operations.
--
-- In the original version 0.1, this module required 'Num' instances for
-- methods instead of 'Backprop' instances.  This interface is still
-- available in "Numeric.Backprop.Num", which has the same API as this
-- module, except with 'Num' constraints on all values instead of
-- 'Backprop' constraints.
--
-- See "Prelude.Backprop.Explicit" for a version allowing you to provide
-- 'zero', 'add', and 'one' explicitly, which can be useful when attempting
-- to avoid orphan instances or when mixing both 'Backprop' and 'Num'
-- styles.
module Numeric.Backprop (
  -- * Types
  BVar,
  W,
  Backprop (..),
  ABP (..),
  NumBP (..),

  -- * Running
  backprop,
  E.evalBP,
  gradBP,
  backpropWith,

  -- ** Multiple inputs
  backprop2,
  E.evalBP2,
  gradBP2,
  backpropWith2,
  backpropN,
  E.evalBPN,
  gradBPN,
  backpropWithN,

  -- * Manipulating 'BVar'
  E.evalBP0,
  E.constVar,
  E.auto,
  E.coerceVar,
  (^^.),
  (.~~),
  (%~~),
  (^^?),
  (^^..),
  (^^?!),
  viewVar,
  setVar,
  overVar,
  sequenceVar,
  collectVar,
  previewVar,
  toListOfVar,
  pattern T2,
  pattern T3,

  -- ** With Isomorphisms
  isoVar,
  isoVar2,
  isoVar3,
  isoVarN,

  -- ** With 'Op's#liftops#
  -- $liftops
  liftOp,
  liftOp1,
  liftOp2,
  liftOp3,

  -- ** Generics#hkd#
  -- $hkd
  splitBV,
  joinBV,
  pattern BV,
  E.BVGroup,

  -- * 'Op'
  Op (..),

  -- ** Creation
  op0,
  opConst,
  idOp,
  bpOp,

  -- *** Giving gradients directly
  op1,
  op2,
  op3,

  -- *** From Isomorphisms
  opCoerce,
  opTup,
  opIso,
  opIsoN,
  opLens,

  -- *** No gradients
  noGrad1,
  noGrad,

  -- * Utility
  Reifies,
) where

import Data.Functor.Identity
import Data.Maybe
import Data.Reflection
import Data.Vinyl
import GHC.Generics
import Lens.Micro
import Numeric.Backprop.Class
import Numeric.Backprop.Explicit (BVar, W)
import qualified Numeric.Backprop.Explicit as E
import Numeric.Backprop.Op

-- $liftops
--
-- This library provides a few primitive actions for manipulating 'BVar's
-- and the values inside them, including its 'Num', 'Fractional', and
-- 'Floating' instances, and lens-based operations like '^^.', '.~~' '^^?',
-- and '^^..'.
--
-- However, the power of this library comes from manipulating many
-- different types from libraries, like matrices and vectors.  Libraries
-- can provide their own @'BVar' s a -> 'BVar' s b@ functions, alongside
-- (or in lieu of) @a -> b@ functions for their types.
--
-- The easiest way to create a 'BVar' function is to use 'liftOp' with an
-- 'Op' constructor.  For example, imagine a vector library providing a dot
-- product function.  We can write this using 'liftOp2' and 'op2':
--
-- @
-- dot :: 'BVar' s Vec -> BVar s Vec -> BVar s Double
-- dot = 'liftOp2' . op2 $ \\xs ys ->
--         ( sum (zipWith (*) xs ys)
--         , \\g -> (map (*g) ys, map (*g) xs)
--         )
-- @
--
-- We provide a function that, given the two inputs, returns:
--
--     (1) The result of the function on those two inputs
--     (2) A function taking the "total derivative", and returning the
--     gradient with respect to each of the inputs.
--
-- See documentation in "Numeric.Backprop.Op" for more information on the
-- second part (the gradient).
--
-- Nice 'Op's are how /backprop/ links together 'BVar's and tracks them to
-- determine their gradient.  Ideally, users would never have to deal with
-- these when backpropagating their own functions, and library authors
-- providing their matrix and vector operations, etc. would provide 'BVar'
-- variants of their normal operations.
--
-- In fact, 'BVar' operations could even be defined /instead/ of normal
-- operations, since it is easy to go from @'BVar' s a -> 'BVar' s b@ to @a
-- -> b@, using 'evalBP', and this carries virtually zero overhead, so some
-- libraries might even provide 'BVar' versions by default.

-- | 'backprop' generalized to multiple inputs of different types.  See the
-- "Numeric.Backprop.Op#prod" for a mini-tutorial on heterogeneous lists.
--
-- Not strictly necessary, because you can always uncurry a function by
-- passing in all of the inputs in a data type containing all of the
-- arguments or a giant tuple.  However, this could potentially also be
-- more performant.
--
-- A @'Rec' ('BVar' s) '[Double, Float, Double]@, for instance, is a tuple
-- of @'BVar' s 'Double'@, @'BVar' s 'Float'@, and @'BVar' s 'Double'@, and
-- can be pattern matched on using ':<' (cons) and 'Ø' (nil).
--
-- The @'RPureConstrained' 'Backprop' as@ in the constraint says that every
-- value in the type-level list @as@ must have a 'Backprop' instance.  This
-- means you can use, say, @'[Double, Float, Int]@, but not @'[Double,
-- Bool, String]@.
--
-- If you stick to /concerete/, monomorphic usage of this (with specific
-- types, typed into source code, known at compile-time), then
-- @'RPureConstrained' 'Backprop' as@ should be fulfilled automatically.
backpropN ::
  (RPureConstrained Backprop as, Backprop b) =>
  (forall s. Reifies s W => Rec (BVar s) as -> BVar s b) ->
  Rec Identity as ->
  (b, Rec Identity as)
backpropN = E.backpropN E.zeroFuncs E.oneFunc
{-# INLINE backpropN #-}

-- | 'backpropN', but allows you to provide the gradient of the "final
-- result" with respect to the output of your function.  See 'backpropWith'
-- for more details.
--
-- Note that argument order changed in v0.2.4.
--
-- @since 0.2.0.0
backpropWithN ::
  RPureConstrained Backprop as =>
  (forall s. Reifies s W => Rec (BVar s) as -> BVar s b) ->
  Rec Identity as ->
  (b, b -> Rec Identity as)
backpropWithN = E.backpropWithN E.zeroFuncs
{-# INLINE backpropWithN #-}

-- | Turn a function @'BVar' s a -> 'BVar' s b@ into the function @a -> b@
-- that it represents, also computing its gradient @a@ as well.
--
-- The Rank-N type @forall s. 'Reifies' s 'W' => ...@ is used to ensure
-- that 'BVar's do not leak out of the context (similar to how it is used
-- in "Control.Monad.ST"), and also as a reference to an ephemeral Wengert
-- tape used to track the graph of references.
backprop ::
  (Backprop a, Backprop b) =>
  (forall s. Reifies s W => BVar s a -> BVar s b) ->
  a ->
  (b, a)
backprop = E.backprop E.zeroFunc E.oneFunc
{-# INLINE backprop #-}

-- | A version of 'backprop' that allows you to specify the gradent of your
-- "final result" in with respect to the output of your function.
--
-- Typically, this is just the scalar 1, or a value of components that are
-- all 1.
--
-- Instead of taking the @b@ gradient, the you may provide a @b -> b@,
-- which 'backpropWith' calls with the result of your function as the
-- argument.  This allows you to return something with the correct "shape",
-- if not a scalar.
--
-- 'backprop' is essentially 'backpropWith' with @'const' 1@ for scalars
-- and 'Num' instances.
--
-- Note that argument order changed in v0.2.4
--
-- @since 0.2.0.0
backpropWith ::
  Backprop a =>
  (forall s. Reifies s W => BVar s a -> BVar s b) ->
  a ->
  (b, b -> a)
backpropWith = E.backpropWith E.zeroFunc
{-# INLINE backpropWith #-}

-- | Take a function @'BVar' s a -> 'BVar' s b@, interpreted as a function
-- @a -> b@, and compute its gradient with respect to its input.
--
-- The resulting @a -> a@ tells how the input (and its components) affects
-- the output.  Positive numbers indicate that the result will vary in the
-- same direction as any adjustment in the input.  Negative numbers
-- indicate that the result will vary in the opposite direction as any
-- adjustment in the input.  Larger numbers indicate a greater sensitivity
-- of change, and small numbers indicate lower sensitivity.
--
-- See documentation of 'backprop' for more information.
--
-- If you want to provide an explicit "final gradient" for the end, see
-- 'backpropWith'.
gradBP ::
  (Backprop a, Backprop b) =>
  (forall s. Reifies s W => BVar s a -> BVar s b) ->
  a ->
  a
gradBP = E.gradBP E.zeroFunc E.oneFunc
{-# INLINE gradBP #-}

-- | 'gradBP' generalized to multiple inputs of different types.  See
-- documentation for 'backpropN' for more details.
gradBPN ::
  (RPureConstrained Backprop as, Backprop b) =>
  (forall s. Reifies s W => Rec (BVar s) as -> BVar s b) ->
  Rec Identity as ->
  Rec Identity as
gradBPN = E.gradBPN E.zeroFuncs E.oneFunc
{-# INLINE gradBPN #-}

-- | 'backprop' for a two-argument function.
--
-- Not strictly necessary, because you can always uncurry a function by
-- passing in all of the argument inside a data type, or just use a tuple.
-- However, this could potentially be more performant.
--
-- For 3 and more arguments, consider using 'backpropN'.
backprop2 ::
  (Backprop a, Backprop b, Backprop c) =>
  (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c) ->
  a ->
  b ->
  (c, (a, b))
backprop2 = E.backprop2 E.zeroFunc E.zeroFunc E.oneFunc
{-# INLINE backprop2 #-}

-- | 'backprop2', but allows you to provide the gradient of the "final
-- result" with respect to the output of your function.  See 'backpropWith'
-- for more details.
--
-- Note that argument order changed in v0.2.4
--
-- @since 0.2.0.0
backpropWith2 ::
  (Backprop a, Backprop b) =>
  (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c) ->
  a ->
  b ->
  (c, c -> (a, b))
backpropWith2 = E.backpropWith2 E.zeroFunc E.zeroFunc
{-# INLINE backpropWith2 #-}

-- | 'gradBP' for a two-argument function.  See 'backprop2' for notes.
gradBP2 ::
  (Backprop a, Backprop b, Backprop c) =>
  (forall s. Reifies s W => BVar s a -> BVar s b -> BVar s c) ->
  a ->
  b ->
  (a, b)
gradBP2 = E.gradBP2 E.zeroFunc E.zeroFunc E.oneFunc
{-# INLINE gradBP2 #-}

-- | Create an 'Op' from a backpropagatable function.  Can be useful for
-- "storing" an otherwise Rank-N backpropagatable function in order to
-- avoid impredicative types.  But this is pretty uncommon, so this is
-- mostly just used for low-level internal situations.
--
-- @
-- 'liftOp' . 'bpOp' = 'id'
-- 'bpOp' . 'liftOp' = 'id'
-- @
bpOp ::
  RPureConstrained Backprop as =>
  (forall s. Reifies s W => Rec (BVar s) as -> BVar s b) ->
  Op as b
bpOp = E.bpOp E.zeroFuncs
{-# INLINE bpOp #-}

-- | An infix version of 'viewVar', meant to evoke parallels to '^.' from
-- lens.
--
-- With normal values, you can extract something from that value with
-- a lens:
--
-- @
-- x '^.' myLens
-- @
--
-- would extract a piece of @x :: b@, specified by @myLens :: 'Lens'' b a@.
-- The result has type @a@.
--
-- @
-- xVar '^^.' myLens
-- @
--
-- would extract a piece out of @xVar :: 'BVar' s b@ (a 'BVar' holding a
-- @b@), specified by @myLens :: Lens' b a@.   The result has type @'BVar'
-- s a@ (a 'BVar' holding a @a@)
--
-- This is the main way to pull out values from 'BVar' of container types.
--
-- If you have control of your data type definitions, consider using
-- 'splitBV', which lets you break out 'BVar's of values into 'BVar's of
-- their individual fields automatically without requiring lenses.
--
-- __NOTE__: Usage of '^^.' on many fields from the same item is usually
-- the main source of overhead in /backprop/ code, if you are looking to
-- optimize your code. See <https://backprop.jle.im/07-performance.html
-- this performance guide> for more information, and details on mitigating
-- this overhead.
--
-- __WARNING__: Do not use with any lenses that operate "numerically" on
-- the contents (like 'multiplying').
(^^.) ::
  forall b a s.
  (Backprop b, Backprop a, Reifies s W) =>
  BVar s b ->
  Lens' b a ->
  BVar s a
x ^^. l = viewVar l x

infixl 8 ^^.
{-# INLINE (^^.) #-}

-- | Using a 'Lens'', extract a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to 'view' from lens.
--
-- See documentation for '^^.' for more information, caveats, and warnings.
viewVar ::
  forall b a s.
  (Backprop a, Backprop b, Reifies s W) =>
  Lens' b a ->
  BVar s b ->
  BVar s a
viewVar = E.viewVar E.addFunc E.zeroFunc
{-# INLINE viewVar #-}

-- | An infix version of 'setVar', meant to evoke parallels to '.~' from
-- lens.
--
-- With normal values, you can set something in a value with a lens:
--
-- @
-- x '&' myLens '.~' 'y'
-- @
--
-- would "set" a part of @x :: b@, specified by @myLens :: 'Lens'' a b@, to
-- a new value @y :: a@.
--
-- @
-- xVar '&' myLens '.~~' yVar
-- @
--
-- would "set" a part of @xVar :: 'BVar' s b@ (a 'BVar' holding a @b@),
-- specified by @myLens :: 'Lens'' a b@, to a new value given by @yVar ::
-- 'BVar' s a@.  The result is a new (updated) value of type @'BVar' s b@.
--
-- This is the main way to set values inside 'BVar's of container types.
--
-- Note that this does not incurr the performance overhead issues of
-- 'viewVar' and '^^.', and is fairly cheap.
(.~~) ::
  (Backprop a, Backprop b, Reifies s W) =>
  Lens' b a ->
  BVar s a ->
  BVar s b ->
  BVar s b
l .~~ x = setVar l x

infixl 8 .~~
{-# INLINE (.~~) #-}

-- | Using a 'Lens'', set a value /inside/ a 'BVar'.  Meant to evoke
-- parallels to "set" from lens.
--
-- See documentation for '.~~' for more information.
setVar ::
  (Backprop a, Backprop b, Reifies s W) =>
  Lens' b a ->
  BVar s a ->
  BVar s b ->
  BVar s b
setVar = E.setVar E.addFunc E.addFunc E.zeroFunc
{-# INLINE setVar #-}

-- | An infix version of 'overVar', meant to evoke parallels to '%~' from
-- lens.
--
-- With normal values, you can set modify in a value with a lens:
--
-- @
-- x '&' myLens '%~' 'negate'
-- @
--
-- would "modify" a part of @x :: b@, specified by @myLens :: 'Lens'' a b@,
-- using the function @negate :: a -> a@.
--
-- @
-- xVar '&' myLens '%~~' 'negate'
-- @
--
-- would "modify" a part of @xVar :: 'BVar' s b@ (a 'BVar' holding a @b@),
-- specified by @myLens :: 'Lens'' a b@, using the function @negate :: BVar
-- s a -> BVar s @.  The result is a new (updated) value of type @'BVar'
-- s b@.
--
-- Is essentially a convenient wrapper over a 'viewVar' followed by
-- a 'setVar'.
--
-- @since 0.2.4.0
(%~~) ::
  (Backprop a, Backprop b, Reifies s W) =>
  Lens' b a ->
  (BVar s a -> BVar s a) ->
  BVar s b ->
  BVar s b
l %~~ f = overVar l f

infixr 4 %~~
{-# INLINE (%~~) #-}

-- | Using a 'Lens'', modify a value /inzide/ a 'BVar'.  Meant to evoke
-- parallels to "over" from lens.  See documentation for '%~~' for more
-- information.
--
-- @since 0.2.4.0
overVar ::
  (Backprop a, Backprop b, Reifies s W) =>
  Lens' b a ->
  (BVar s a -> BVar s a) ->
  BVar s b ->
  BVar s b
overVar = E.overVar E.addFunc E.addFunc E.zeroFunc E.zeroFunc
{-# INLINE overVar #-}

-- | An infix version of 'previewVar', meant to evoke parallels to '^?'
-- from lens.
--
-- With normal values, you can (potentially) extract something from that
-- value with a lens:
--
-- @
-- x '^?' myPrism
-- @
--
-- would (potentially) extract a piece of @x :: b@, specified by
-- @myPrism :: 'Traversal'' b a@. The result has type @'Maybe' a@.
--
-- @
-- xVar '^^?' myPrism
-- @
--
-- would (potentially) extract a piece out of @xVar :: 'BVar' s b@ (a
-- 'BVar' holding a @b@), specified by @myPrism :: Prism' b a@.
-- The result has type @'Maybe' ('BVar' s a)@ ('Maybe' a 'BVar' holding
-- a @a@).
--
-- This is intended to be used with 'Prism''s (which hits at most one
-- target), but will actually work with /any/ 'Traversal''.  If the
-- traversal hits more than one target, the first one found will be
-- extracted.
--
-- This can be used to "pattern match" on 'BVar's, by using prisms on
-- constructors.
--
-- __NOTE__: Has the same potential of performance overhead issues as
-- '^^.'; see documentation of '^^.' for more details.
(^^?) ::
  forall b a s.
  (Backprop b, Backprop a, Reifies s W) =>
  BVar s b ->
  Traversal' b a ->
  Maybe (BVar s a)
v ^^? t = previewVar t v

infixl 8 ^^?
{-# INLINE (^^?) #-}

-- | An *UNSAFE* version of '^^?' and 'previewVar' assuming that the value
-- is there.
--
-- Is undefined if the 'Traversal' hits no targets.
--
-- Is essentially '^^?' with 'fromJust', or '^^..' with 'head'.
--
-- @since 0.2.1.0
(^^?!) ::
  forall b a s.
  (Backprop b, Backprop a, Reifies s W) =>
  BVar s b ->
  Traversal' b a ->
  BVar s a
v ^^?! t = fromMaybe (error e) (previewVar t v)
  where
    e = "Numeric.Backprop.^^?!: Empty traversal"

infixl 8 ^^?!
{-# INLINE (^^?!) #-}

-- | Using a 'Traversal'', extract a single value /inside/ a 'BVar', if it
-- exists.  If more than one traversal target exists, returns te first.
-- Meant to evoke parallels to 'preview' from lens.  Really only intended
-- to be used wth 'Prism''s, or up-to-one target traversals.
--
-- See documentation for '^^?' for more information, warnings, and caveats.
previewVar ::
  forall b a s.
  (Backprop b, Backprop a, Reifies s W) =>
  Traversal' b a ->
  BVar s b ->
  Maybe (BVar s a)
previewVar = E.previewVar E.addFunc E.zeroFunc
{-# INLINE previewVar #-}

-- | An infix version of 'toListOfVar', meant to evoke parallels to '^..'
-- from lens.
--
-- With normal values, you can extract all targets of a 'Traversal' from
-- that value with a:
--
-- @
-- x '^..' myTraversal
-- @
--
-- would extract all targets inside of @x :: b@, specified by @myTraversal
-- :: 'Traversal'' b a@. The result has type @[a]@.
--
-- @
-- xVar '^^..' myTraversal
-- @
--
-- would extract all targets inside of @xVar :: 'BVar' s b@ (a 'BVar'
-- holding a @b@), specified by @myTraversal :: Traversal' b a@.   The result
-- has type @['BVar' s a]@ (A list of 'BVar's holding @a@s).
--
-- __NOTE__: Has all of the performance overhead issues of 'sequenceVar';
-- see documentation for 'sequenceVar' for more information.
(^^..) ::
  forall b a s.
  (Backprop b, Backprop a, Reifies s W) =>
  BVar s b ->
  Traversal' b a ->
  [BVar s a]
v ^^.. t = toListOfVar t v
{-# INLINE (^^..) #-}

-- | Using a 'Traversal'', extract all targeted values /inside/ a 'BVar'.
-- Meant to evoke parallels to 'toListOf' from lens.
--
-- See documentation for '^^..' for more information, warnings, and
-- caveats.
toListOfVar ::
  forall b a s.
  (Backprop b, Backprop a, Reifies s W) =>
  Traversal' b a ->
  BVar s b ->
  [BVar s a]
toListOfVar = E.toListOfVar E.addFunc E.zeroFunc
{-# INLINE toListOfVar #-}

-- | Extract all of the 'BVar's out of a 'Traversable' container of
-- 'BVar's.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the gradient is assumed to
-- correspond with the second item in the input, etc.; this can cause
-- unexpected behavior in 'Foldable' instances that don't have a fixed
-- number of items.
--
-- __NOTE__: A potential source of performance overhead.  If there are
-- \(n\) total elements, and you use \(m\) of them, then there is an
-- overhead cost on the order of \(\mathcal{O}(m n)\), with a constant
-- factor dependent on the cost of 'add'.  Should be negligible for types
-- with cheap 'add' (like 'Double'), but may be costly for things like
-- large matrices.  See <https://backprop.jle.im/07-performance.html the
-- performance guide> for for details.
sequenceVar ::
  (Traversable t, Backprop a, Reifies s W) =>
  BVar s (t a) ->
  t (BVar s a)
sequenceVar = E.sequenceVar E.addFunc E.zeroFunc
{-# INLINE sequenceVar #-}

-- | Collect all of the 'BVar's in a container into a 'BVar' of that
-- container's contents.
--
-- Note that this associates gradients in order of occurrence in the
-- original data structure; the second item in the total derivative and
-- gradient is assumed to correspond with the second item in the input,
-- etc.; this can cause unexpected behavior in 'Foldable' instances that
-- don't have a fixed number of items.
--
-- Note that this does __not__ suffer from the same performance overhead
-- issues as 'sequenceVar'.  'collectVar' is \(\mathcal{O}(n)\), with
-- a very small constant factor that consistent for all types.  This
-- reveals a general property of reverse-mode automatic differentiation;
-- "many to one" is cheap, but "one to many" is expensive.
collectVar ::
  (Foldable t, Functor t, Backprop a, Reifies s W) =>
  t (BVar s a) ->
  BVar s (t a)
collectVar = E.collectVar E.addFunc E.zeroFunc
{-# INLINE collectVar #-}

-- | Lift an 'Op' with an arbitrary number of inputs to a function on the
-- appropriate number of 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information, and "Numeric.Backprop.Op#prod" for a mini-tutorial on using
-- 'Rec'.
liftOp ::
  (RPureConstrained Backprop as, Reifies s W) =>
  Op as b ->
  Rec (BVar s) as ->
  BVar s b
liftOp = E.liftOp E.addFuncs
{-# INLINE liftOp #-}

-- | Lift an 'Op' with a single input to be a function on a single 'BVar'.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp1 ::
  (Backprop a, Reifies s W) =>
  Op '[a] b ->
  BVar s a ->
  BVar s b
liftOp1 = E.liftOp1 E.addFunc
{-# INLINE liftOp1 #-}

-- | Lift an 'Op' with two inputs to be a function on a two 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp2 ::
  (Backprop a, Backprop b, Reifies s W) =>
  Op '[a, b] c ->
  BVar s a ->
  BVar s b ->
  BVar s c
liftOp2 = E.liftOp2 E.addFunc E.addFunc
{-# INLINE liftOp2 #-}

-- | Lift an 'Op' with three inputs to be a function on a three 'BVar's.
--
-- Should preferably be used only by libraries to provide primitive 'BVar'
-- functions for their types for users.
--
-- See "Numeric.Backprop#liftops" and documentation for 'liftOp' for more
-- information.
liftOp3 ::
  (Backprop a, Backprop b, Backprop c, Reifies s W) =>
  Op '[a, b, c] d ->
  BVar s a ->
  BVar s b ->
  BVar s c ->
  BVar s d
liftOp3 = E.liftOp3 E.addFunc E.addFunc E.addFunc
{-# INLINE liftOp3 #-}

-- | Convert the value inside a 'BVar' using a given isomorphism.  Useful
-- for things like constructors.
--
-- If you have control of your data type definitions, consider using
-- 'joinBV', which lets you use your data type constructors themselves to
-- join together 'BVar's as their fields.
--
-- Warning: This is unsafe!  It assumes that the isomorphisms themselves
-- have derivative 1, so will break for things like 'exp' & 'log'.
-- Basically, don't use this for any "numeric" isomorphisms.
--
-- @since 0.1.4.0
isoVar ::
  (Backprop a, Reifies s W) =>
  (a -> b) ->
  (b -> a) ->
  BVar s a ->
  BVar s b
isoVar = E.isoVar E.addFunc
{-# INLINE isoVar #-}

-- | Convert the values inside two 'BVar's using a given isomorphism.
-- Useful for things like constructors.  See 'isoVar' for caveats.
--
-- If you have control of your data type definitions, consider using
-- 'joinBV', which lets you use your data type constructors themselves to
-- join together 'BVar's as their fields.
--
-- @since 0.1.4.0
isoVar2 ::
  (Backprop a, Backprop b, Reifies s W) =>
  (a -> b -> c) ->
  (c -> (a, b)) ->
  BVar s a ->
  BVar s b ->
  BVar s c
isoVar2 = E.isoVar2 E.addFunc E.addFunc
{-# INLINE isoVar2 #-}

-- | Convert the values inside three 'BVar's using a given isomorphism.
-- Useful for things like constructors.  See 'isoVar' for caveats.
--
-- @since 0.1.4.0
isoVar3 ::
  (Backprop a, Backprop b, Backprop c, Reifies s W) =>
  (a -> b -> c -> d) ->
  (d -> (a, b, c)) ->
  BVar s a ->
  BVar s b ->
  BVar s c ->
  BVar s d
isoVar3 = E.isoVar3 E.addFunc E.addFunc E.addFunc
{-# INLINE isoVar3 #-}

-- | Convert the values inside a tuple of 'BVar's using a given
-- isomorphism. Useful for things like constructors.  See 'isoVar' for
-- caveats.
--
-- If you have control of your data type definitions, consider using
-- 'joinBV', which lets you use your data type constructors themselves to
-- join together 'BVar's as their fields.
--
-- @since 0.1.4.0
isoVarN ::
  (RPureConstrained Backprop as, Reifies s W) =>
  (Rec Identity as -> b) ->
  (b -> Rec Identity as) ->
  Rec (BVar s) as ->
  BVar s b
isoVarN = E.isoVarN E.addFuncs
{-# INLINE isoVarN #-}

-- | Useful pattern for constructing and deconstructing 'BVar's of
-- two-tuples.
--
-- @since 0.2.1.0
pattern T2 ::
  (Backprop a, Backprop b, Reifies s W) =>
  BVar s a ->
  BVar s b ->
  BVar s (a, b)
pattern T2 x y <- (\xy -> (xy ^^. _1, xy ^^. _2) -> (x, y))
  where
    T2 = isoVar2 (,) id
#if MIN_VERSION_base(4,10,0)
{-# COMPLETE BV #-}
#endif

-- | Useful pattern for constructing and deconstructing 'BVar's
-- three-tuples.
--
-- @since 0.2.1.0
pattern T3 ::
  (Backprop a, Backprop b, Backprop c, Reifies s W) =>
  BVar s a ->
  BVar s b ->
  BVar s c ->
  BVar s (a, b, c)
pattern T3 x y z <- (\xyz -> (xyz ^^. _1, xyz ^^. _2, xyz ^^. _3) -> (x, y, z))
  where
    T3 = isoVar3 (,,) id
#if MIN_VERSION_base(4,10,0)
{-# COMPLETE BV #-}
#endif

-- $hkd
--
-- 'splitBV' and 'joinBV' let you split out a 'BVar' of a data type and
-- join together a data type of 'BVar's using the "higher-kinded data type"
-- technique, a la
-- <http://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- It will let you take a data type like
--
-- @
-- data MyType = MT { mtX :: 'Double', mtY :: [Double] }
--
-- -- | Automatic instance
-- instance Backprop MyType
-- @
--
-- And automatically let you turn a @'BVar' s MyType@ into a @'BVar'
-- s 'Double'@ and @BVar s [Double]@, without munging around with lenses
-- and 'viewVar'.  It'll also let you take a @BVar s Double@ and a @BVar
-- s [Double]@ and turn it into a @BVar s MyType@ without messing around
-- with manually lifting ops or 'isoVar'.
--
-- To do this, rewrite 'MyType' to take a 'Functor' argument:
--
-- @
-- -- | Can be re-used for every data type you use this trick with
-- type family HKD f a where
--     HKD 'Identity' a = a
--     HKD f        a =  f a
--
-- data MyType' f = MT { mtX :: HKD f Double, mtY :: HKD f [Double] }
--   deriving Generic
--
-- -- | This is the original data type, which can be used the same way as
-- -- before
-- type MyType = MyType' 'Identity'
--
-- -- | Automatic instance
-- instance 'Backprop' MyType
-- @
--
-- Now, 'splitBV' can be used, with type:
--
-- @
-- 'splitBV' :: BVar s MyType -> MyType' (BVar s)
-- @
--
-- So you can use it lke:
--
-- @
-- myFunction :: 'BVar' s MyType -> BVar s Double
-- myFunction ('splitBV' -> MT x y) =  x + 'Prelude.Backprop.sum' y
-- @
--
-- Or also, using the 'BV' pattern synonym:
--
-- @
-- myFunction :: 'BVar' s MyType -> BVar s Double
-- myFunction ('BV' (MT x y)) =  x + 'Prelude.Backprop.sum' y
-- @
--
-- If you use 'splitBV', the contents will be a @BVar s Double@ and a @BVar
-- s [Double]@.  It lets you "extract" the fields, because your 'MyType''
-- constructor now holds a @'BVar' s Double@ and a @BVar s [Double]@,
-- instead of just a normal 'Double' and @[Double]@.
--
-- Note that access using 'splitBV' and pattern matching is slightly slower
-- than access using lenses (by about 10-20%).
--
-- With this trick, 'joinBV' can also be used, with the type:
--
-- @
-- 'joinBV' :: MyType' (BVar s) -> BVar s MyType
-- @
--
-- So you can take a bunch of 'BVar's and turn them into a 'BVar' of
-- a 'MyType':
--
-- @
-- myOtherFunction :: 'BVar' s Double -> BVar s [Double] -> BVar s MyType
-- myOtherFunction x y = 'joinBV' $ MT x y
-- @
--
-- The 'BV' pattern synonym abstracts over manual application of 'splitBV'
-- and 'joinBV' as a pattern.
--
-- This will work with all data types made with a single constructor, whose
-- fields are all instances of 'Backprop', where the type itself has an
-- instance of 'Backprop'.

-- | Split out a 'BVar' of "higher-kinded data type", a la
-- <http://reasonablypolymorphic.com/blog/higher-kinded-data/>
--
-- Lets you take 'BVar' of a value into a separate 'BVar' of every field of
-- that value.
--
-- See "Numeric.Backprop#hkd" for a tutorial on usage.
--
-- This will work with all data types made with a single constructor, whose
-- fields are all instances of 'Backprop', where the type itself has an
-- instance of 'Backprop'.  The type also must derive 'Generic'.
--
-- Note that access using 'splitBV' and pattern matching is slightly slower
-- than access using lenses (by about 10-20%).
--
-- See also 'BV', pattern synonym version where the deconstructor is
-- exactly a view into 'splitBV'.
--
-- __NOTE__: Like '^^.' and 'viewVar', 'splitBV' usage could potentially be
-- the main source of performance overhead in your program.  If your data
-- type has \(n\) fields, and you use 'splitBV' to later use \(m\) of those
-- fields, there is an overhead cost on the order of \(\mathcal{O}(m n)\),
-- with a constant factor dependent on the cost of 'add' for your original
-- data type.  Should be negligible for types with cheap 'add' (like
-- 'Double'), but may be costly for things like large matrices.  See
-- <https://backprop.jle.im/07-performance.html the performance guide> for
-- for details.
--
-- However, there is some potential opportunities to re-write some core
-- library functionality that would allow 'splitBV' to avoid all of the
-- significant performance overhead issues of '^^.'.  Contact me if you are
-- interested in helping out!
--
-- @since 0.2.2.0
splitBV ::
  ( Generic (z f)
  , Generic (z (BVar s))
  , E.BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
  , Backprop (z f)
  , Backprop (Rep (z f) ())
  , RPureConstrained Backprop as
  , Reifies s W
  ) =>
  -- | 'BVar' of value
  BVar s (z f) ->
  -- | 'BVar's of fields
  z (BVar s)
splitBV = E.splitBV E.addFunc E.addFuncs E.zeroFunc E.zeroFuncs
{-# INLINE splitBV #-}

-- | Assemble a 'BVar' of "higher-kinded data type", a la
-- <http://reasonablypolymorphic.com/blog/higher-kinded-data/>
--
-- It lets you take a 'BVar' of every field of a value, and join them into
-- a 'BVar' of that value.
--
-- See "Numeric.Backprop#hkd" for a tutorial on usage.
--
-- This will work with all data types made with a single constructor, whose
-- fields are all instances of 'Backprop', where the type itself has an
-- instance of 'Backprop'.
--
-- See also 'BV', a pattern synonym version where the constructor is
-- exactly 'joinBV'.
--
-- Note that 'joinBV' does not suffer the major performance overhead issues
-- of 'splitBV'.  This is a general property of reverse-mode automatic
-- differentiation: "many to one" is cheap, but "one to many" is expensive.
--
-- @since 0.2.2.0
joinBV ::
  ( Generic (z f)
  , Generic (z (BVar s))
  , E.BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
  , Backprop (z f)
  , Backprop (Rep (z f) ())
  , RPureConstrained Backprop as
  , Reifies s W
  ) =>
  -- | 'BVar's of fields
  z (BVar s) ->
  -- | 'BVar' of combined value
  BVar s (z f)
joinBV = E.joinBV E.addFunc E.addFuncs E.zeroFunc E.zeroFuncs
{-# INLINE joinBV #-}

-- | Pattern synonym wrapping manual usage of 'splitBV' and 'joinBV'.  It
-- is a pattern for a @'BVar' s (z f)@ containing a @z ('BVar' s)@
--
-- @since 0.2.3.0
pattern BV ::
  ( Generic (z f)
  , Generic (z (BVar s))
  , E.BVGroup s as (Rep (z f)) (Rep (z (BVar s)))
  , Backprop (Rep (z f) ())
  , Backprop (z f)
  , RPureConstrained Backprop as
  , RecApplicative as
  , Reifies s W
  )
#if MIN_VERSION_base(4,10,0)
    => z (BVar s)           -- ^ 'BVar's of fields
    -> BVar s (z f)         -- ^ 'BVar' of combined value
#else
    => z (BVar s)
    -> BVar s (z f)
#endif
pattern BV v <- (splitBV -> v)
  where
    BV = joinBV
#if MIN_VERSION_base(4,10,0)
{-# COMPLETE BV #-}
#endif
