{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Numeric.Backprop.Op
-- Copyright   : (c) Justin Le 2023
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the 'Op' type and combinators, which represent differentiable
-- functions/operations on values, and are used internally by the library
-- to perform back-propagation.
--
-- Users of the library can ignore this module for the most part. Library
-- authors defining backpropagatable primitives for their functions are
-- recommend to simply use 'op0', 'op1', 'op2', 'op3', which are
-- re-exported in "Numeric.Backprop".  However, authors who want more
-- options in defining their primtive functions might find some of these
-- functions useful.
--
-- Note that if your entire function is a single non-branching composition
-- of functions, 'Op' and its utility functions alone are sufficient to
-- differentiate/backprop.  However, this happens rarely in practice.
--
-- To use these 'Op's with the backprop library, they can be made to work
-- with 'BVar's using 'liftOp', 'liftOp1', 'liftOp2', and 'liftOp3'.
--
-- If you are writing a library, see
-- <https://backprop.jle.im/06-equipping-your-library.html> for a guide for
-- equipping your library with backpropatable operations using 'Op's.
--
-- See also <https://backprop.jle.im/06-manual-gradients.html this guide>
-- for writing Ops manually on your own numerical functions.
module Numeric.Backprop.Op (
  -- * Implementation
  -- $opdoc
  Op (..),

  -- ** Tuple Types#prod#
  -- $prod
  Rec (..),

  -- * Running

  -- ** Pure
  runOp,
  evalOp,
  gradOp,
  gradOpWith,

  -- * Creation
  op0,
  opConst,
  idOp,
  opLens,

  -- ** Giving gradients directly
  op1,
  op2,
  op3,

  -- ** From Isomorphisms
  opCoerce,
  opTup,
  opIso,
  opIso2,
  opIso3,
  opIsoN,

  -- ** No gradient
  noGrad1,
  noGrad,

  -- * Manipulation
  composeOp,
  composeOp1,
  (~.),

  -- * Utility

  -- ** Numeric Ops#numops#
  -- $numops
  (+.),
  (-.),
  (*.),
  negateOp,
  absOp,
  signumOp,
  (/.),
  recipOp,
  expOp,
  logOp,
  sqrtOp,
  (**.),
  logBaseOp,
  sinOp,
  cosOp,
  tanOp,
  asinOp,
  acosOp,
  atanOp,
  sinhOp,
  coshOp,
  tanhOp,
  asinhOp,
  acoshOp,
  atanhOp,
) where

import Control.Applicative
import Data.Bifunctor
import Data.Coerce
import Data.Functor.Identity
import Data.List (foldl')
import Data.Type.Util
import Data.Vinyl.Core
import qualified Data.Vinyl.Recursive as VR
import Lens.Micro
import Lens.Micro.Extras

-- $opdoc
-- 'Op's contain information on a function as well as its gradient, but
-- provides that information in a way that allows them to be "chained".
--
-- For example, for a function
--
-- \[
-- f : \mathbb{R}^n \rightarrow \mathbb{R}
-- \]
--
-- We might want to apply a function \(g\) to the result we get, to get
-- our "final" result:
--
-- \[
-- \eqalign{
-- y &= f(\mathbf{x})\cr
-- z &= g(y)
-- }
-- \]
--
-- Now, we might want the gradient \(\nabla z\) with respect to
-- \(\mathbf{x}\), or \(\nabla_\mathbf{x} z\).  Explicitly, this is:
--
-- \[
-- \nabla_\mathbf{x} z = \left< \frac{\partial z}{\partial x_1}, \frac{\partial z}{\partial x_2}, \ldots \right>
-- \]
--
-- We can compute that by multiplying the total derivative of \(z\) with
-- respect to \(y\) (that is, \(\frac{dz}{dy}\)) with the gradient of
-- \(f\)) itself:
--
-- \[
-- \eqalign{
-- \nabla_\mathbf{x} z &= \frac{dz}{dy} \left< \frac{\partial y}{\partial x_1}, \frac{\partial y}{\partial x_2}, \ldots \right>\cr
-- \nabla_\mathbf{x} z &= \frac{dz}{dy} \nabla_\mathbf{x} y
-- }
-- \]
--
-- So, to create an @'Op' as a@ with the 'Op' constructor, you give
-- a function that returns a tuple, containing:
--
--     1. An @a@: The result of the function
--     2. An @a -> Rec Identity as@:  A function that, when given
--     \(\frac{dz}{dy}\), returns the total gradient
--     \(\nabla_z \mathbf{x}\).
--
-- This is done so that 'Op's can easily be "chained" together, one after
-- the other.  If you have an 'Op' for \(f\) and an 'Op' for \(g\), you can
-- compute the gradient of \(f\) knowing that the result target is
-- \(g \circ f\).
--
-- See <https://backprop.jle.im/06-manual-gradients.html this guide> for
-- a detailed look on writing ops manually on your own numerical functions.
--
-- Note that end users should probably never be required to construct an
-- 'Op' explicitly this way.  Instead, libraries should provide
-- carefuly pre-constructed ones, or provide ways to generate them
-- automatically (like 'op1', 'op2', and 'op3' here).
--
-- For examples of 'Op's implemented from scratch, see the implementations
-- of '+.', '-.', 'recipOp', 'sinOp', etc.
--
-- See "Numeric.Backprop.Op#prod" for a mini-tutorial on using 'Rec' and
-- 'Rec Identity'.

-- | An @'Op' as a@ describes a differentiable function from @as@ to @a@.
--
-- For example, a value of type
--
-- @
-- 'Op' '[Int, Bool] Double
-- @
--
-- is a function from an 'Int' and a 'Bool', returning a 'Double'.  It can
-- be differentiated to give a /gradient/ of an 'Int' and a 'Bool' if given
-- a total derivative for the @Double@.  If we call 'Bool' \(2\), then,
-- mathematically, it is akin to a:
--
-- \[
-- f : \mathbb{Z} \times 2 \rightarrow \mathbb{R}
-- \]
--
-- See 'runOp', 'gradOp', and 'gradOpWith' for examples on how to run it,
-- and 'Op' for instructions on creating it.
--
-- It is simpler to not use this type constructor directly, and instead use
-- the 'op2', 'op1', 'op2', and 'op3' helper smart constructors.
--
-- See "Numeric.Backprop.Op#prod" for a mini-tutorial on using 'Rec' and
-- 'Rec Identity'.
--
-- To /use/ an 'Op' with the backprop library, see 'liftOp', 'liftOp1',
-- 'liftOp2', and 'liftOp3'.
newtype Op as a
  = -- | Construct an 'Op' by giving a function creating the
    -- result, and also a continuation on how to create the gradient, given
    -- the total derivative of @a@.
    --
    -- See the module documentation for "Numeric.Backprop.Op" for more
    -- details on the function that this constructor and 'Op' expect.
    Op
    { runOpWith :: Rec Identity as -> (a, a -> Rec Identity as)
    -- ^ Run the function that the 'Op' encodes, returning
    -- a continuation to compute the gradient, given the total
    -- derivative of @a@.  See documentation for "Numeric.Backprop.Op"
    -- for more information.
    }

-- | Helper wrapper used for the implementation of 'composeOp'.
newtype OpCont as a = OC {runOpCont :: a -> Rec Identity as}

-- | Compose 'Op's together, like 'sequence' for functions, or @liftAN@.
--
-- That is, given an @'Op' as b1@, an @'Op' as b2@, and an @'Op' as b3@, it
-- can compose them with an @'Op' '[b1,b2,b3] c@ to create an @'Op' as
-- c@.
composeOp ::
  forall as bs c.
  RPureConstrained Num as =>
  -- | 'Rec' of 'Op's taking @as@ and returning
  --     different @b@ in @bs@
  Rec (Op as) bs ->
  -- | 'OpM' taking eac of the @bs@ from the
  --     input 'Rec'.
  Op bs c ->
  -- | Composed 'Op'
  Op as c
composeOp os o = Op $ \xs ->
  let (ys, conts) = runzipWith (bimap Identity OC . flip runOpWith xs) os
      (z, gFz) = runOpWith o ys
      gFunc g0 =
        let g1 = gFz g0
            g2s :: Rec (Const (Rec Identity as)) bs
            g2s =
              VR.rzipWith
                (\oc (Identity g) -> Const $ runOpCont oc g)
                conts
                g1
         in VR.rmap (\(Dict x) -> Identity x)
              . foldl'
                ( VR.rzipWith
                    ( \(Dict !x) (Identity y) ->
                        let q = x + y in q `seq` Dict q
                    )
                )
                (rpureConstrained @Num (Dict @Num 0))
              . VR.rfoldMap ((: []) . getConst)
              $ g2s
   in (z, gFunc)

-- | Convenient wrapper over 'composeOp' for the case where the second
-- function only takes one input, so the two 'Op's can be directly piped
-- together, like for '.'.
composeOp1 ::
  RPureConstrained Num as =>
  Op as b ->
  Op '[b] c ->
  Op as c
composeOp1 = composeOp . (:& RNil)

-- | Convenient infix synonym for (flipped) 'composeOp1'.  Meant to be used
-- just like '.':
--
-- @
-- f :: 'Op' '[b]   c
-- g :: 'Op' '[a,a] b
--
-- f '~.' g :: Op '[a, a] c
-- @
infixr 9 ~.

(~.) ::
  RPureConstrained Num as =>
  Op '[b] c ->
  Op as b ->
  Op as c
(~.) = flip composeOp1
{-# INLINE (~.) #-}

-- | Run the function that an 'Op' encodes, to get the result.
--
-- >>> runOp (op2 (*)) (3 :& 5 :& RNil)
-- 15
evalOp :: Op as a -> Rec Identity as -> a
evalOp o = fst . runOpWith o
{-# INLINE evalOp #-}

-- | Run the function that an 'Op' encodes, to get the resulting output and
-- also its gradient with respect to the inputs.
--
-- >>> gradOp' (op2 (*)) (3 :& 5 :& RNil)
-- (15, 5 :& 3 :& RNil)
runOp :: Num a => Op as a -> Rec Identity as -> (a, Rec Identity as)
runOp o = second ($ 1) . runOpWith o
{-# INLINE runOp #-}

-- | Get the gradient function that an 'Op' encodes, with a third argument
-- expecting the total derivative of the result.
--
-- See the module documentaiton for "Numeric.Backprop.Op" for more
-- information.
gradOpWith ::
  -- | 'Op' to run
  Op as a ->
  -- | Inputs to run it with
  Rec Identity as ->
  -- | The total derivative of the result.
  a ->
  -- | The gradient
  Rec Identity as
gradOpWith o = snd . runOpWith o
{-# INLINE gradOpWith #-}

-- | Run the function that an 'Op' encodes, and get the gradient of the
-- output with respect to the inputs.
--
-- >>> gradOp (op2 (*)) (3 :& 5 :& RNil)
-- 5 :& 3 :& RNil
-- -- the gradient of x*y is (y, x)
--
-- @
-- 'gradOp' o xs = 'gradOpWith' o xs 1
-- @
gradOp :: Num a => Op as a -> Rec Identity as -> Rec Identity as
gradOp o i = gradOpWith o i 1
{-# INLINE gradOp #-}

-- | An 'Op' that coerces an item into another item whose type has the same
-- runtime representation.
--
-- >>> gradOp' opCoerce (Identity 5) :: (Int, Identity Int)
-- (5, Identity 1)
--
-- @
-- 'opCoerce' = 'opIso' 'coerced' 'coerce'
-- @
opCoerce :: Coercible a b => Op '[a] b
opCoerce = opIso coerce coerce
{-# INLINE opCoerce #-}

-- | Create an 'Op' with no gradient.  Can be evaluated with 'evalOp',  but
-- will throw a runtime exception when asked for the gradient.
--
-- Can be used with 'BVar' with 'liftOp1', and 'evalBP' will work fine.
-- 'gradBP'  and 'backprop' will also work fine if the result is never used
-- in the final answer, but will throw a runtime exception if the final
-- answer depends on the result of this operation.
--
-- Useful if your only API is exposed through /backprop/.  Just be sure to
-- tell your users that this will explode when finding the gradient if the
-- result is used in the final result.
--
-- @since 0.1.3.0
noGrad1 :: (a -> b) -> Op '[a] b
noGrad1 f = op1 $ \x ->
  ( f x
  , \_ -> errorWithoutStackTrace "Numeric.Backprop.Op.noGrad1: no gradient defined"
  )
{-# INLINE noGrad1 #-}

-- | Create an 'Op' with no gradient.  Can be evaluated with 'evalOp',  but
-- will throw a runtime exception when asked for the gradient.
--
-- Can be used with 'BVar' with 'liftOp', and 'evalBP' will work fine.
-- 'gradBP'  and 'backprop' will also work fine if the result is never used
-- in the final answer, but will throw a runtime exception if the final
-- answer depends on the result of this operation.
--
-- Useful if your only API is exposed through /backprop/.  Just be sure to
-- tell your users that this will explode when finding the gradient if the
-- result is used in the final result.
--
-- @since 0.1.3.0
noGrad :: (Rec Identity as -> b) -> Op as b
noGrad f = Op $ \xs ->
  ( f xs
  , \_ -> errorWithoutStackTrace "Numeric.Backprop.Op.noGrad: no gradient defined"
  )
{-# INLINE noGrad #-}

-- | An 'Op' that just returns whatever it receives.  The identity
-- function.
--
-- @
-- 'idOp' = 'opIso' 'id' 'id'
-- @
idOp :: Op '[a] a
idOp = op1 (,id)
{-# INLINE idOp #-}

-- | An 'Op' that takes @as@ and returns exactly the input tuple.
--
-- >>> gradOp' opTup (1 :& 2 :& 3 :& RNil)
-- (1 :& 2 :& 3 :& RNil, 1 :& 1 :& 1 :& RNil)
opTup :: Op as (Rec Identity as)
opTup = Op (,id)
{-# INLINE opTup #-}

-- | An 'Op' that runs the input value through an isomorphism.
--
-- Warning: This is unsafe!  It assumes that the isomorphisms themselves
-- have derivative 1, so will break for things like 'exp' & 'log'.
-- Basically, don't use this for any "numeric" isomorphisms.
opIso :: (a -> b) -> (b -> a) -> Op '[a] b
opIso to' from' = op1 $ \x -> (to' x, from')
{-# INLINE opIso #-}

-- | An 'Op' that runs the two input values through an isomorphism.  Useful
-- for things like constructors.  See 'opIso' for caveats.
--
-- @since 0.1.4.0
opIso2 :: (a -> b -> c) -> (c -> (a, b)) -> Op '[a, b] c
opIso2 to' from' = op2 $ \x y -> (to' x y, from')
{-# INLINE opIso2 #-}

-- | An 'Op' that runs the three input values through an isomorphism.
-- Useful for things like constructors.  See 'opIso' for caveats.
--
-- @since 0.1.4.0
opIso3 :: (a -> b -> c -> d) -> (d -> (a, b, c)) -> Op '[a, b, c] d
opIso3 to' from' = op3 $ \x y z -> (to' x y z, from')
{-# INLINE opIso3 #-}

-- | An 'Op' that runs the input value through an isomorphism between
-- a tuple of values and a value.  See 'opIso' for caveats.
--
-- In "Numeric.Backprop.Op" since version 0.1.2.0, but only exported from
-- "Numeric.Backprop" since version 0.1.3.0.
--
-- @since 0.1.2.0
opIsoN :: (Rec Identity as -> b) -> (b -> Rec Identity as) -> Op as b
opIsoN to' from' = Op $ \xs -> (to' xs, from')
{-# INLINE opIsoN #-}

-- | An 'Op' that extracts a value from an input value using a 'Lens''.
--
-- Warning: This is unsafe!  It assumes that it extracts a specific value
-- unchanged, with derivative 1, so will break for things that numerically
-- manipulate things before returning them.
opLens :: Num a => Lens' a b -> Op '[a] b
opLens l = op1 $ \x -> (view l x, \d -> set l d 0)
{-# INLINE opLens #-}

-- | An 'Op' that ignores all of its inputs and returns a given constant
-- value.
--
-- >>> gradOp' (opConst 10) (1 :& 2 :& 3 :& RNil)
-- (10, 0 :& 0 :& 0 :& RNil)
opConst ::
  forall as a.
  RPureConstrained Num as =>
  a ->
  Op as a
opConst x =
  Op $
    const
      (x, const $ rpureConstrained @Num 0)
{-# INLINE opConst #-}

-- | Create an 'Op' that takes no inputs and always returns the given
-- value.
--
-- There is no gradient, of course (using 'gradOp' will give you an empty
-- tuple), because there is no input to have a gradient of.
--
-- >>> runOp (op0 10) RNil
-- (10, RNil)
--
-- For a constant 'Op' that takes input and ignores it, see 'opConst' and
-- 'opConst''.
op0 :: a -> Op '[] a
op0 x = Op $ \case
  RNil -> (x, const RNil)
{-# INLINE op0 #-}

-- | Create an 'Op' of a function taking one input, by giving its explicit
-- derivative.  The function should return a tuple containing the result of
-- the function, and also a function taking the derivative of the result
-- and return the derivative of the input.
--
-- If we have
--
-- \[
-- \eqalign{
-- f &: \mathbb{R} \rightarrow \mathbb{R}\cr
-- y &= f(x)\cr
-- z &= g(y)
-- }
-- \]
--
-- Then the derivative \( \frac{dz}{dx} \), it would be:
--
-- \[
-- \frac{dz}{dx} = \frac{dz}{dy} \frac{dy}{dx}
-- \]
--
-- If our 'Op' represents \(f\), then the second item in the resulting
-- tuple should be a function that takes \(\frac{dz}{dy}\) and returns
-- \(\frac{dz}{dx}\).
--
-- As an example, here is an 'Op' that squares its input:
--
-- @
-- square :: Num a => 'Op' '[a] a
-- square = 'op1' $ \\x -> (x*x, \\d -> 2 * d * x
--                      )
-- @
--
-- Remember that, generally, end users shouldn't directly construct 'Op's;
-- they should be provided by libraries or generated automatically.
op1 ::
  (a -> (b, b -> a)) ->
  Op '[a] b
op1 f = Op $ \case
  Identity x :& RNil ->
    let (y, dx) = f x
     in (y, \(!d) -> (:& RNil) . Identity . dx $ d)
{-# INLINE op1 #-}

-- | Create an 'Op' of a function taking two inputs, by giving its explicit
-- gradient.  The function should return a tuple containing the result of
-- the function, and also a function taking the derivative of the result
-- and return the derivative of the input.
--
-- If we have
--
-- \[
-- \eqalign{
-- f &: \mathbb{R}^2 \rightarrow \mathbb{R}\cr
-- z &= f(x, y)\cr
-- k &= g(z)
-- }
-- \]
--
-- Then the gradient \( \left< \frac{\partial k}{\partial x}, \frac{\partial k}{\partial y} \right> \)
-- would be:
--
-- \[
-- \left< \frac{\partial k}{\partial x}, \frac{\partial k}{\partial y} \right> =
--  \left< \frac{dk}{dz} \frac{\partial z}{dx}, \frac{dk}{dz} \frac{\partial z}{dy} \right>
-- \]
--
-- If our 'Op' represents \(f\), then the second item in the resulting
-- tuple should be a function that takes \(\frac{dk}{dz}\) and returns
-- \( \left< \frac{\partial k}{dx}, \frac{\partial k}{dx} \right> \).
--
-- As an example, here is an 'Op' that multiplies its inputs:
--
-- @
-- mul :: Num a => 'Op' '[a, a] a
-- mul = 'op2'' $ \\x y -> (x*y, \\d -> (d*y, x*d)
--                      )
-- @
--
-- Remember that, generally, end users shouldn't directly construct 'Op's;
-- they should be provided by libraries or generated automatically.
op2 ::
  (a -> b -> (c, c -> (a, b))) ->
  Op '[a, b] c
op2 f = Op $ \case
  Identity x :& Identity y :& RNil ->
    let (z, dxdy) = f x y
     in (z, (\(!dx, !dy) -> Identity dx :& Identity dy :& RNil) . dxdy)
{-# INLINE op2 #-}

-- | Create an 'Op' of a function taking three inputs, by giving its explicit
-- gradient.  See documentation for 'op2' for more details.
op3 ::
  (a -> b -> c -> (d, d -> (a, b, c))) ->
  Op '[a, b, c] d
op3 f = Op $ \case
  Identity x :& Identity y :& Identity z :& RNil ->
    let (q, dxdydz) = f x y z
     in (q, (\(!dx, !dy, !dz) -> Identity dx :& Identity dy :& Identity dz :& RNil) . dxdydz)
{-# INLINE op3 #-}

instance (RPureConstrained Num as, Num a) => Num (Op as a) where
  o1 + o2 = composeOp (o1 :& o2 :& RNil) (+.)
  {-# INLINE (+) #-}
  o1 - o2 = composeOp (o1 :& o2 :& RNil) (-.)
  {-# INLINE (-) #-}
  o1 * o2 = composeOp (o1 :& o2 :& RNil) (*.)
  {-# INLINE (*) #-}
  negate o = composeOp (o :& RNil) negateOp
  {-# INLINE negate #-}
  signum o = composeOp (o :& RNil) signumOp
  {-# INLINE signum #-}
  abs o = composeOp (o :& RNil) absOp
  {-# INLINE abs #-}
  fromInteger x = opConst (fromInteger x)
  {-# INLINE fromInteger #-}

instance (RPureConstrained Num as, Fractional a) => Fractional (Op as a) where
  o1 / o2 = composeOp (o1 :& o2 :& RNil) (/.)
  recip o = composeOp (o :& RNil) recipOp
  {-# INLINE recip #-}
  fromRational x = opConst (fromRational x)
  {-# INLINE fromRational #-}

instance (RPureConstrained Num as, Floating a) => Floating (Op as a) where
  pi = opConst pi
  {-# INLINE pi #-}
  exp o = composeOp (o :& RNil) expOp
  {-# INLINE exp #-}
  log o = composeOp (o :& RNil) logOp
  {-# INLINE log #-}
  sqrt o = composeOp (o :& RNil) sqrtOp
  {-# INLINE sqrt #-}
  o1 ** o2 = composeOp (o1 :& o2 :& RNil) (**.)
  {-# INLINE (**) #-}
  logBase o1 o2 = composeOp (o1 :& o2 :& RNil) logBaseOp
  {-# INLINE logBase #-}
  sin o = composeOp (o :& RNil) sinOp
  {-# INLINE sin #-}
  cos o = composeOp (o :& RNil) cosOp
  {-# INLINE cos #-}
  tan o = composeOp (o :& RNil) tanOp
  {-# INLINE tan #-}
  asin o = composeOp (o :& RNil) asinOp
  {-# INLINE asin #-}
  acos o = composeOp (o :& RNil) acosOp
  {-# INLINE acos #-}
  atan o = composeOp (o :& RNil) atanOp
  {-# INLINE atan #-}
  sinh o = composeOp (o :& RNil) sinhOp
  {-# INLINE sinh #-}
  cosh o = composeOp (o :& RNil) coshOp
  {-# INLINE cosh #-}
  tanh o = composeOp (o :& RNil) tanhOp
  {-# INLINE tanh #-}
  asinh o = composeOp (o :& RNil) asinhOp
  {-# INLINE asinh #-}
  acosh o = composeOp (o :& RNil) acoshOp
  {-# INLINE acosh #-}
  atanh o = composeOp (o :& RNil) atanhOp
  {-# INLINE atanh #-}

-- $numops
--
-- Built-in ops for common numeric operations.
--
-- Note that the operators (like '+.') are meant to be used in prefix
-- form, like:
--
-- @
-- 'Numeric.Backprop.liftOp2' ('.+') v1 v2
-- @

-- | 'Op' for addition
(+.) :: Num a => Op '[a, a] a
(+.) = op2 $ \x y -> (x + y, \g -> (g, g))
{-# INLINE (+.) #-}

-- | 'Op' for subtraction
(-.) :: Num a => Op '[a, a] a
(-.) = op2 $ \x y -> (x - y, \g -> (g, -g))
{-# INLINE (-.) #-}

-- | 'Op' for multiplication
(*.) :: Num a => Op '[a, a] a
(*.) = op2 $ \x y -> (x * y, \g -> (y * g, x * g))
{-# INLINE (*.) #-}

-- | 'Op' for division
(/.) :: Fractional a => Op '[a, a] a
(/.) = op2 $ \x y -> (x / y, \g -> (g / y, -(g * x / (y * y))))
{-# INLINE (/.) #-}

-- | 'Op' for exponentiation
(**.) :: Floating a => Op '[a, a] a
(**.) = op2 $ \x y ->
  ( x ** y
  , let dx = y * x ** (y - 1)
        dy = x ** y * log x
     in \g -> (g * dx, g * dy)
  )
{-# INLINE (**.) #-}

-- | 'Op' for negation
negateOp :: Num a => Op '[a] a
negateOp = op1 $ \x -> (negate x, negate)
{-# INLINE negateOp #-}

-- | 'Op' for 'signum'
signumOp :: Num a => Op '[a] a
signumOp = op1 $ \x -> (signum x, const 0)
{-# INLINE signumOp #-}

-- | 'Op' for absolute value
absOp :: Num a => Op '[a] a
absOp = op1 $ \x -> (abs x, (* signum x))
{-# INLINE absOp #-}

-- | 'Op' for multiplicative inverse
recipOp :: Fractional a => Op '[a] a
recipOp = op1 $ \x -> (recip x, (/ (x * x)) . negate)
{-# INLINE recipOp #-}

-- | 'Op' for 'exp'
expOp :: Floating a => Op '[a] a
expOp = op1 $ \x -> (exp x, (exp x *))
{-# INLINE expOp #-}

-- | 'Op' for the natural logarithm
logOp :: Floating a => Op '[a] a
logOp = op1 $ \x -> (log x, (/ x))
{-# INLINE logOp #-}

-- | 'Op' for square root
sqrtOp :: Floating a => Op '[a] a
sqrtOp = op1 $ \x -> (sqrt x, (/ (2 * sqrt x)))
{-# INLINE sqrtOp #-}

-- | 'Op' for 'logBase'
logBaseOp :: Floating a => Op '[a, a] a
logBaseOp = op2 $ \x y ->
  ( logBase x y
  , let dx = -(logBase x y / (log x * x))
     in \g -> (g * dx, g / (y * log x))
  )
{-# INLINE logBaseOp #-}

-- | 'Op' for sine
sinOp :: Floating a => Op '[a] a
sinOp = op1 $ \x -> (sin x, (* cos x))
{-# INLINE sinOp #-}

-- | 'Op' for cosine
cosOp :: Floating a => Op '[a] a
cosOp = op1 $ \x -> (cos x, (* (-sin x)))
{-# INLINE cosOp #-}

-- | 'Op' for tangent
tanOp :: Floating a => Op '[a] a
tanOp = op1 $ \x -> (tan x, (/ cos x ^ (2 :: Int)))
{-# INLINE tanOp #-}

-- | 'Op' for arcsine
asinOp :: Floating a => Op '[a] a
asinOp = op1 $ \x -> (asin x, (/ sqrt (1 - x * x)))
{-# INLINE asinOp #-}

-- | 'Op' for arccosine
acosOp :: Floating a => Op '[a] a
acosOp = op1 $ \x -> (acos x, (/ sqrt (1 - x * x)) . negate)
{-# INLINE acosOp #-}

-- | 'Op' for arctangent
atanOp :: Floating a => Op '[a] a
atanOp = op1 $ \x -> (atan x, (/ (x * x + 1)))
{-# INLINE atanOp #-}

-- | 'Op' for hyperbolic sine
sinhOp :: Floating a => Op '[a] a
sinhOp = op1 $ \x -> (sinh x, (* cosh x))
{-# INLINE sinhOp #-}

-- | 'Op' for hyperbolic cosine
coshOp :: Floating a => Op '[a] a
coshOp = op1 $ \x -> (cosh x, (* sinh x))
{-# INLINE coshOp #-}

-- | 'Op' for hyperbolic tangent
tanhOp :: Floating a => Op '[a] a
tanhOp = op1 $ \x -> (tanh x, (/ cosh x ^ (2 :: Int)))
{-# INLINE tanhOp #-}

-- | 'Op' for hyperbolic arcsine
asinhOp :: Floating a => Op '[a] a
asinhOp = op1 $ \x -> (asinh x, (/ sqrt (x * x + 1)))
{-# INLINE asinhOp #-}

-- | 'Op' for hyperbolic arccosine
acoshOp :: Floating a => Op '[a] a
acoshOp = op1 $ \x -> (acosh x, (/ sqrt (x * x - 1)))
{-# INLINE acoshOp #-}

-- | 'Op' for hyperbolic arctangent
atanhOp :: Floating a => Op '[a] a
atanhOp = op1 $ \x -> (atanh x, (/ (1 - x * x)))
{-# INLINE atanhOp #-}

-- $prod
--
-- 'Rec', from the <http://hackage.haskell.org/package/vinyl vinyl> library
-- (in "Data.Vinyl.Core") is a heterogeneous list/tuple type, which allows
-- you to tuple together multiple values of different types and operate on
-- them generically.
--
-- A @'Rec' f '[a, b, c]@ contains an @f a@, an @f b@, and an @f c@, and
-- is constructed by consing them together with ':&' (using 'RNil' as nil):
--
-- @
-- 'Identity' "hello" ':&' Identity True :& Identity 7.8 :& RNil    :: 'Rec' 'I' '[String, Bool, Double]
-- 'Const' "hello" :& Const "world" :& Const "ok" :& RNil  :: 'Rec' ('C' String) '[a, b, c]
-- 'Proxy' :& Proxy :& Proxy :& RNil           :: 'Rec' 'Proxy' '[a, b, c]
-- @
--
-- So, in general:
--
-- @
-- x :: f a
-- y :: f b
-- z :: f c
-- x :& y :& z :& RNil :: Rec f '[a, b, c]
-- @
