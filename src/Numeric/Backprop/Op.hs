{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Numeric.Backprop.Op
-- Copyright   : (c) Justin Le 2018
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

module Numeric.Backprop.Op (
  -- * Implementation
  -- $opdoc
  -- * Types
  -- ** Op and Synonyms
    Op(..)
  -- ** Tuple Types#prod#
  -- $prod
  , Prod(..), Tuple, I(..)
  -- * Running
  -- ** Pure
  , runOp, evalOp, gradOp, gradOpWith
  -- * Creation
  , op0, opConst, idOp
  , opConst'
  -- ** Giving gradients directly
  , op1, op2, op3
  -- ** Automatic creation using the /ad/ library
  , op1', op2', op3', opN'
  , Replicate
  -- ** From Isomorphisms
  , opCoerce, opTup, opIso, opLens
  -- * Manipulation
  , composeOp, composeOp1, (~.)
  , composeOp', composeOp1'
  -- * Utility
  , pattern (:>), only, head'
  , pattern (::<), only_
  -- ** Numeric Ops#numops#
  -- $numops
  , (+.), (-.), (*.), negateOp, absOp, signumOp
  , (/.), recipOp
  , expOp, logOp, sqrtOp, (**.), logBaseOp
  , sinOp, cosOp, tanOp, asinOp, acosOp, atanOp
  , sinhOp, coshOp, tanhOp, asinhOp, acoshOp, atanhOp
  ) where

import           Data.Bifunctor
import           Data.Coerce
import           Data.Reflection                (Reifies)
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Util
import           Data.Type.Vector hiding        (head')
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric.AD
import           Numeric.AD.Internal.Reverse    (Reverse, Tape)
import           Numeric.AD.Mode.Forward hiding (grad')
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness

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
--     2. An @a -> Tuple as@:  A function that, when given
--     \(\frac{dz}{dy}\), returns the total gradient
--     \(\nabla_z \mathbf{x}\).
--
-- This is done so that 'Op's can easily be "chained" together, one after
-- the other.  If you have an 'Op' for \(f\) and an 'Op' for \(g\), you can
-- compute the gradient of \(f\) knowing that the result target is
-- \(g \circ f\).
--
-- Note that end users should probably never be required to construct an
-- 'Op' explicitly this way.  Instead, libraries should provide
-- carefuly pre-constructed ones, or provide ways to generate them
-- automatically (like 'op1', 'op2', and 'op3' here).
--
-- For examples of 'Op's implemented from scratch, see the implementations
-- of '+.', '-.', 'recipOp', 'sinOp', etc.
--
-- See "Numeric.Backprop.Op#prod" for a mini-tutorial on using 'Prod' and
-- 'Tuple'.

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
-- the 'op2', 'op1', 'op2', and 'op3' helper smart constructors  And,
-- if your function is a numeric function, they can even be created
-- automatically using 'op1'', 'op2'', 'op3'', and 'opN' with a little help
-- from "Numeric.AD" from the /ad/ library.
--
-- See "Numeric.Backprop.Op#prod" for a mini-tutorial on using 'Prod' and
-- 'Tuple'.
newtype Op as a =
    -- | Construct an 'Op' by giving a function creating the
    -- result, and also a continuation on how to create the gradient, given
    -- the total derivative of @a@.
    --
    -- See the module documentation for "Numeric.Backprop.Op" for more
    -- details on the function that this constructor and 'Op' expect.
    Op { -- | Run the function that the 'Op' encodes, returning
         -- a continuation to compute the gradient, given the total
         -- derivative of @a@.  See documentation for "Numeric.Backprop.Op"
         -- for more information.
         runOpWith :: Tuple as -> (a, a -> Tuple as)
       }

-- | Helper wrapper used for the implementation of 'composeOp'.
newtype OpCont as a = OC { runOpCont :: a -> Tuple as }

-- | A version of 'composeOp' taking explicit 'Length', indicating the
-- number of inputs expected and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the the expected input tuple.  If you ever actually explicitly write
-- down @as@ as a list of types, you should be able to just use
-- 'composeOp'.
composeOp'
    :: Every Num as
    => Length as
    -> Prod (Op as) bs   -- ^ 'Prod' of 'Op's taking @as@ and returning
                         --     different @b@ in @bs@
    -> Op bs c           -- ^ 'OpM' taking eac of the @bs@ from the
                         --     input 'Prod'.
    -> Op as c           -- ^ Composed 'Op'
composeOp' l os o = Op $ \xs ->
    let (ys, conts) = unzipP
                    . map1 ((\(x, c) -> I x :&: OC c) . flip runOpWith xs)
                    $ os
        (z, gFz) = runOpWith o ys
        gFunc g0 =
          let g1 = gFz g0
              g2s = toList (\(oc :&: I g) -> runOpCont oc g)
                  $ conts `zipP` g1
          in  imap1 (\i gs -> I (sum gs) \\ every @_ @Num i)
                 . foldr (\x -> map1 (uncurryFan (\(I y) -> (y:))) . zipP x)
                         (lengthProd [] l)
                 $ g2s
    in (z, gFunc)

-- | Compose 'Op's together, like 'sequence' for functions, or @liftAN@.
--
-- That is, given an @'Op' as b1@, an @'Op' as b2@, and an @'Op' as b3@, it
-- can compose them with an @'Op' '[b1,b2,b3] c@ to create an @'Op' as
-- c@.
composeOp
    :: (Every Num as, Known Length as)
    => Prod (Op as) bs   -- ^ 'Prod' of 'Op's taking @as@ and returning
                         --     different @b@ in @bs@
    -> Op bs c           -- ^ 'Op' taking eac of the @bs@ from the
                         --     input 'Prod'.
    -> Op as c           -- ^ Composed 'Op'
composeOp = composeOp' known

-- | A version of 'composeOp1' taking explicit 'Length', indicating the
-- number of inputs expected and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the the expected input tuple.  If you ever actually explicitly write
-- down @as@ as a list of types, you should be able to just use
-- 'composeOp1'.
composeOp1'
    :: Every Num as
    => Length as
    -> Op as b
    -> Op '[b] c
    -> Op as c
composeOp1' l = composeOp' l . only

-- | Convenient wrapper over 'composeOp' for the case where the second
-- function only takes one input, so the two 'Op's can be directly piped
-- together, like for '.'.
composeOp1
    :: (Every Num as, Known Length as)
    => Op as b
    -> Op '[b] c
    -> Op as c
composeOp1 = composeOp1' known

-- | Convenient infix synonym for (flipped) 'composeOp1'.  Meant to be used
-- just like '.':
--
-- @
-- 'op1' negate            :: 'Op' '[a]   a
-- 'op2' (+)               :: Op '[a,a] a
--
-- op1 negate '~.' op2 (+) :: Op '[a, a] a
-- @
infixr 9 ~.
(~.)
    :: (Known Length as, Every Num as)
    => Op '[b] c
    -> Op as b
    -> Op as c
(~.) = flip composeOp1
{-# INLINE (~.) #-}


-- | Run the function that an 'Op' encodes, to get the result.
--
-- >>> runOp (op2 (*)) (3 ::< 5 ::< Ø)
-- 15
evalOp :: Op as a -> Tuple as -> a
evalOp o = fst . runOpWith o
{-# INLINE evalOp #-}

-- | Run the function that an 'Op' encodes, to get the resulting output and
-- also its gradient with respect to the inputs.
--
-- >>> gradOp' (op2 (*)) (3 ::< 5 ::< Ø)
-- (15, 5 ::< 3 ::< Ø)
runOp :: Num a => Op as a -> Tuple as -> (a, Tuple as)
runOp o = second ($ 1) . runOpWith o
{-# INLINE runOp #-}

-- | Get the gradient function that an 'Op' encodes, with a third argument
-- expecting the total derivative of the result.
--
-- See the module documentaiton for "Numeric.Backprop.Op" for more
-- information.
gradOpWith
    :: Op as a      -- ^ 'Op' to run
    -> Tuple as     -- ^ Inputs to run it with
    -> a            -- ^ The total derivative of the result.
    -> Tuple as     -- ^ The gradient
gradOpWith o = snd . runOpWith o
{-# INLINE gradOpWith #-}

-- | Run the function that an 'Op' encodes, and get the gradient of the
-- output with respect to the inputs.
--
-- >>> gradOp (op2 (*)) (3 ::< 5 ::< Ø)
-- 5 ::< 3 ::< Ø
-- -- the gradient of x*y is (y, x)
--
-- @
-- 'gradOp' o xs = 'gradOpWith' o xs 1
-- @
--
gradOp :: Num a => Op as a -> Tuple as -> Tuple as
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

-- | An 'Op' that just returns whatever it receives.  The identity
-- function.
--
-- @
-- 'idOp' = 'opIso' 'id' 'id'
-- @
idOp :: Op '[a] a
idOp = op1 $ \x -> (x, id)
{-# INLINE idOp #-}

-- | An 'Op' that takes @as@ and returns exactly the input tuple.
--
-- >>> gradOp' opTup (1 ::< 2 ::< 3 ::< Ø)
-- (1 ::< 2 ::< 3 ::< Ø, 1 ::< 1 ::< 1 ::< Ø)
opTup :: Op as (Tuple as)
opTup = Op $ \xs -> (xs, id)
{-# INLINE opTup #-}

-- | An 'Op' that runs the input value through an isomorphism.
--
-- Warning: This is unsafe!  It assumes that the isomorphisms themselves
-- have derivative 1, so will break for things like
-- 'Numeric.Lens.exponentiating'.  Basically, don't use this for any
-- "numeric" isomorphisms.
opIso :: (a -> b) -> (b -> a) -> Op '[ a ] b
opIso to' from' = op1 $ \x -> (to' x, from')
{-# INLINE opIso #-}

-- | An 'Op' that extracts a value from an input value using a 'Lens''.
--
-- Warning: This is unsafe!  It assumes that it extracts a specific value
-- unchanged, with derivative 1, so will break for things that numerically
-- manipulate things before returning them.
opLens :: Num a => Lens' a b -> Op '[ a ] b
opLens l = op1 $ \x -> (view l x, \d -> set l d 0)
{-# INLINE opLens #-}

-- | A version of 'opConst' taking explicit 'Length', indicating the
-- number of inputs and their types.
--
-- Requiring an explicit 'Length' is mostly useful for rare "extremely
-- polymorphic" situations, where GHC can't infer the type and length of
-- the the expected input tuple.  If you ever actually explicitly write
-- down @as@ as a list of types, you should be able to just use
-- 'opConst'.
opConst' :: Every Num as => Length as -> a -> Op as a
opConst' l x = Op $ const
    (x , const $ map1 ((0 \\) . every @_ @Num) (indices' l))
{-# INLINE opConst' #-}

-- | An 'Op' that ignores all of its inputs and returns a given constant
-- value.
--
-- >>> gradOp' (opConst 10) (1 ::< 2 ::< 3 ::< Ø)
-- (10, 0 ::< 0 ::< 0 ::< Ø)
opConst :: (Every Num as, Known Length as) => a -> Op as a
opConst = opConst' known
{-# INLINE opConst #-}

-- | Create an 'Op' that takes no inputs and always returns the given
-- value.
--
-- There is no gradient, of course (using 'gradOp' will give you an empty
-- tuple), because there is no input to have a gradient of.
--
-- >>> runOp (op0 10) Ø
-- (10, Ø)
--
-- For a constant 'Op' that takes input and ignores it, see 'opConst' and
-- 'opConst''.
op0 :: a -> Op '[] a
op0 x = Op $ \case
    Ø -> (x, const Ø)
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
-- square = 'op1'' $ \\x -> (x*x, \\d -> 2 * d * x
--                       )
-- @
--
-- Remember that, generally, end users shouldn't directly construct 'Op's;
-- they should be provided by libraries or generated automatically.
--
-- For numeric functions, single-input 'Op's can be generated automatically
-- using 'op1''.
op1
    :: (a -> (b, b -> a))
    -> Op '[a] b
op1 f = Op $ \case
    I x :< Ø ->
      let (y, dx) = f x
      in  (y, \(!d) -> only_ . dx $ d)
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
--
-- For numeric functions, two-input 'Op's can be generated automatically
-- using 'op2''.
op2
    :: (a -> b -> (c, c -> (a, b)))
    -> Op '[a,b] c
op2 f = Op $ \case
    I x :< I y :< Ø ->
      let (z, dxdy) = f x y
      in  (z, (\(!dx,!dy) -> dx ::< dy ::< Ø) . dxdy)
{-# INLINE op2 #-}

-- | Create an 'Op' of a function taking three inputs, by giving its explicit
-- gradient.  See documentation for 'op2' for more details.
op3
    :: (a -> b -> c -> (d, d -> (a, b, c)))
    -> Op '[a,b,c] d
op3 f = Op $ \case
    I x :< I y :< I z :< Ø ->
      let (q, dxdydz) = f x y z
      in  (q, (\(!dx, !dy, !dz) -> dx ::< dy ::< dz ::< Ø) . dxdydz)
{-# INLINE op3 #-}

-- | Automatically create an 'Op' of a numerical function taking one
-- argument.  Uses 'Numeric.AD.diff', and so can take any numerical
-- function polymorphic over the standard numeric types.
--
-- >>> gradOp' (op1' (recip . negate)) (5 ::< Ø)
-- (-0.2, 0.04 ::< Ø)
op1' :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Op '[a] a
op1' f = op1 $ \x ->
    let (z, dx) = diff' f x
    in  (z, (* dx))
{-# INLINE op1' #-}

-- | Automatically create an 'Op' of a numerical function taking two
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical function
-- polymorphic over the standard numeric types.
--
-- >>> gradOp' (op2' (\x y -> x * sqrt y)) (3 ::< 4 ::< Ø)
-- (6.0, 2.0 ::< 0.75 ::< Ø)
op2' :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a] a
op2' f = opN' $ \case I x :* I y :* ØV -> f x y
{-# INLINE op2' #-}

-- | Automatically create an 'Op' of a numerical function taking three
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical function
-- polymorphic over the standard numeric types.
--
-- >>> gradOp' (op3' (\x y z -> (x * sqrt y)**z)) (3 ::< 4 ::< 2 ::< Ø)
-- (36.0, 24.0 ::< 9.0 ::< 64.503 ::< Ø)
op3' :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a,a] a
op3' f = opN' $ \case I x :* I y :* I z :* ØV -> f x y z
{-# INLINE op3' #-}

-- | Automatically create an 'Op' of a numerical function taking multiple
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical
-- function polymorphic over the standard numeric types.
--
-- >>> gradOp' (opN' (\(x :+ y :+ Ø) -> x * sqrt y)) (3 ::< 4 ::< Ø)
-- (6.0, 2.0 ::< 0.75 ::< Ø)
opN' :: (Num a, Known Nat n)
    => (forall s. Reifies s Tape => Vec n (Reverse s a) -> Reverse s a)
    -> Op (Replicate n a) a
opN' f = Op $ \xs ->
    let (y, dxs) = grad' f (prodToVec' known xs)
    in  (y, vecToProd . \q -> (q *) <$> dxs)
{-# INLINE opN' #-}

instance (Known Length as, Every Num as, Num a) => Num (Op as a) where
    o1 + o2       = composeOp (o1 :< o2 :< Ø) (+.)
    {-# INLINE (+) #-}
    o1 - o2       = composeOp (o1 :< o2 :< Ø) (-.)
    {-# INLINE (-) #-}
    o1 * o2       = composeOp (o1 :< o2 :< Ø) (*.)
    {-# INLINE (*) #-}
    negate o      = composeOp (o  :< Ø)       negateOp
    {-# INLINE negate #-}
    signum o      = composeOp (o  :< Ø)       signumOp
    {-# INLINE signum #-}
    abs    o      = composeOp (o  :< Ø)       absOp
    {-# INLINE abs #-}
    fromInteger x = opConst (fromInteger x)
    {-# INLINE fromInteger #-}

instance (Known Length as, Every Fractional as, Every Num as, Fractional a) => Fractional (Op as a) where
    o1 / o2        = composeOp (o1 :< o2 :< Ø) (/.)
    recip o        = composeOp (o  :< Ø)       recipOp
    {-# INLINE recip #-}
    fromRational x = opConst (fromRational x)
    {-# INLINE fromRational #-}

instance (Known Length as, Every Floating as, Every Fractional as, Every Num as, Floating a) => Floating (Op as a) where
    pi            = opConst pi
    {-# INLINE pi #-}
    exp   o       = composeOp (o  :< Ø)       expOp
    {-# INLINE exp #-}
    log   o       = composeOp (o  :< Ø)       logOp
    {-# INLINE log #-}
    sqrt  o       = composeOp (o  :< Ø)       sqrtOp
    {-# INLINE sqrt #-}
    o1 ** o2      = composeOp (o1 :< o2 :< Ø) (**.)
    {-# INLINE (**) #-}
    logBase o1 o2 = composeOp (o1 :< o2 :< Ø) logBaseOp
    {-# INLINE logBase #-}
    sin   o       = composeOp (o  :< Ø)       sinOp
    {-# INLINE sin #-}
    cos   o       = composeOp (o  :< Ø)       cosOp
    {-# INLINE cos #-}
    tan   o       = composeOp (o  :< Ø)       tanOp
    {-# INLINE tan #-}
    asin  o       = composeOp (o  :< Ø)       asinOp
    {-# INLINE asin #-}
    acos  o       = composeOp (o  :< Ø)       acosOp
    {-# INLINE acos #-}
    atan  o       = composeOp (o  :< Ø)       atanOp
    {-# INLINE atan #-}
    sinh  o       = composeOp (o  :< Ø)       sinhOp
    {-# INLINE sinh #-}
    cosh  o       = composeOp (o  :< Ø)       coshOp
    {-# INLINE cosh #-}
    tanh  o       = composeOp (o  :< Ø)       tanhOp
    {-# INLINE tanh #-}
    asinh o       = composeOp (o  :< Ø)       asinhOp
    {-# INLINE asinh #-}
    acosh o       = composeOp (o  :< Ø)       acoshOp
    {-# INLINE acosh #-}
    atanh o       = composeOp (o  :< Ø)       atanhOp
    {-# INLINE atanh #-}

-- $numops
--
-- Built-in ops for common numeric operations, implemented directly so
-- that they are more efficient than using 'op1'' \/ 'op2'' etc.
--
-- The naming scheme is:
--
-- @
-- ('+.') = 'op2'' ('+')
-- 'negateOp' = 'op1'' 'negate
-- @
--
-- Note that the operators (like '+.') are meant to be used in prefix
-- form, like:
--
-- @
-- 'Numeric.Backprop.liftB2' ('.+') v1 v2
-- @

-- | Optimized version of @'op1'' ('+')@.
(+.) :: Num a => Op '[a, a] a
(+.) = op2 $ \x y -> (x + y, \g -> (g, g))
{-# INLINE (+.) #-}

-- | Optimized version of @'op1'' ('-')@.
(-.) :: Num a => Op '[a, a] a
(-.) = op2 $ \x y -> (x - y, \g -> (g, -g))
{-# INLINE (-.) #-}

-- | Optimized version of @'op1'' ('*')@.
(*.) :: Num a => Op '[a, a] a
(*.) = op2 $ \x y -> (x * y, \g -> (y*g, x*g))
{-# INLINE (*.) #-}

-- | Optimized version of @'op1'' ('/')@.
(/.) :: Fractional a => Op '[a, a] a
(/.) = op2 $ \x y -> (x / y, \g -> (g/y, -g*x/(y*y)))
{-# INLINE (/.) #-}

-- | Optimized version of @'op1'' ('**')@.
(**.) :: Floating a => Op '[a, a] a
(**.) = op2 $ \x y -> ( x ** y
                      , let dx = y*x**(y-1)
                            dy = x**y*log x
                        in  \g -> (g*dx, g*dy)
                      )
{-# INLINE (**.) #-}

-- | Optimized version of @'op1'' 'negate'@.
negateOp :: Num a => Op '[a] a
negateOp = op1 $ \x -> (negate x, negate)
{-# INLINE negateOp  #-}

-- | Optimized version of @'op1'' 'signum'@.
signumOp :: Num a => Op '[a] a
signumOp = op1 $ \x -> (signum x, const 0)
{-# INLINE signumOp  #-}

-- | Optimized version of @'op1'' 'abs'@.
absOp :: Num a => Op '[a] a
absOp = op1 $ \x -> (abs x, (* signum x))
{-# INLINE absOp #-}

-- | Optimized version of @'op1'' 'recip'@.
recipOp :: Fractional a => Op '[a] a
recipOp = op1 $ \x -> (recip x, (/(x*x)) . negate)
{-# INLINE recipOp #-}

-- | Optimized version of @'op1'' 'exp'@.
expOp :: Floating a => Op '[a] a
expOp = op1 $ \x -> (exp x, (exp x *))
{-# INLINE expOp #-}

-- | Optimized version of @'op1'' 'log'@.
logOp :: Floating a => Op '[a] a
logOp = op1 $ \x -> (log x, (/x))
{-# INLINE logOp #-}

-- | Optimized version of @'op1'' 'sqrt'@.
sqrtOp :: Floating a => Op '[a] a
sqrtOp = op1 $ \x -> (sqrt x, (/ (2 * sqrt x)))
{-# INLINE sqrtOp #-}

-- | Optimized version of @'op2'' 'logBase'@.
logBaseOp :: Floating a => Op '[a, a] a
logBaseOp = op2 $ \x y -> ( logBase x y
                          , let dx = - logBase x y / (log x * x)
                            in  \g -> (g*dx, g/(y * log x))
                          )
{-# INLINE logBaseOp #-}

-- | Optimized version of @'op1'' 'sin'@.
sinOp :: Floating a => Op '[a] a
sinOp = op1 $ \x -> (sin x, (* cos x))
{-# INLINE sinOp #-}

-- | Optimized version of @'op1'' 'cos'@.
cosOp :: Floating a => Op '[a] a
cosOp = op1 $ \x -> (cos x, (* (-sin x)))
{-# INLINE cosOp #-}

-- | Optimized version of @'op1'' 'tan'@.
tanOp :: Floating a => Op '[a] a
tanOp = op1 $ \x -> (tan x, (/ cos x^(2::Int)))
{-# INLINE tanOp #-}

-- | Optimized version of @'op1'' 'asin'@.
asinOp :: Floating a => Op '[a] a
asinOp = op1 $ \x -> (asin x, (/ sqrt(1 - x*x)))
{-# INLINE asinOp #-}

-- | Optimized version of @'op1'' 'acos'@.
acosOp :: Floating a => Op '[a] a
acosOp = op1 $ \x -> (acos x, (/ sqrt (1 - x*x)) . negate)
{-# INLINE acosOp #-}

-- | Optimized version of @'op1'' 'atan'@.
atanOp :: Floating a => Op '[a] a
atanOp = op1 $ \x -> (atan x, (/ (x*x + 1)))
{-# INLINE atanOp #-}

-- | Optimized version of @'op1'' 'sinh'@.
sinhOp :: Floating a => Op '[a] a
sinhOp = op1 $ \x -> (sinh x, (* cosh x))
{-# INLINE sinhOp #-}

-- | Optimized version of @'op1'' 'cosh'@.
coshOp :: Floating a => Op '[a] a
coshOp = op1 $ \x -> (cosh x, (* sinh x))
{-# INLINE coshOp #-}

-- | Optimized version of @'op1'' 'tanh'@.
tanhOp :: Floating a => Op '[a] a
tanhOp = op1 $ \x -> (tanh x, (/ cosh x^(2::Int)))
{-# INLINE tanhOp #-}

-- | Optimized version of @'op1'' 'asinh'@.
asinhOp :: Floating a => Op '[a] a
asinhOp = op1 $ \x -> (asinh x, (/ sqrt (x*x + 1)))
{-# INLINE asinhOp #-}

-- | Optimized version of @'op1'' 'acosh'@.
acoshOp :: Floating a => Op '[a] a
acoshOp = op1 $ \x -> (acosh x, (/ sqrt (x*x - 1)))
{-# INLINE acoshOp #-}

-- | Optimized version of @'op1'' 'atanh'@.
atanhOp :: Floating a => Op '[a] a
atanhOp = op1 $ \x -> (atanh x, (/ (1 - x*x)))
{-# INLINE atanhOp #-}

-- $prod
--
-- 'Prod', from the <http://hackage.haskell.org/package/type-combinators
-- type-combinators> library (in "Data.Type.Product") is a heterogeneous
-- list/tuple type, which allows you to tuple together multiple values of
-- different types and operate on them generically.
--
-- A @'Prod' f '[a, b, c]@ contains an @f a@, an @f b@, and an @f c@, and
-- is constructed by consing them together with ':<' (using 'Ø' as nil):
--
-- @
-- 'I' "hello" ':<' I True :< I 7.8 :< Ø    :: 'Prod' 'I' '[String, Bool, Double]
-- 'C' "hello" :< C "world" :< C "ok" :< Ø  :: 'Prod' ('C' String) '[a, b, c]
-- 'Proxy' :< Proxy :< Proxy :< Ø           :: 'Prod' 'Proxy' '[a, b, c]
-- @
--
-- ('I' is the identity functor, and 'C' is the constant functor)
--
-- So, in general:
--
-- @
-- x :: f a
-- y :: f b
-- z :: f c
-- x :< y :< z :< Ø :: Prod f '[a, b, c]
-- @
--
-- If you're having problems typing 'Ø', you can use 'only':
--
-- @
-- only z           :: Prod f '[c]
-- x :< y :< only z :: Prod f '[a, b, c]
-- @
--
-- 'Tuple' is provided as a convenient type synonym for 'Prod' 'I', and has
-- a convenient pattern synonym '::<' (and 'only_'), which can also be used
-- for pattern matching:
--
-- @
-- x :: a
-- y :: b
-- z :: c
--
-- 'only_' z             :: 'Tuple' '[c]
-- x '::<' y ::< z ::< Ø :: 'Tuple' '[a, b, c]
-- x ::< y ::< only_ z :: 'Tuple' '[a, b, c]
-- @


