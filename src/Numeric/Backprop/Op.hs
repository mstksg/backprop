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

-- |
-- Module      : Numeric.Backprop.Op
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the 'Op' (and 'OpM') type and combinators, which represent
-- differentiable functions/operations on values, and are used by the
-- library to perform backpropagation.
--
-- Note that 'Op' is a /subset/ or /subtype/ of 'OpM', and so, any function
-- that expects an @'OpM' m as a@ (or an @'Numeric.Backprop.OpB' s as a@)
-- can be given an @'Op' as a@ and it'll work just fine.
--

module Numeric.Backprop.Op (
  -- * Implementation
  -- $opdoc
  -- * Types
  -- ** Op and Synonyms
    Op, pattern Op, OpM(..)
  -- ** Tuple Types
  , Prod(..), Tuple
  -- * Running
  -- ** Pure
  , runOp, gradOp, gradOp', gradOpWith, gradOpWith', runOp'
  -- ** Monadic
  , runOpM, gradOpM, gradOpM', gradOpWithM, gradOpWithM', runOpM'
  -- * Manipulation
  , composeOp
  , composeOp'
  -- * Creation
  , op0, opConst
  , opConst'
  -- ** Automatic creation using the /ad/ library
  , op1, op2, op3, opN
  , Replicate
  -- ** Giving gradients directly
  , op1', op2', op3'
  -- ** From Isomorphisms
  , opCoerce, opTup, opIso
  , opCoerce', opTup', opIso'
  -- * Utility
  , pattern (:>), only, head'
  , pattern (::<), only_
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
-- So, to create an @'Op' as a@ with the 'Op' constructor (or an 'OpM' with the
-- 'OpM' constructor), you give a function that returns a tuple,
-- containing:
--
--     1. An @a@: The result of the function
--     2. An @Maybe a -> Tuple as@:  A function that, when given
--     \(\frac{dz}{dy}\) (in a 'Just'), returns the total gradient
--     \(\nabla_z \mathbf{x}\).  If the function is given is given
--     'Nothing', then \(\frac{dz}{dy}\) should be taken to be 1.  In other
--     words, you would simply need to return \(\nabla_y \mathbf{x}\),
--     unchanged.  The "final result" is just \(f(\mathbf{x})\), and not
--     \(g(f(\mathbf{x}))\).
--
-- This is done so that 'Op's can easily be "chained" together, one after
-- the other.  If you have an 'Op' for \(f\) and an 'Op' for \(g\), you can
-- compute the gradient of \(f\) knowing that the result target is
-- \(g \circ f\).
--
-- Note that end users should probably never be required to construct an
-- 'Op' or 'OpM' explicitly this way.  Instead, libraries should provide
-- carefuly pre-constructed ones, or provide ways to generate them
-- automatically (like 'op1', 'op2', and 'op3' here).

-- | An @'OpM' m as a@ represents a /differentiable/ (monadic) function
-- from @as@ to @a@, in the context of a 'Monad' @m@.
--
-- For example, an
--
-- @
-- 'OpM' IO '[Int, Bool] Double
-- @
--
-- would be a function that takes an 'Int' and a 'Bool' and returns
-- a 'Double' (in 'IO').  It can be differentiated to give a /gradient/ of
-- an 'Int' and a 'Bool' (also in 'IO') if given the total derivative for
-- the @Double@.
--
-- Note that an 'OpM' is a /superclass/ of 'Op', so any function that
-- expects an @'OpM' m as a@ can also accept an @'Op' as a@.
--
-- See 'runOpM', 'gradOpM', and 'gradOpWithM' for examples on how to run
-- it.
newtype OpM m as a =
    -- | Construct an 'OpM' by giving a (monadic) function creating the
    -- result, and also a continuation on how to create the gradient, given
    -- the total derivative of @a@.
    --
    -- See the module documentation for "Numeric.Backprop.Op" for more
    -- details on the function that this constructor and 'Op' expect.
    OpM (Tuple as -> m (a, Maybe a -> m (Tuple as)))

-- | An @'Op' as a@ is a type synonym over 'OpM' that describes
-- a differentiable function from @as@ to @a@.
--
-- For example, an
--
-- @
-- 'Op' '[Int, Bool] Double
-- @
--
-- is a function from an 'Int' and a 'Bool', returning a 'Double'.  It can
-- be differentiated to give a /gradient/ of an 'Int' and a 'Bool' if given
-- a total derivative for the @Double@.
--
-- See 'runOp', 'gradOp', and 'gradOpWith' for examples on how to run it,
-- and 'Op' for instructions on creating it.
--
-- This type is abstracted over using the pattern synonym with constructor
-- 'Op', so you can create one from scratch with it.  However, it's
-- simplest to create it using 'op2'', 'op1'', 'op2'', and 'op3'' helper
-- smart constructors  And, if your function is a numeric function, they
-- can even be created automatically using 'op1', 'op2', 'op3', and 'opN'
-- with a little help from "Numeric.AD" from the /ad/ library.
--
-- Note that this type is a /subset/ or /subtype/ of 'OpM'.  So, if a function
-- ever expects an @'OpM' m as a@, you can always provide an @'Op' as a@
-- instead.
--
-- Many functions in this library will expect an @'OpM' m as a@ (or
-- an @'Numeric.Backprop.OpB' s as a@), and in all of these cases, you can
-- provide an @'Op' as a@.
type Op as a = forall m. Monad m => OpM m as a

-- | Helper wrapper used for the implementation of 'composeOp'.
newtype OpCont m as a = OC { runOpCont :: Maybe a -> m (Tuple as) }

-- | Construct an 'Op' by giving a function creating the result, and also
-- a continuation on how to create the gradient, given the total derivative
-- of @a@.
--
-- See the module documentation for "Numeric.Backprop.Op" for more details
-- on the function that this constructor and 'OpM' expect.
pattern Op :: (Tuple as -> (a, Maybe a -> Tuple as)) -> Op as a
pattern Op runOp' <- OpM (\f -> (second . fmap) getI . getI . f -> runOp')
  where
    Op f = OpM (pure . (second . fmap) pure . f)

-- | A combination of 'runOpM' and 'gradOpWithM''.  Given an 'OpM' and
-- inputs, returns the result of the 'OpM' and a continuation that gives
-- its gradient.
--
-- The continuation takes the total derivative of the result as input.  See
-- documenation for 'gradOpWithM'' and module documentation for
-- "Numeric.Backprop.Op" for more information.
runOpM'
    :: OpM m as a                       -- ^ 'OpM' to run
    -> Tuple as                         -- ^ Inputs
    -> m (a, Maybe a -> m (Tuple as))   -- ^ Result, and continuation to
                                        --     get the gradient
runOpM' (OpM f) = f

-- | A combination of 'runOp' and 'gradOpWith''.  Given an 'Op' and inputs,
-- returns the result of the 'Op' and a continuation that gives its
-- gradient.
--
-- The continuation takes the total derivative of the result as input.  See
-- documenation for 'gradOpWith'' and module documentation for
-- "Numeric.Backprop.Op" for more information.
runOp'
    :: Op as a                  -- ^ 'Op' to run
    -> Tuple as                 -- ^ Inputs
    -> (a, Maybe a -> Tuple as) -- ^ Result, and continuation to get
                                --     the gradient
runOp' o = (second . fmap) getI . getI . runOpM' o

-- | 'composeOp', but taking explicit 'Summer's, for the situation where
-- the @as@ are not instance of 'Num'.
composeOp'
    :: Monad m
    => Prod Summer as       -- ^ Explicit 'Summer's
    -> Prod (OpM m as) bs   -- ^ 'Prod' of 'OpM's taking @as@ and returning
                            --     different @b@ in @bs@
    -> OpM m bs c           -- ^ 'OpM' taking eac of the @bs@ from the
                            --     input 'Prod'.
    -> OpM m as c           -- ^ Composed 'OpM'
composeOp' ss os o = OpM $ \xs -> do
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

-- | Compose 'OpM's together, similar to '.'.  But, because all 'OpM's are
-- \(\mathbb{R}^N \rightarrow \mathbb{R}\), this is more like 'sequence'
-- for functions, or @liftAN@.
--
-- That is, given an @'OpM' m as b1@, an @'OpM' m as b2@, and an @'OpM'
-- m as b3@, it can compose them with an @'OpM' m '[b1,b2,b3] c@ to create
-- an @'OpM' m as c@.
composeOp
    :: (Monad m, Known Length as, Every Num as)
    => Prod (OpM m as) bs   -- ^ 'Prod' of 'OpM's taking @as@ and returning
                            --     different @b@ in @bs@
    -> OpM m bs c           -- ^ 'OpM' taking eac of the @bs@ from the
                            --     input 'Prod'.
    -> OpM m as c           -- ^ Composed 'OpM'
composeOp = composeOp' summers

-- | Run the function that an 'Op' encodes, to get the result.
--
-- >>> runOp (op2 (*)) (3 ::< 5 ::< Ø)
-- 15
runOp :: Op as a -> Tuple as -> a
runOp o = fst . runOp' o

-- | Run the function that an 'Op' encodes, to get the resulting output and
-- also its gradient with respect to the inputs.
--
-- >>> gradOpM' (op2 (*)) (3 ::< 5 ::< Ø) :: IO (Int, Tuple '[Int, Int])
-- (15, 5 ::< 3 ::< Ø)
gradOp' :: Op as a -> Tuple as -> (a, Tuple as)
gradOp' o = second ($ Nothing) . runOp' o

-- | The monadic version of 'runOp', for 'OpM's.
--
-- >>> runOpM (op2 (*)) (3 ::< 5 ::< Ø) :: IO Int
-- 15
runOpM :: Functor m => OpM m as a -> Tuple as -> m a
runOpM o = fmap fst . runOpM' o

-- | The monadic version of 'gradOp'', for 'OpM's.
gradOpM' :: Monad m => OpM m as a -> Tuple as -> m (a, Tuple as)
gradOpM' o x = do
    (y, gF) <- runOpM' o x
    g <- gF Nothing
    return (y, g)

-- | A combination of 'gradOp' and 'gradOpWith'.  The third argument is
-- (optionally) the total derivative the result.  Give 'Nothing' and it is
-- assumed that the result is the final result (and the total derivative is
-- 1), and this behaves the same as 'gradOp'.  Give @'Just' d@ and it uses
-- the @d@ as the total derivative of the result, and this behaves like
-- 'gradOpWith'.
--
-- See 'gradOp' and the module documentaiton for "Numeric.Backprop.Op" for
-- more information.
gradOpWith'
    :: Op as a      -- ^ 'Op' to run
    -> Tuple as     -- ^ Inputs to run it with
    -> Maybe a      -- ^ If 'Just', taken as the total derivative of the
                    --     result.  If 'Nothing', assumes that the result is
                    --     the final result.
    -> Tuple as     -- ^ The gradient
gradOpWith' o = snd . runOp' o

-- | The monadic version of 'gradOpWith'', for 'OpM's.
gradOpWithM'
    :: Monad m
    => OpM m as a       -- ^ 'OpM' to run
    -> Tuple as         -- ^ Inputs to run it with
    -> Maybe a          -- ^ If 'Just', taken as the total derivative of the
                        --     result.  If 'Nothing', assumes that the result is
                        --     the final result.
    -> m (Tuple as)     -- ^ The gradient
gradOpWithM' o xs g = do
    (_, f) <- runOpM' o xs
    f g

-- | Run the function that an 'Op' encodes, and get the gradient of
-- a "final result" with respect to the inputs, given the total derivative
-- of the output with the final result.
--
-- See 'gradOp' and the module documentaiton for "Numeric.Backprop.Op" for
-- more information.
gradOpWith
    :: Op as a      -- ^ 'Op' to run
    -> Tuple as     -- ^ Inputs to run it with
    -> a            -- ^ The total derivative of the result
    -> Tuple as     -- ^ The gradient
gradOpWith o i = gradOpWith' o i . Just

-- | The monadic version of 'gradOpWith', for 'OpM's.
gradOpWithM
    :: Monad m
    => OpM m as a       -- ^ 'OpM' to run
    -> Tuple as         -- ^ Inputs to run it with
    -> a                -- ^ The total derivative of the result
    -> m (Tuple as)     -- ^ the gradient
gradOpWithM o i = gradOpWithM' o i . Just

-- | Run the function that an 'Op' encodes, and get the gradient of the
-- output with respect to the inputs.
--
-- >>> gradOp (op2 (*)) (3 ::< 5 ::< Ø)
-- 5 ::< 3 ::< Ø
-- -- the gradient of x*y is (y, x)
gradOp :: Op as a -> Tuple as -> Tuple as
gradOp o i = gradOpWith' o i Nothing

-- | The monadic version of 'gradOp', for 'OpM's.
gradOpM :: Monad m => OpM m as a -> Tuple as -> m (Tuple as)
gradOpM o i = do
    (_, gF) <- runOpM' o i
    gF Nothing

-- | A version of 'opCoerce' that takes an explicit 'Unity', so can be run
-- on values that aren't 'Num' instances.
opCoerce' :: Coercible a b => Unity a -> Op '[a] b
opCoerce' u = opIso' u coerced

-- | An 'Op' that coerces an item into another item whose type has the same
-- runtime representation.  Requires the input to be an instance of 'Num'.
--
-- >>> gradOp' opCoerce (Identity 5) :: (Int, Identity Int)
-- (5, Identity 1)
--
-- @
-- 'opCoerce' = 'opIso' 'coerced'
-- @
opCoerce :: (Coercible a b, Num a) => Op '[a] b
opCoerce = opIso coerced

-- | A version of 'opTup' that takes explicit 'Unity's, so can be run on
-- values of types that aren't 'Num' instances.
opTup'
    :: Prod Unity as
    -> Op as (Tuple as)
opTup' u = Op $ \xs -> (xs, fromMaybe (map1 (I . getUnity) u))

-- | An 'Op' that takes @as@ and returns exactly the input tuple.
--
-- >>> gradOp' opTup (1 ::< 2 ::< 3 ::< Ø)
-- (1 ::< 2 ::< 3 ::< Ø, 1 ::< 1 ::< 1 ::< Ø)
opTup
    :: (Every Num as, Known Length as)
    => Op as (Tuple as)
opTup = opTup' (map1 ((// known) . every @_ @Num) indices)

-- | A version of 'opIso' that takes an explicit 'Unity', so can be run on
-- values of types that aren't 'Num' instances.
opIso' :: Unity a -> Iso' a b -> Op '[ a ] b
opIso' u i = op1' $ \x -> (view i x, maybe (getUnity u) (review i))

-- | An 'Op' that runs the input value through the isomorphism encoded in
-- the 'Iso'.  Requires the input to be an instance of 'Num'.
--
-- Warning: This is unsafe!  It assumes that the isomorphisms themselves
-- have derivative 1, so will break for things like
-- 'Numeric.Lens.exponentiating'.  Basically, don't use this for any
-- "numeric" isomorphisms.
opIso :: Num a => Iso' a b -> Op '[ a ] b
opIso = opIso' known

-- | A version of 'opConst' that takes explicit 'Summer's, so can be run on
-- values of types that aren't 'Num' instances.
opConst' :: Prod Summer as -> a -> Op as a
opConst' ss x = Op $ \_ ->
    (x , const $ map1 (\s -> I $ runSummer s []) ss)

-- | An 'Op' that ignores all of its inputs and returns a given constant
-- value.
--
-- >>> gradOp' (opConst 10) (1 ::< 2 ::< 3 ::< Ø)
-- (10, 0 ::< 0 ::< 0 ::< Ø)
opConst :: (Every Num as, Known Length as) => a -> Op as a
opConst = opConst' summers

-- | Create an 'Op' that takes no inputs and always returns the given
-- value.
--
-- There is no gradient, of course (using 'gradOp' will give you an empty
-- tuple), because there is no input to have a gradient of.
--
-- >>> gradOp' (op0 10) Ø
-- (10, Ø)
--
-- For a constant 'Op' that takes input and ignores it, see 'opConst' and
-- 'opConst''.
--
-- Note that because this returns an 'Op', it can be used with any function
-- that expects an 'OpM' or 'Numeric.Backprop.OpB', as well.
op0 :: a -> Op '[] a
op0 x = Op $ \case
    Ø -> (x, const Ø)

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
-- If the input is 'Nothing', then \(\frac{dz}{dy}\) should be taken to be
-- \(1\).
--
-- As an example, here is an 'Op' that squares its input:
--
-- @
-- square :: Num a => 'Op' '[a] a
-- square = 'op1'' $ \\x -> (x*x, \\case Nothing -> 2 * x
--                                   Just d  -> 2 * d * x
--                       )
-- @
--
-- Remember that, generally, end users shouldn't directly construct 'Op's;
-- they should be provided by libraries or generated automatically.
--
-- For numeric functions, single-input 'Op's can be generated automatically
-- using 'op1'.
op1'
    :: (a -> (b, Maybe b -> a))
    -> Op '[a] b
op1' f = Op $ \case
    I x :< Ø ->
      let (y, dx) = f x
      in  (y, only_ . dx)

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
-- If the input is 'Nothing', then \(\frac{dk}{dz}\) should be taken to be
-- \(1\).
--
-- As an example, here is an 'Op' that multiplies its inputs:
--
-- @
-- mul :: Num a => 'Op' '[a, a] a
-- mul = 'op2'' $ \\x y -> (x*y, \\case Nothing -> (y  , x  )
--                                  Just d  -> (d*y, x*d)
--                      )
-- @
--
-- Remember that, generally, end users shouldn't directly construct 'Op's;
-- they should be provided by libraries or generated automatically.
--
-- For numeric functions, two-input 'Op's can be generated automatically
-- using 'op2'.
op2'
    :: (a -> b -> (c, Maybe c -> (a, b)))
    -> Op '[a,b] c
op2' f = Op $ \case
    I x :< I y :< Ø ->
      let (z, dxdy) = f x y
      in  (z, (\(dx,dy) -> dx ::< dy ::< Ø) . dxdy)

-- | Create an 'Op' of a function taking three inputs, by giving its explicit
-- gradient.  See documentation for 'op2'' for more details.
op3'
    :: (a -> b -> c -> (d, Maybe d -> (a, b, c)))
    -> Op '[a,b,c] d
op3' f = Op $ \case
    I x :< I y :< I z :< Ø ->
      let (q, dxdydz) = f x y z
      in  (q, (\(dx, dy, dz) -> dx ::< dy ::< dz ::< Ø) . dxdydz)

-- | Automatically create an 'Op' of a numerical function taking one
-- argument.  Uses 'Numeric.AD.diff', and so can take any numerical
-- function polymorphic over the standard numeric types.
--
-- >>> gradOp' (op1 (recip . negate)) (5 ::< Ø)
-- (-0.2, 0.04 ::< Ø)
op1 :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Op '[a] a
op1 f = op1' $ \x ->
    let (z, dx) = diff' f x
    in  (z, maybe dx (* dx))

-- | Automatically create an 'Op' of a numerical function taking two
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical function
-- polymorphic over the standard numeric types.
--
-- >>> gradOp' (op2 (\x y -> x * sqrt y)) (3 ::< 4 ::< Ø)
-- (6.0, 2.0 ::< 0.75 ::< Ø)
op2 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a] a
op2 f = opN $ \case I x :* I y :* ØV -> f x y

-- | Automatically create an 'Op' of a numerical function taking three
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical function
-- polymorphic over the standard numeric types.
--
-- >>> gradOp' (op3 (\x y z -> (x * sqrt y)**z)) (3 ::< 4 ::< 2 ::< Ø)
-- (36.0, 24.0 ::< 9.0 ::< 64.503 ::< Ø)
op3 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op '[a,a,a] a
op3 f = opN $ \case I x :* I y :* I z :* ØV -> f x y z

-- | Automatically create an 'Op' of a numerical function taking multiple
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical
-- function polymorphic over the standard numeric types.
--
-- >>> gradOp' (opN (\(x :+ y :+ Ø) -> x * sqrt y)) (3 ::< 4 ::< Ø)
-- (6.0, 2.0 ::< 0.75 ::< Ø)
opN :: (Num a, Known Nat n)
    => (forall s. Reifies s Tape => Vec n (Reverse s a) -> Reverse s a)
    -> Op (Replicate n a) a
opN f = Op $ \xs ->
    let (y, dxs) = grad' f (prodToVec' known xs)
    in  (y, vecToProd . maybe dxs (\q -> (q *) <$> dxs))

instance (Monad m, Known Length as, Every Num as, Num a) => Num (OpM m as a) where
    o1 + o2       = composeOp (o1 :< o2 :< Ø) $ op2 (+)
    o1 - o2       = composeOp (o1 :< o2 :< Ø) $ op2 (-)
    o1 * o2       = composeOp (o1 :< o2 :< Ø) $ op2 (*)
    negate o      = composeOp (o :< Ø)        $ op1 negate
    signum o      = composeOp (o :< Ø)        $ op1 signum
    abs    o      = composeOp (o :< Ø)        $ op1 abs
    fromInteger x = opConst (fromInteger x)

instance (Monad m, Known Length as, Every Fractional as, Every Num as, Fractional a) => Fractional (OpM m as a) where
    o1 / o2        = composeOp (o1 :< o2 :< Ø) $ op2 (/)
    recip o        = composeOp (o :< Ø)        $ op1 recip
    fromRational x = opConst (fromRational x)

instance (Monad m, Known Length as, Every Floating as, Every Fractional as, Every Num as, Floating a) => Floating (OpM m as a) where
    pi            = opConst pi
    exp   o       = composeOp (o :< Ø)        $ op1 exp
    log   o       = composeOp (o :< Ø)        $ op1 log
    sqrt  o       = composeOp (o :< Ø)        $ op1 sqrt
    o1 ** o2      = composeOp (o1 :< o2 :< Ø) $ op2 (**)
    logBase o1 o2 = composeOp (o1 :< o2 :< Ø) $ op2 logBase
    sin   o       = composeOp (o :< Ø)        $ op1 sin
    cos   o       = composeOp (o :< Ø)        $ op1 cos
    tan   o       = composeOp (o :< Ø)        $ op1 tan
    asin  o       = composeOp (o :< Ø)        $ op1 asin
    acos  o       = composeOp (o :< Ø)        $ op1 acos
    atan  o       = composeOp (o :< Ø)        $ op1 atan
    sinh  o       = composeOp (o :< Ø)        $ op1 sinh
    cosh  o       = composeOp (o :< Ø)        $ op1 cosh
    asinh o       = composeOp (o :< Ø)        $ op1 asinh
    acosh o       = composeOp (o :< Ø)        $ op1 acosh
    atanh o       = composeOp (o :< Ø)        $ op1 atanh

