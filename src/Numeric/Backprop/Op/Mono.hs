{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Numeric.Backprop.Op.Mono
-- Copyright   : (c) Justin Le 2017
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides monomorphic versions of the types and combinators in
-- "Numeric.Backprop.Op", for usage with "Numeric.Backprop.Mono" and
-- "Numeric.Backprop.Mono.Implicit".
--
-- They are monomorphic in the sense that all of the /inputs/ have to be of
-- the same type.  So, something like
--
-- @
-- 'Numeric.Backprop.Op' '[Double, Double, Double] Int
-- @
--
-- From "Numeric.Backprop" would, in this module, be:
--
-- @
-- 'Op' 'N3' Double Int
-- @
-- 
-- See the module header for "Numeric.Backprop.Op" for more explicitly
-- details on how to encode an 'Op' and how they are implemented.  For the
-- most part, the same principles will apply.
--
-- Note that 'Op' is a /subset/ or /subtype/ of 'OpM', and so, any function
-- that expects an @'OpM' m as a@ (or an @'Numeric.Backprop.Mono.OpB' s as a@)
-- can be given an @'Op' as a@ and it'll work just fine.
--

module Numeric.Backprop.Op.Mono (
  -- * Types
  -- ** Op and synonyms
    Op, pattern Op, OpM, pattern OpM
  -- ** Vector types
  -- | See "Numeric.Backprop.Mono#vec" for a mini-tutorial on 'VecT' and
  -- 'Vec'
  , VecT(..), Vec, I(..)
  -- * Running
  -- ** Pure
  , runOp, gradOp, gradOp', gradOpWith, gradOpWith', runOp'
  -- ** Monadic
  , runOpM, gradOpM, gradOpM', gradOpWithM, gradOpWithM', runOpM'
  -- * Creation
  , op0, opConst, composeOp, composeOp1, (~.)
  -- ** Automatic creation using the /ad/ library
  , op1, op2, op3, opN
  , Replicate
  -- ** Giving gradients directly
  , op1', op2', op3'
  -- * Utility
  , pattern (:+), (*:), (+:), head'
  -- ** 'Nat' type synonyms
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
  -- ** Numeric Ops#numops#
  -- $numops
  , (+.), (-.), (*.), negateOp, absOp, signumOp
  , (/.), recipOp
  , expOp, logOp, sqrtOp, (**.), logBaseOp
  , sinOp, cosOp, tanOp, asinOp, acosOp, atanOp
  , sinhOp, coshOp, tanhOp, asinhOp, acoshOp, atanhOp
 ) where

import           Data.Bifunctor
import           Data.Reflection                  (Reifies)
import           Data.Type.Combinator
import           Data.Type.Nat
import           Data.Type.Util
import           Data.Type.Vector
import           Numeric.AD.Internal.Reverse      (Reverse, Tape)
import           Numeric.AD.Mode.Forward          (AD, Forward)
import           Type.Class.Known
import           Type.Family.Nat
import qualified Numeric.Backprop.Internal.Helper as BP
import qualified Numeric.Backprop.Op              as BP

-- | An @'Op' n a b@ describes a differentiable function from @n@ values of
-- type @a@ to a value of type @b@.
--
-- For example, a value of type
--
-- @
-- 'Op' 'N2' Int Double
-- @
--
-- is a function that takes two 'Int's and returns a 'Double'.
-- It can be differentiated to give a /gradient/ of two 'Int's, if given
-- a total derivative for the 'Double'.  Mathematically, it is akin to a:
--
-- \[
-- f : \mathbb{Z}^2 \rightarrow \mathbb{R}
-- \]
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
-- Note that this type is a /subset/ or /subtype/ of 'OpM' (and also of
-- 'Numeric.Backprop.Mono.OpB').  So, if a function ever expects an @'OpM'
-- m as a@ (or a 'Numeric.Backprop.Mono.OpB'), you can always provide an
-- @'Op' as a@ instead.
--
-- Many functions in this library will expect an @'OpM' m as a@ (or
-- an @'Numeric.Backprop.Mono.OpB' s as a@), and in all of these cases, you can
-- provide an @'Op' as a@.
type Op n a b  = BP.Op (Replicate n a) b

-- | An @'OpM' m n a b@ represents a differentiable (monadic) function from
-- @n@ values of type @a@ to a value of type @b@.
--
-- For example, an
--
-- @
-- 'OpM' IO 'N2' Int Double
-- @
--
-- would be a function that takes two 'Int's and returns a 'Double' (in
-- 'IO').  It can be differentiated to give a /gradient/ of the two input
-- 'Int's (also in 'IO') if given the total derivative for @a@.
--
-- Note that an 'OpM' is a /superclass/ of 'Op', so any function that
-- expects an @'OpM' m as a@ can also accept an @'Op' as a@.
--
-- See 'runOpM', 'gradOpM', and 'gradOpWithM' for examples on how to run
-- it.
type OpM m n a = BP.OpM m (Replicate n a)

-- | Construct an 'Op' by giving a function creating the result, and also
-- a continuation on how to create the gradient, given the total derivative
-- of @a@.
--
-- See the module documentation for "Numeric.Backprop.Op" for more details
-- on the function that this constructor and 'OpM' expect.
pattern Op :: Known Nat n => (Vec n a -> (b, Maybe b -> Vec n a)) -> Op n a b
pattern Op runOp' <- BP.Op (\f xs -> (second . fmap) (prodAlong xs)
                                    . f
                                    . vecToProd
                                    $ xs
                             -> runOp'
                           )
  where
    Op f = BP.Op (\xs -> (second . fmap) vecToProd . f . prodToVec' known $ xs)

-- | Construct an 'OpM' by giving a (monadic) function creating the result,
-- and also a continuation on how to create the gradient, given the total
-- derivative of @a@.
--
-- See the module documentation for "Numeric.Backprop.Op" for more details
-- on the function that this constructor and 'Op' expect.
pattern OpM :: (Known Nat n, Functor m) => (Vec n a -> m (b, Maybe b -> m (Vec n a))) -> OpM m n a b
pattern OpM runOpM' <- BP.OpM (\f xs -> (fmap . second . fmap . fmap) (prodAlong xs)
                                      . f
                                      . vecToProd
                                      $ xs
                               -> runOpM'
                              )
  where
    OpM f = BP.OpM (\xs -> (fmap . second . fmap . fmap) vecToProd . f . prodToVec' known $ xs)

-- | Create an 'Op' that takes no inputs and always returns the given
-- value.
--
-- There is no gradient, of course (using 'gradOp' will give you an empty
-- vector), because there is no input to have a gradient of.
--
-- >>> gradOp' (op0 10) ØV
-- (10, ØV)
--
-- For a constant 'Op' that takes input and ignores it, see 'opConst'.
--
-- Note that because this returns an 'Op', it can be used with any function
-- that expects an 'OpM' or 'Numeric.Backprop.Mono.OpB', as well.
op0 :: a -> Op N0 b a
op0 x = BP.op0 x

-- | An 'Op' that ignores all of its inputs and returns a given constant
-- value.
--
-- >>> gradOp' (opConst 10) (1 :+ 2 :+ 3 :+ ØV)
-- (10, 0 :+ 0 :+ 0 :+ ØV)
opConst :: forall n a b. (Known Nat n, Num b) => a -> Op n b a
opConst x = BP.opConst' (BP.nSummers' @n @b known) x

-- | Automatically create an 'Op' of a numerical function taking one
-- argument.  Uses 'Numeric.AD.diff', and so can take any numerical
-- function polymorphic over the standard numeric types.
--
-- >>> gradOp' (op1 (recip . negate)) (5 :+ ØV)
-- (-0.2, 0.04 :+ ØV)
op1 :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Op N1 a a
op1 f = BP.op1 f

-- | Automatically create an 'Op' of a numerical function taking two
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical function
-- polymorphic over the standard numeric types.
--
-- >>> gradOp' (op2 (\x y -> x * sqrt y)) (3 :+ 4 :+ ØV)
-- (6.0, 2.0 :+ 0.75 :+ ØV)
op2 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Op N2 a a
op2 = BP.op2

-- | Automatically create an 'Op' of a numerical function taking three
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical function
-- polymorphic over the standard numeric types.
--
-- >>> gradOp' (op3 (\x y z -> (x * sqrt y)**z)) (3 :+ 4 :+ 2 :+ ØV)
-- (36.0, 24.0 :+ 9.0 :+ 64.503 :+ ØV)
op3 :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Op N3 a a
op3 = BP.op3

-- | Automatically create an 'Op' of a numerical function taking multiple
-- arguments.  Uses 'Numeric.AD.grad', and so can take any numerical
-- function polymorphic over the standard numeric types.
--
-- >>> gradOp' (opN (\(x :+ y :+ Ø) -> x * sqrt y)) (3 :+ 4 :+ ØV)
-- (6.0, 2.0 :+ 0.75 :+ ØV)
opN :: (Num a, Known Nat n)
    => (forall s. Reifies s Tape => Vec n (Reverse s a) -> Reverse s a)
    -> Op n a a
opN = BP.opN

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
-- square :: Num a => 'Op' 'N1' a a
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
    -> Op N1 a b
op1' = BP.op1'

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
-- mul :: Num a => 'Op' 'N2' a a
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
    :: (a -> a -> (b, Maybe b -> (a, a)))
    -> Op N2 a b
op2' = BP.op2'

-- | Create an 'Op' of a function taking three inputs, by giving its explicit
-- gradient.  See documentation for 'op2'' for more details.
op3'
    :: (a -> a -> a -> (b, Maybe b -> (a, a, a)))
    -> Op N3 a b
op3' = BP.op3'

-- | A combination of 'runOp' and 'gradOpWith''.  Given an 'Op' and inputs,
-- returns the result of the 'Op' and a continuation that gives its
-- gradient.
--
-- The continuation takes the total derivative of the result as input.  See
-- documenation for 'gradOpWith'' and module documentation for
-- "Numeric.Backprop.Op" for more information.
runOp' :: Op n a b -> Vec n a -> (b, Maybe b -> Vec n a)
runOp' o xs = (second . fmap) (prodAlong xs)
            . BP.runOp' o
            . vecToProd
            $ xs

-- | Run the function that an 'Op' encodes, to get the result.
--
-- >>> runOp (op2 (*)) (3 :+ 5 :+ Ø)
-- 15
runOp :: Op n a b -> Vec n a -> b
runOp o = fst . runOp' o

-- | A combination of 'gradOp' and 'gradOpWith'.  The third argument is
-- (optionally) the total derivative the result.  Give 'Nothing' and it is
-- assumed that the result is the final result (and the total derivative is
-- 1), and this behaves the same as 'gradOp'.  Give @'Just' d@ and it uses
-- the @d@ as the total derivative of the result, and this behaves like
-- 'gradOpWith'.
--
-- See 'gradOp' and the module documentaiton for "Numeric.Backprop.Op" for
-- more information.
gradOpWith' :: Op n a b -> Vec n a -> Maybe b -> Vec n a
gradOpWith' o = snd . runOp' o

-- | Run the function that an 'Op' encodes, and get the gradient of
-- a "final result" with respect to the inputs, given the total derivative
-- of the output with the final result.
--
-- See 'gradOp' and the module documentaiton for "Numeric.Backprop.Op" for
-- more information.
gradOpWith :: Op n a b -> Vec n a -> b -> Vec n a
gradOpWith o i = gradOpWith' o i . Just

-- | Run the function that an 'Op' encodes, and get the gradient of the
-- output with respect to the inputs.
--
-- >>> gradOp (op2 (*)) (3 :+ 5 :+ ØV)
-- 5 :+ 3 :+ ØV
-- -- the gradient of x*y is (y, x)
gradOp :: Op n a b -> Vec n a -> Vec n a
gradOp o i = gradOpWith' o i Nothing

-- | Run the function that an 'Op' encodes, to get the resulting output and
-- also its gradient with respect to the inputs.
--
-- >>> gradOpM' (op2 (*)) (3 :+ 5 :+ ØV) :: IO (Int, Vec N2 Int)
-- (15, 5 :+ 3 :+ ØV)
gradOp' :: Op n a b -> Vec n a -> (b, Vec n a)
gradOp' o = second ($ Nothing) . runOp' o

-- | The monadic version of 'runOp', for 'OpM's.
--
-- >>> runOpM (op2 (*)) (3 :+ 5 :+ ØV) :: IO Int
-- 15
runOpM' :: Functor m => OpM m n a b -> Vec n a -> m (b, Maybe b -> m (Vec n a))
runOpM' o xs = (fmap . second . fmap . fmap) (prodAlong xs)
             . BP.runOpM' o
             . vecToProd
             $ xs

-- | The monadic version of 'runOp', for 'OpM's.
--
-- >>> runOpM (op2 (*)) (3 :+ 5 :+ ØV) :: IO Int
-- 15
runOpM :: Functor m => OpM m n a b -> Vec n a -> m b
runOpM o = fmap fst . runOpM' o

-- | The monadic version of 'gradOp', for 'OpM's.
gradOpM :: Monad m => OpM m n a b -> Vec n a -> m (Vec n a)
gradOpM o i = do
    (_, gF) <- runOpM' o i
    gF Nothing

-- | The monadic version of 'gradOp'', for 'OpM's.
gradOpM' :: Monad m => OpM m n a b -> Vec n a -> m (b, Vec n a)
gradOpM' o i = do
    (x, gF) <- runOpM' o i
    g <- gF Nothing
    return (x, g)

-- | The monadic version of 'gradOpWith'', for 'OpM's.
gradOpWithM' :: Monad m => OpM m n a b -> Vec n a -> Maybe b -> m (Vec n a)
gradOpWithM' o i d = do
    (_, gF) <- runOpM' o i
    gF d

-- | The monadic version of 'gradOpWith', for 'OpM's.
gradOpWithM :: Monad m => OpM m n a b -> Vec n a -> b -> m (Vec n a)
gradOpWithM o i d = do
    (_, gF) <- runOpM' o i
    gF (Just d)

-- | Compose 'OpM's together, similar to '.'.  But, because all 'OpM's are
-- \(\mathbb{R}^N \rightarrow \mathbb{R}\), this is more like 'sequence'
-- for functions, or @liftAN@.
--
-- That is, given an @o@ of @'OpM' m n a b@s, it can compose them with an
-- @'OpM' m o b c@ to create an @'OpM' m o a c@.
composeOp
    :: forall m n o a b c. (Monad m, Num a, Known Nat n)
    => VecT o (OpM m n a) b
    -> OpM m o b c
    -> OpM m n a c
composeOp v o = BP.composeOp' (BP.nSummers' @n @a known) (vecToProd v) o

-- | Convenient wrappver over 'composeOp' for the case where the second
-- function only takes one input, so the two 'OpM's can be directly piped
-- together, like for '.'.
composeOp1
    :: forall m n a b c. (Monad m, Num a, Known Nat n)
    => OpM m n a b
    -> OpM m N1 b c
    -> OpM m n a c
composeOp1 v o = composeOp @_ @_ @_ @a (v :* ØV) o

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
    :: forall m n a b c. (Monad m, Num a, Known Nat n)
    => OpM m N1 b c
    -> OpM m n a b
    -> OpM m n a c
f ~. g = composeOp1 @_ @_ @a g f

-- $numops
--
-- Built-in ops for common numeric operations, implemented directly so
-- that they are more efficient than using 'op1' \/ 'op2' etc.
--
-- The naming scheme is:
--
-- @
-- ('+.') = 'op2' ('+')
-- 'negateOp' = 'op1' 'negate
-- @
--
-- Note that the operators (like '+.') are meant to be used in prefix
-- form, like:
--
-- @
-- 'Numeric.Backprop.Mono.liftB2' ('.+') v1 v2
-- @

-- | Optimized version of @'op2' ('+')@.
(+.) :: Num a => Op N2 a a
(+.) = (BP.+.) 

-- | Optimized version of @'op2' ('-')@.
(-.) :: Num a => Op N2 a a
(-.) = (BP.-.) 

-- | Optimized version of @'op2' ('*')@.
(*.) :: Num a => Op N2 a a
(*.) = (BP.*.) 

-- | Optimized version of @'op2' ('/')@.
(/.) :: Fractional a => Op N2 a a
(/.) = (BP./.) 

-- | Optimized version of @'op2' ('**')@.
(**.) :: Floating a => Op N2 a a
(**.) = (BP.**.) 

-- | Optimized version of @'op1' 'negate'@.
negateOp :: Num a => Op N1 a a
negateOp = BP.negateOp 

-- | Optimized version of @'op1' 'signumOp'@.
signumOp :: Num a => Op N1 a a
signumOp = BP.signumOp 

-- | Optimized version of @'op1' 'absOp'@.
absOp :: Num a => Op N1 a a
absOp = BP.absOp 

-- | Optimized version of @'op1' 'recipOp'@.
recipOp :: Fractional a => Op N1 a a
recipOp = BP.recipOp 

-- | Optimized version of @'op1' 'expOp'@.
expOp :: Floating a => Op N1 a a
expOp = BP.expOp 

-- | Optimized version of @'op1' 'logOp'@.
logOp :: Floating a => Op N1 a a
logOp = BP.logOp 

-- | Optimized version of @'op1' 'sqrtOp'@.
sqrtOp :: Floating a => Op N1 a a
sqrtOp = BP.sqrtOp 

-- | Optimized version of @'op2' 'logBaseOp'@.
logBaseOp :: Floating a => Op N2 a a
logBaseOp = BP.logBaseOp 

-- | Optimized version of @'op1' 'sinOp'@.
sinOp :: Floating a => Op N1 a a
sinOp = BP.sinOp 

-- | Optimized version of @'op1' 'cosOp'@.
cosOp :: Floating a => Op N1 a a
cosOp = BP.cosOp 

-- | Optimized version of @'op1' 'tanOp'@.
tanOp :: Floating a => Op N1 a a
tanOp = BP.tanOp 

-- | Optimized version of @'op1' 'asinOp'@.
asinOp :: Floating a => Op N1 a a
asinOp = BP.asinOp 

-- | Optimized version of @'op1' 'acosOp'@.
acosOp :: Floating a => Op N1 a a
acosOp = BP.acosOp 

-- | Optimized version of @'op1' 'atanOp'@.
atanOp :: Floating a => Op N1 a a
atanOp = BP.atanOp 

-- | Optimized version of @'op1' 'sinhOp'@.
sinhOp :: Floating a => Op N1 a a
sinhOp = BP.sinhOp 

-- | Optimized version of @'op1' 'coshOp'@.
coshOp :: Floating a => Op N1 a a
coshOp = BP.coshOp 

-- | Optimized version of @'op1' 'tanhOp'@.
tanhOp :: Floating a => Op N1 a a
tanhOp = BP.tanhOp 

-- | Optimized version of @'op1' 'asinhOp'@.
asinhOp :: Floating a => Op N1 a a
asinhOp = BP.asinhOp 

-- | Optimized version of @'op1' 'acoshOp'@.
acoshOp :: Floating a => Op N1 a a
acoshOp = BP.acoshOp 

-- | Optimized version of @'op1' 'atanhOp'@.
atanhOp :: Floating a => Op N1 a a
atanhOp = BP.atanhOp 
