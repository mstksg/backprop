{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Product
import           Data.Type.Vector
import           GHC.TypeLits
import           Numeric.AD
import           Numeric.Backprop
import           Numeric.Backprop.Op
import           Numeric.LinearAlgebra.Static hiding (dot)
import           Type.Class.Witness hiding           (outer)
import           Type.Family.List

data Tensor :: [Nat] -> Type where
    TS :: { unTS :: Double } -> Tensor '[]
    TV :: { unTV :: R n    } -> Tensor '[n]
    TM :: { unTM :: L m n  } -> Tensor '[m,n]

deriving instance ListC (KnownNat <$> ns) => Show (Tensor ns)

-- matVec
--     :: (KnownNat m, KnownNat n)
--     => Op Tensor '[ '[m,n] , '[n] ] '[m]
-- matVec = op2' $ \case
--     TM m -> \case
--       TV v -> ( TV (m #> v)
--               , \(maybe 1 unTV -> g) ->
--                    (TM (g `outer` v), TV (tr m #> g))
--               )

-- dot :: KnownNat n
--     => Op Tensor '[ '[n] , '[n] ] '[]
-- dot = op2' $ \case
--     TV x -> \case
--       TV y -> ( TS (x <.> y)
--               , \case Nothing      ->
--                         (TV y            , TV x            )
--                       Just (TS g)  ->
--                         (TV (konst g * y), TV (x * konst g))
--               )

-- logistic :: Floating a => a -> a
-- logistic x = 1 / (1 + exp (-x))

-- -- data Network :: Type -> Nat -> Nat -> Type where
-- --     N :: { _nsPs    :: !(Sing ps)
-- --          , _nOp     :: !(BPOp s Tensor ('[i] ': ps) '[o])
-- --          , _nParams :: !(Prod Tensor ps)
-- --          } -> Network s i o

-- -- data Layer :: (Nat, Nat) -> Type where
-- --     Layer :: { _lWeights :: Tensor '[m, n]
-- --              , _lBiases  :: Tensor '[m]
-- --              }
-- --           -> Layer ( '(,) n m )



-- -- ffLayer
-- --     :: forall s n m. (KnownNat m, KnownNat n)
-- --     => Network s n m
-- -- ffLayer = N sing ffLayer' (0 :< 0 :< Ø)
-- --   where
-- --     ffLayer'
-- --         :: BPOp s Tensor '[ '[n], '[m, n], '[m] ] '[m]
-- --     ffLayer' = withInps $ \(x :< w :< b :< Ø) -> do
-- --         y <- newBPRef2 w x $ matVec
-- --         z <- newBPRef2 y b $ op2 (+)
-- --         newBPRef1 z        $ op1 logistic

-- -- (~*~)
-- --     :: Network s a b
-- --     -> Network s b c
-- --     -> Network s a c
-- -- N sPs1 o1 p1 ~*~ N sPs2 o2 p2 =
-- --     N (sPs1 %:++ sPs2) _ (p1 `append'` p2)
-- --         \\ singLength sPs1
-- -- infixr 4 ~*~
-- -- {-# INLINE (~*~) #-}


-- -- err
-- --     :: KnownNat m
-- --     => Tensor '[m]
-- --     -> BPRef s Tensor rs '[m]
-- --     -> BPOp s Tensor rs '[]
-- -- err targ r = do
-- --     t <- newBPRef0     $ op0 targ
-- --     d <- newBPRef2 r t $ op2 (-)
-- --     newBPRef2 d d      $ dot

main :: IO ()
main = putStrLn "hey"










-- liftT0
--     :: SingI ns
--     => (forall a. Floating a => a)
--     -> Tensor ns
-- liftT0 f = go sing
--   where
--     go :: forall ms. Sing ms -> Tensor ms
--     go = \case
--       SNil -> TS f
--       SNat `SCons` SNil -> TV f
--       SNat `SCons` (SNat `SCons` SNil) -> TM f
--       _ `SCons` (_ `SCons` (_ `SCons` _)) -> error "not implemented"

-- liftT1
--     :: SingI ns
--     => (forall a. Floating a => a -> a)
--     -> Tensor ns
--     -> Tensor ns
-- liftT1 f = go sing
--   where
--     go :: forall ms. Sing ms -> Tensor ms -> Tensor ms
--     go = \case
--       SNil                             -> \case TS x -> TS (f x)
--       SNat `SCons` SNil                -> \case TV x -> TV (f x)
--       SNat `SCons` (SNat `SCons` SNil) -> \case TM x -> TM (f x)
--       _ `SCons` (_ `SCons` (_ `SCons` _)) -> \case

-- liftT2
--     :: SingI ns
--     => (forall a. Floating a => a -> a -> a)
--     -> Tensor ns
--     -> Tensor ns
--     -> Tensor ns
-- liftT2 f = go sing
--   where
--     go :: forall ms. Sing ms -> Tensor ms -> Tensor ms -> Tensor ms
--     go = \case
--       SNil -> \case
--         TS x -> \case TS y -> TS (f x y)
--       SNat `SCons` SNil -> \case
--         TV x -> \case TV y -> TV (f x y)
--       SNat `SCons` (SNat `SCons` SNil) -> \case
--         TM x -> \case TM y -> TM (f x y)
--       _ `SCons` (_ `SCons` (_ `SCons` _)) -> \case

-- instance SingI ns => Num (Tensor ns) where
--     (+) = liftT2 (+)
--     (-) = liftT2 (-)
--     (*) = liftT2 (*)
--     negate = liftT1 negate
--     signum = liftT1 signum
--     abs    = liftT1 abs
--     fromInteger x = liftT0 (fromInteger x)

-- instance SingI ns => Fractional (Tensor ns) where
--     (/) = liftT2 (/)
--     recip = liftT1 recip
--     fromRational x = liftT0 (fromRational x)

-- instance SingI ns => Floating (Tensor ns) where
--     pi = liftT0 pi
--     exp = liftT1 exp
--     log = liftT1 log
--     sqrt = liftT1 sqrt
--     (**) = liftT2 (**)
--     logBase = liftT2 logBase
--     sin = liftT1 sin
--     cos = liftT1 cos
--     tan = liftT1 tan
--     asin = liftT1 asin
--     acos = liftT1 acos
--     atan = liftT1 atan
--     sinh = liftT1 sinh
--     cosh = liftT1 cosh
--     tanh = liftT1 tanh
--     asinh = liftT1 asinh
--     acosh = liftT1 acosh
--     atanh = liftT1 atanh
    

