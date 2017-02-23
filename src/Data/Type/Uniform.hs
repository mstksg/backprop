{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}


module Data.Type.Uniform where

import           Data.Kind

data Uniform :: [k] -> k -> Type where
    UZ :: Uniform '[] a
    US :: Uniform as a -> Uniform (a ': as) a
