backprop
========

Automatic *heterogeneous* back-propagation using explicit graphs built in monadic
style.  Implements reverse-mode automatic differentiation.  Differs from [ad][]
by offering full heterogeneity -- each intermediate step and the resulting
value can have different types.  Mostly intended for usage with tensor
manipulation libraries to implement automatic back-propagation for gradient
descent and other optimization techniques.

[ad]: http://hackage.haskell.org/package/ad

Simple (monomorphic) usage:

~~~haskell
{-# LANGUAGE GADTs #-}

import           Numeric.Backprop.Mono

test :: BP s N3 Double (BPRef s N3 Double Double)
test = withInps $ \(x :* y :* z :* ØV) -> do
    xy  <- newBPRef2 x y  $ op2 (*)
    xyy <- newBPRef2 xy y $ op2 (+)
    newBPRef2 xyy z       $ op2 (*)

main :: IO ()
main = print $ backprop test (2 :+ 3 :+ 4 :+ ØV)
~~~

The above builds an explicit graph of the function `f x y z = ((x * y) + y) * z`
and performs automatic differentiation/back-propagation to compute the gradient
of the function and the result, giving `36` and `12 :+ 12 :+ 9 :+ ØV`.

Simple monomorphic operations are liftable using `op1` / `op2` / `op3`, but
polymorphic heterogeneous operations require some understanding of how
operations are encoded to use.  Ideally, a library would abstract over building
`Op`s explicitly.
