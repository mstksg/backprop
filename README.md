backprop
========

Automatic *heterogeneous* back-propagation using explicit graphs built in monadic
style.  Implements reverse-mode automatic differentiation.  Differs from [ad][]
by offering full heterogeneity -- each intermediate step and the resulting
value can have different types.  Mostly intended for usage with tensor
manipulation libraries to implement automatic back-propagation for gradient
descent and other optimization techniques.

[ad]: http://hackage.haskell.org/package/ad

Simple (monomorphic) usage: (provided as a [sample][monotest])

[monotest]: https://github.com/mstksg/backprop/blob/master/samples/MonoTest.hs

~~~haskell
{-# LANGUAGE GADTs #-}

import           Numeric.Backprop.Mono

test :: BPOp s N3 Double Double
test = withInps $ \(x :* y :* z :* ØV) -> do
    xy  <- op2 (*) -$ (x   :* y :* ØV)
    xyy <- op2 (+) -$ (xy  :* y :* ØV)
    op2 (*)        -$ (xyy :* z :* ØV)

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

Here is a slightly more complicated example, describing the running of a neural
network with one hidden layer to calculate its squared error with respect to
target `targ`, which is parameterized by two weight matrices and two bias
vectors.  Vector/matrix types are from the *hmatrix* package.

~~~haskell
simpleOp
      :: (KnownNat m, KnownNat n, KnownNat o)
      => R m
      -> R o
      -> BPOp s '[ L n m, R n, L o n, R o ] Double
simpleOp inp targ = withInps $ \(w1 :< b1 :< w2 :< b2 :< Ø) -> do
    -- First layer
    y1  <- matVec   -$ (w1 :< x1 :< Ø)
    z1  <- op2 (+)  -$ (y1 :< b1 :< Ø)
    x2  <- logistic -$ only z1
    -- Second layer
    y2  <- matVec   -$ (w2 :< x2 :< Ø)
    z2  <- op2 (+)  -$ (y2 :< b2 :< Ø)
    out <- logistic -$ only z2
    -- Return error squared
    err <- op2 (-)  -$ (out :< t :< Ø)
    dot             -$ (err :< err :< Ø)
  where
    x1 = constRef inp
    t  = constRef targ
~~~

A more full annotated example is given [in the repository as a literate haskell
file][neuraltest], and gives over a full example with a recursive network type.
It is also [rendered as a pdf][neuraltest-pdf].

[neuraltest]: https://github.com/mstksg/backprop/blob/master/samples/NeuralTest.lhs
[neuraltest-pdf]: https://github.com/mstksg/backprop/blob/master/samples/rendered/NeuralTest.pdf
