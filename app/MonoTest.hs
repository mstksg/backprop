{-# LANGUAGE GADTs #-}

import           Numeric.Backprop.Mono

test :: BPOp s N3 Double Double
test = withInps $ \(x :* y :* z :* ØV) -> do
    xy  <- opRef2 x y  $ op2 (*)
    xyy <- opRef2 xy y $ op2 (+)
    opRef2 xyy z       $ op2 (*)

main :: IO ()
main = print $ backprop test (2 :+ 3 :+ 4 :+ ØV)
