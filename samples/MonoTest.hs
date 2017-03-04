{-# LANGUAGE GADTs #-}

import           Numeric.Backprop.Mono

test :: BPOp s N3 Double Double
test = withInps $ \(x :* y :* z :* ØV) -> do
    xy  <- op2 (*) -$ (x   :* y :* ØV)
    xyy <- op2 (+) -$ (xy  :* y :* ØV)
    op2 (*)        -$ (xyy :* z :* ØV)

test' :: BPOp s N3 Double Double
test' = withInps $ \(x :* y :* z :* ØV) ->
    return $ ((x * y) + y) * z

main :: IO ()
main = do
    print $ backprop test  (2 :+ 3 :+ 40 :+ ØV)
    print $ backprop test' (2 :+ 3 :+ 40 :+ ØV)
