{-# LANGUAGE GADTs #-}

import           Data.Type.Vector
import           Numeric.Backprop.Mono
import           Type.Family.Nat

test :: BP s N3 Double (BPRef s N3 Double Double)
test = withInps $ \(x :* y :* z :* ØV) -> do
    xy  <- newBPRef2 x y  $ op2 (*)
    xyy <- newBPRef2 xy y $ op2 (+)
    newBPRef2 xyy z       $ op2 (*)

main :: IO ()
main = print $ backprop test (2 :+ 3 :+ 4 :+ ØV)
