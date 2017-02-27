{-# LANGUAGE GADTs #-}

import           Data.Type.Vector
import           Numeric.Backprop.Mono
import           Type.Family.Nat
import qualified Numeric.Backprop.Op as O

test :: BP s N3 Double (BPRef s N3 Double Double)
test = withInps $ \(x :* y :* z :* ØV) -> do
    xy  <- newBPRef2 x y $ O.op2' (*)
    xyy <- newBPRef2 xy y $ O.op2' (+)
    newBPRef2 xyy z $ O.op2' (*)

main :: IO ()
main = print $ backprop test (2 :+ 3 :+ 4 :+ ØV)
