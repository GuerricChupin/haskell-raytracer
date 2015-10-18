module Color ( Color
             , toPPM
             , white
             , black
             , darken
             ) where

import qualified Data.Array.Accelerate as A

type Color = (Int, Int, Int)

toPPM :: Color -> String
toPPM (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

white :: Color
white = (255, 255, 255)

black :: Color
black = (0, 0, 0)

-- darken by the given coefficient between 0 and 1. darken x, where
-- (x <= 0), is _ -> black, darken 1 is 'id' and darken x where x > 1 is
-- "lighten".
darken :: Double -> A.Exp Color -> A.Exp Color
darken x col | x <= 0    = A.constant black
             | x >= 1    = A.constant white
             | otherwise = A.lift (f r, f g, f b)
   where
     (r, g, b) = A.unlift col
     f n = A.floor (A.lift x * A.fromIntegral n)

