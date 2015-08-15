module Color ( Color
             , toPPM
             , white
             , black
             , darken
             ) where

type Color = (Int, Int, Int)

toPPM (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

white :: Color
white = (255, 255, 255)

black :: Color
black = (0, 0, 0)

-- darken by the given coefficient between 0 and 1. darken 0 is _ -> black
-- and darken 1 is 'id'.
darken :: Double -> Color -> Color
darken x (r, g, b) = (f r, f g, f b)
   where f n = floor (x * fromIntegral n)

