module Pixel ( Pixel
             , toPPM
             , white
             , black
             , lighten
             ) where

type Pixel = (Int, Int, Int)

toPPM (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

white :: Pixel
white = (255, 255, 255)

black :: Pixel
black = (0, 0, 0)

-- lighten by the given coefficient between 0 and 1. lighten 0 is _ -> black
-- and lighten 1 is _ -> white.
lighten :: Double -> Pixel -> Pixel
lighten x (r, g, b) = (f r, f g, f b)
   where f n = floor (x * fromIntegral n)

