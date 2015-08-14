module Pixel ( Pixel
             , toPPM
             , white
             , black
             ) where

type Pixel = (Int, Int, Int)

toPPM (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

white :: Pixel
white = (255, 255, 255)

black :: Pixel
black = (0, 0, 0)

