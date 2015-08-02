module Pixel ( Pixel
             , toPPM
             ) where

type Pixel = (Int, Int, Int)

toPPM (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

