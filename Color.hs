module Color ( Color
             , toPPM
             , white
             , black
             , darken
             , maxColor
             ) where

import AuxiliaryFunctions

import Data.Word
import Data.Ratio

type Color = (Word8, Word8, Word8)

toPPM :: Color -> String
toPPM (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

maxColor :: Word8
maxColor = 255

maxColor16 :: Word16
maxColor16 = fromIntegral maxColor

maxColor' :: Double
maxColor' = (fromIntegral maxColor)

white :: Color
white = (maxColor, maxColor, maxColor)

black :: Color
black = (0, 0, 0)

-- darken by the given coefficient between 0 and 1. darken x, where
-- (x <= 0), is _ -> black, darken 1 is 'id' and darken x where x > 1 is
-- "lighten".
darken :: Double -> Color -> Color
darken x (r, g, b) | x <= 0     = black
                   | otherwise = (f r, f g, f b)
   where
     xm = floor $ x * maxColor' :: Word16
     f n = fromIntegral $ xm * (fromIntegral n) `quot` maxColor16

fradix :: Int
fradix = fromInteger $ floatRadix 0
