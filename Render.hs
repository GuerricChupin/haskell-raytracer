module Render ( render
              ) where

import Scene
import Shapes
import Image
import Pixel
import qualified Data.Matrix as M
import GeometricTypes
import AuxiliaryFunctions
import Debug.Trace

-- camera is fixed at (0, 0, 1) and the screen is orthogonal to the camera and
-- is a rectange defined by the two corners (-1, -1, 0) and (1, 1, 0).
render :: ImageDefinition -> Scene -> Image
render (w, h) objs = Image $ M.fromList w h $
   map pixelColor
      [Ray {origin = cameraPos,
            dir    = traceShow (x, y) ((x, y, 0) .- cameraPos)} | x <- abscissas, y <- ordinates]
   where
   cameraPos = (0, 0, 1)
   abscissas = [-1 + 2 / fromIntegral w * x | x <- map fromIntegral [0..w]]
   ordinates = [-1 + 2 / fromIntegral h * y | y <- map fromIntegral [0..h]]
   pixelColor :: Ray -> Pixel
   pixelColor r | or $ map (hit r) objs = white
                | otherwise             = black

