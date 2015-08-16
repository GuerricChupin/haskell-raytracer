module Render ( render
              ) where

import Scene
import Sphere
import Image
import Color
import qualified Data.Matrix as M
import GeometricTypes
import AuxiliaryFunctions
import Renderable
import Data.List (maximumBy,minimumBy)
import Data.Function (on)
import LightSource

minimalExposure = 0.05
maxReflection = 5

-- camera is fixed at (0, 0, d) and the screen is orthogonal to the camera and
-- is a rectange centered in origin of size (a, b).
render :: ImageDefinition -> (Double, Double) -> Double -> Scene -> Image
render (w, h) (a, b) d scene = Image $ M.fromList h w $
   map (pointColor scene d 0)
      [Ray {origin = cameraPos,
         dir    = (x, y, 0) .- cameraPos} | y <- ordinates, x <- abscissas]
   where
   cameraPos = (0, 0, d)
   abscissas =
     [a * (-0.5 + x / fromIntegral w) | x <- map fromIntegral [0..(w - 1)]]
   ordinates =
     [b * (-0.5 + y / fromIntegral h) | y <- map fromIntegral [(h - 1), (h - 2)..0]]

-- Only the closest intersection to the screen is considered.
pointColor :: Scene -> Double -> Int -> Ray -> Color
pointColor scene d acc r | null inters || acc >= maxReflection = black
                         | otherwise   = (darken (max minimalExposure realExposure) (colorAt closestPt closestObj)) .+ (reflColor scene closestPt closestObj)
   where inters = concatMap (\o -> map ((,) o) (intersections r o)) (objs scene)
         (closestObj, closestPt) = minimumBy
            (compare `on` (distance cameraPos . snd)) inters
         n = normalise $ dir $ normal closestObj closestPt
         lightDir = normalise $ direction (source scene)
         cameraPos = (0, 0, d)
         lightRay = Ray {origin = closestPt, dir = lightDir}
         shadow =
           (not $ null $ concatMap (\o -> intersections lightRay o) (objs scene))
           || (lightDir `dotProd` n) < 0
         realExposure =
           if shadow
           then minimalExposure
           else lightDir `dotProd` n 
         reflRay = Ray {origin = closestPt, dir = neg (dir r `sym` n)}
         reflColor :: Scene -> Point -> SceneObject -> Color
         reflColor scene p obj = pointColor scene d (acc + 1) reflRay
