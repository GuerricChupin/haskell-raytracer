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
import LightSource
import Data.Maybe (isNothing, fromJust, isJust)
import Debug.Trace
import Material

minExposure = 0.1
maxReflection = 5
maxRefraction = 5

-- camera is fixed at (0, 0, d) and the screen is orthogonal to the camera and
-- is a rectange centered in origin of size (a, b).
render :: (Renderable a)
       => ImageDefinition -> (Double, Double) -> Double
       -> Scene a
       -> Image
render (w, h) (a, b) d scene = Image $ M.fromList h w $
   map (pointColor scene d 0 0)
      [Ray {origin = cameraPos,
            dir    = (x, y, 0) .- cameraPos,
            refr   = 1} | y <- ordinates, x <- abscissas]
   where
   cameraPos = (0, 0, d)
   abscissas =
     [a * (-0.5 + x / fromIntegral w) | x <- map fromIntegral [0..(w - 1)]]
   ordinates =
     [b * (-0.5 + y / fromIntegral h) | y <- map fromIntegral [(h - 1), (h - 2)..0]]

-- Only the closest intersection to the screen is considered.
pointColor :: (Renderable a)
           => Scene a -> Double -> Int -> Int -> Ray -> Color
pointColor scene d acc acc' r | acc >= maxReflection
                                || acc' >= maxRefraction
                                || isNothing hit = black
                              | otherwise = --traceShow (n2, dir r `crossProd` refrDir)
   darken (op * (1-reflFactor) * max minExposure realExposure) (color mat) .+
   darken reflFactor reflColor .+
   refrResult
   where
   hit = firstIntersection r (world scene)
   IntersectInfo {point = p, normal = n', localMat = mat, n2 = n2} = fromJust hit
   n = normalise n'
   lightDir = normalise $ direction (source scene)
   cameraPos = (0, 0, d)
   lightRay = Ray {origin = p, dir = lightDir, refr = 1}
   shadow = isJust $ firstIntersection lightRay $ world scene
   realExposure =
     if shadow
     then 0
     else max 0 (lightDir `dotProd` n)
   n1 = refr r
   reflFactor = reflect mat
   reflRay = Ray {origin = p, dir = neg (dir r `sym` n), refr = n1}
   reflColor = pointColor scene d (acc + 1) acc' reflRay
   op = opacity mat
   iSin = norm (n `crossProd` (dir r)) / ((norm (dir r)) * (norm n))
   refrRatio = n1 / n2
   totRefl = rSin > 1
   rSin = refrRatio * iSin
   refrDir = refrRatio.*(dir r) .+
             ((refrRatio*(sqrt (1 - iSin^2)) - (sqrt (1-rSin^2))).* n)
   refrRay = Ray {origin = p, dir = refrDir, refr = n2}
   refrColor = pointColor scene d acc (acc' + 1) refrRay
   refrResult =
     if totRefl
     then white
     else darken ((1 - op) * (1 - reflFactor)) refrColor

