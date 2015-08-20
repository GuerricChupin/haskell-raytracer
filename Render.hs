module Render ( render
              ) where

import Scene
import Sphere
import Image
import Color
import qualified Data.Matrix as M
import Geometry
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
pointColor scene d acc acc' r
  | acc >= maxReflection
    || acc' >= maxRefraction
    || isNothing hit = black
  | otherwise = objResult .+ reflResult .+ refrResult .+ reflRefrResult
  where
    -- minimum informations
    hit = firstIntersection r (world scene)
    IntersectInfo {point = p, normal = n', localMat = mat, n2 = n2} =
      fromJust hit
    n = normalise n'
    lightDir = normalise $ direction (source scene)
    cameraPos = (0, 0, d)
    op = opacity mat
    lightRay = Ray {origin = p, dir = lightDir, refr = 1}
    -- natural object color calculation and shadowing
    shadow = isJust $ firstIntersection lightRay $ world scene
    realExposure =
      if shadow
      then 0
      else max 0 (lightDir `dotProd` n)
    objResult
      | op == 0 || reflFactor == 1 = black
      | otherwise =
          darken (op  * (1 -reflFactor) * max minExposure realExposure)
                 (color mat)
    -- reflection 
    reflFactor = reflect mat
    reflRay = Ray {origin = p, dir = neg (dir r `sym` n), refr = n1}
    reflColor = pointColor scene d (acc + 1) acc' reflRay
    reflResult
      | reflFactor == 0 = black
      | otherwise = darken reflFactor reflColor
    -- refraction
    n1 = refr r
    iSin = norm (n `crossProd` (dir r)) / ((norm (dir r)) * (norm n))
    refrRatio = n1 / n2
    rSin = refrRatio * iSin
    totRefl = rSin > 1
    iCos = sqrt $ 1 - iSin^2
    rCos = sqrt $ 1 - rSin^2
    reflCoef
      | totRefl = 1
      | n1 <= n2 = ro + (1-ro) * (1-iCos)^5 
      | otherwise = ro + (1-ro) * (1-rCos)^5
      where ro = ((n1 - n2)/(n1 + n2))^2
    transCoef = 1 - reflCoef
    refrDir
      | n1 == n2 = dir r
      | otherwise = (refrRatio / iN) .*(dir r) .+
                    ((refrRatio * iCos - rCos).* realNormal)
      where realNormal = if dir r `dotProd` n < 0 then n else neg n
            iN = norm (dir r)
    refrRay = Ray {origin = p, dir = refrDir, refr = n2}
    refrColor = pointColor scene d acc (acc' + 1) refrRay
    refrResult
      | totRefl || op == 1 || transCoef == 0 = black
      | otherwise =  darken ((1 - op) * transCoef * (1-reflFactor)) refrColor
    -- reflected part after the refraction
    reflRefrResult
      | reflCoef == 0 = black
      | otherwise = darken ((1 - op) * reflCoef * (1-reflFactor)) reflColor
