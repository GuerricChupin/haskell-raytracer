module Render ( render
              ) where

import Scene
import Sphere
import Image
import Color
import Geometry
import Renderable
import LightSource
import Data.Maybe (isNothing, fromJust, isJust)
import Material
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as A

minExposure = 0.1
maxReflection = 5
maxRefraction = 5

-- camera is fixed at (0, 0, d) and the screen is orthogonal to the camera and
-- is a rectange centered in origin of size (a, b).
render :: (Renderable a)
       => ImageDefinition -> (Double, Double) -> Double
       -> Scene a
       -> Image
render (w, h) (a, b) d scene =
  Image $ A.run $ A.map (pointColor scene d 0 0) $
  A.use $ A.fromList (A.Z A.:.h A.:.w) $
      [(cameraPos, (x,y,0) .- cameraPos, 1) | y <- ordinates, x <- abscissas]
   where
     cameraPos = (0, 0, d)
     abscissas =
       [a * (-0.5 + x / fromIntegral w) | x <- map fromIntegral [0..(w - 1)]]
     ordinates =
       [b * (-0.5 + y / fromIntegral h) | y <- map fromIntegral [(h - 1), (h - 2)..0]]
       
-- Only the closest intersection to the screen is considered.
pointColor :: (Renderable a)
           => Scene a -> Double -> Int -> Int -> A.Exp Ray -> A.Exp Color
pointColor scene d acc acc' ray
  | acc >= maxReflection
    || acc' >= maxRefraction
    || isNothing hit = A.constant black
  | otherwise = objResult .+ reflResult .+ refrResult .+ reflRefrResult
  where
    r = A.unlift ray :: (A.Exp Point, A.Exp Vector, A.Exp Double)
    -- minimum informations
    hit = firstIntersection r (world scene)
    IntersectInfo {point = toLiftp, normal = toLiftn', localMat = mat, n2 = toLiftn2} =
      fromJust hit
    p = A.constant toLiftp
    n' = A.constant toLiftn'
    n2 = A.constant toLiftn2
    n = normalise n'
    lightDir = normalise $ direction (source scene)
    cameraPos = (0, 0, d)
    op = A.constant $ opacity mat
    lightRay = (p, lightDir, 1)
    -- natural object color calculation and shadowing
    shadow = isJust $ firstIntersection lightRay $ world scene
    realExposure =
      if shadow
      then 0
      else max 0 (lightDir `dotProd` n)
    objResult
      | op == 0 || reflFactor == 1 = A.constant black
      | otherwise =
          darken (op  * (1 -reflFactor) * max minExposure realExposure)
                 (color mat)
    -- reflection 
    reflFactor = A.constant $ reflect mat
    reflRay = (p, neg (dir r `sym` n), n1)
    reflColor = pointColor scene d (acc + 1) acc' reflRay
    reflResult
      | reflFactor == 0 = A.constant black
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
    refrRay = (p, refrDir, n2)
    refrColor = pointColor scene d acc (acc' + 1) (A.lift refrRay)
    refrResult
      | totRefl || op == 1 || transCoef == 0 = A.constant black
      | otherwise =  darken ((1 - op) * transCoef * (1-reflFactor)) refrColor
    -- reflected part after the refraction
    reflRefrResult
      | reflCoef == 0 = A.constant black
      | otherwise = darken ((1 - op) * reflCoef * (1-reflFactor)) reflColor
