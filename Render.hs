module Render ( render
              ) where

import Scene
import Sphere
import Image
import Color
import Geometry
import Renderable
import LightSource
import Material
import Camera
import AuxiliaryFunctions

import Data.Maybe (isNothing, fromJust, isJust)
import qualified Data.Array.Repa as A

minExposure = 0.1
maxReflection = 5
maxRefraction = 5

-- camera is fixed at (0, 0, d) and the screen is orthogonal to the camera and
-- is a rectange centered in origin of size (a, b).
render :: (Monad m)
       => Camera
       -> Scene
       -> m Image
render (Camera (w, h) c o f (wo, ho) d) scene =
  Image <$> (A.computeP $ A.map (pointColor scene 0 0) $
  A.fromFunction (A.Z A.:.h A.:.w) mkRays)
   where
     mkRays :: A.DIM2 -> Ray
     mkRays (A.Z A.:.i A.:.j) =
       (cameraOrigin
       , rotateVect no f $ d
       .* (((tan $ (-0.5 + fromIntegral j / fromIntegral w) * wo) .* x)
       .+  ((tan $ (0.5 - fromIntegral i / fromIntegral h) * ho) .* y))
       .+ (d .* no)
       , 1
       )
     no@(nox, noy, noz) = normalise o
     cameraOrigin = c .- (d .* no)
     x = normalise $ rotateVect (0,1,0) (dxo) (1,0,0)
     y = no `crossProd` x
     dxo | noz == 0 = (sgn nox) * pi/2
         | otherwise = atan (nox/noz)

-- Only the closest intersection to the screen is considered.
pointColor :: Scene -> Int -> Int -> Ray -> Color
pointColor scene acc acc' r
  | acc >= maxReflection
    || acc' >= maxRefraction
    || isNothing hit = black
  | otherwise = objResult .+ reflResult .+ refrResult .+ reflRefrResult
  where
    -- minimum informations
    hit = firstIntersection r scene
    IntersectInfo {point = p, normal = n', localMat = mat, n2 = n2} =
      fromJust hit
    n = normalise n'
    lightDir = normalise $ direction (source scene)
    op = opacity mat
    lightRay = (p, lightDir, 1)
    -- natural object color calculation and shadowing
    shadow = isJust $ firstIntersection lightRay $ scene
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
    reflRay = (p, neg (dir r `sym` n), n1)
    reflColor = pointColor scene (acc + 1) acc' reflRay
    reflResult
      | reflFactor == 0 = black
      | otherwise = darken reflFactor reflColor
    -- refraction
    n1 = refr r
    iSin = norm (n `crossProd` (dir r)) / ((norm (dir r)) * (norm n))
    refrRatio = n1 / n2
    rSin = refrRatio * iSin
    totRefl = rSin > 1
    iCos = sqrt $ 1 - iSin^*^2
    rCos = sqrt $ 1 - rSin^*^2
    reflCoef
      | totRefl = 1
      | n1 <= n2 = ro + (1-ro) * (1-iCos)^*^5
      | otherwise = ro + (1-ro) * (1-rCos)^*^5
      where ro = ((n1 - n2)/(n1 + n2))^*^2
    transCoef = 1 - reflCoef
    refrDir
      | n1 == n2 = dir r
      | otherwise = (refrRatio / iN) .*(dir r) .+
                    ((refrRatio * iCos - rCos).* realNormal)
      where realNormal = if dir r `dotProd` n < 0 then n else neg n
            iN = norm (dir r)
    refrRay = (p, refrDir, n2)
    refrColor = pointColor scene acc (acc' + 1) refrRay
    refrResult
      | totRefl || op == 1 || transCoef == 0 = black
      | otherwise =  darken ((1 - op) * transCoef * (1-reflFactor)) refrColor
    -- reflected part after the refraction
    reflRefrResult
      | reflCoef == 0 = black
      | otherwise = darken ((1 - op) * reflCoef * (1-reflFactor)) reflColor
