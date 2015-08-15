module Sphere ( Sphere (Sphere)
              , center
              , radius
              , color
              , sphereIntersect
              ) where 

import Data.List (nub)
import Renderable
import GeometricTypes
import AuxiliaryFunctions
import Color

data Sphere = Sphere { center :: Point
                     , radius :: Double
                     , color  :: Color
                     } deriving (Eq)

sphereIntersect :: Ray -> Sphere -> [Point]
sphereIntersect Ray {origin = (a,b,c), dir = (x,y,z)}
                Sphere {center = (d,e,f), radius = r}
  | cond > max 0 p && cond < -p =
      [(a,b,c) .+ ((-p + cond).*normdir),
       (a,b,c) .+ ((-p - cond).*normdir)]
  | otherwise = []
  where
    normdir = normalise (x,y,z)
    p = normdir `dotProd` oc
    cond = sqrt $ p^2 - (sqNorm oc) + r^2
    oc = (a,b,c) .- (d,e,f)

instance Renderable Sphere where
   hit r s = not $ null $ sphereIntersect r s
   intersections r s = sphereIntersect r s
   normal s p = Ray { origin = p
                    , dir    = p .- center s}
   colorAt p s = color s
