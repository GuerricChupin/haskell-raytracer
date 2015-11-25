module Sphere ( Sphere (Sphere)
              , center
              , radius
              , sphereIntersect
              ) where 

import Intersectable
import Geometry
import Data.List ( minimumBy
                 )
import Data.Function (on)
import Data.Maybe (isJust)

epsilon :: Double
epsilon = 1.0e-11

data Sphere = Sphere { center  :: Point
                     , radius  :: Double
                     } deriving (Eq)

sphereIntersect :: Ray -> Sphere -> [Point]
sphereIntersect ((a,b,c),(x,y,z),_)
                Sphere {center = (d,e,f), radius = r}
  | cond > 0 =
      [(a,b,c) .+ (ppc.*normdir) | ppc > epsilon] ++
      [(a,b,c) .+ (pmc.*normdir) | pmc > epsilon]
  | otherwise = []
  where
    normdir = normalise (x,y,z)
    p = normdir `dotProd` oc
    cond = sqrt $ p^2 - (sqNorm oc) + r^2
    oc = (a,b,c) .- (d,e,f)
    ppc = -p + cond
    pmc = -p - cond

instance Intersectable Sphere where
   hit r s = isJust $ firstIntersection r s
   contains s p = distance p (center s) < radius s - epsilon
   getFirstIntersection ray s
      | null inters = Nothing
      | otherwise   = Just $ ( p
                             , p .- center s
                             , if rayEnters
                               then Entering
                               else Leaving
                             )
      where inters = sphereIntersect ray s
            p = minimumBy (compare `on` (distance (origin ray))) inters
            o = origin ray
            rayEnters = (p .- center s) `dotProd` dir ray < epsilon
   boundingBox s = BoundingBox { xmin = x - r
                               , ymin = y - r
                               , zmin = z - r
                               , xmax = x + r
                               , ymax = y + r
                               , zmax = z + r
                               }
     where (x,y,z) = center s
           r = radius s
