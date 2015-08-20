module Sphere ( Sphere (Sphere)
              , center
              , radius
              , sphereIntersect
              ) where 

import Renderable
import Geometry
import Color
import Data.List ( minimumBy
                 )
import Data.Function (on)
import Debug.Trace
import Data.Maybe (isJust)
import Material

epsilon :: Double
epsilon = 1.0e-11


data Sphere s = Sphere { center  :: Point
                       , radius  :: Double
                       , mat :: Material s
                       } deriving (Eq)

sphereIntersect :: Ray -> Sphere -> [Point]
sphereIntersect Ray {origin = (a,b,c), dir = (x,y,z)}
                Sphere {center = (d,e,f), radius = r, mat = _}
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

instance Renderable Sphere where
   hit r s = isJust $ firstIntersection r s
   contains s p = distance p (center s) < radius s - epsilon
   firstIntersection ray s
      | null inters = Nothing
      | otherwise   = Just $ IntersectInfo { point = p
                                           , normal = p .- center s
                                           , localMat = mat s
                                           , n2 = if rayEnters
                                                  then refract $ mat s
                                                  else outerRefr
                                           }
      where inters = sphereIntersect ray s
            p = minimumBy (compare `on` (distance (origin ray))) inters
            o = origin ray
            rayEnters = (p .- center s) `dotProd` dir ray < epsilon

