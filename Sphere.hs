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
    cond = p^2 - (sqNorm oc) + r^2
    scond = sqrt cond
    oc = (a,b,c) .- (d,e,f)
    ppc = -p + scond
    pmc = -p - scond

instance Intersectable Sphere where
   hit r s = isJust $ firstIntersection r s
   contains s p = distance p (center s) < radius s - epsilon
   firstIntersection ray s
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
