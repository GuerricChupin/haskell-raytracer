module Sphere ( Sphere (Sphere)
              , center
              , radius
              , sphereIntersect
              ) where 

import Renderable
import GeometricTypes
import AuxiliaryFunctions
import Color
import Data.List ( minimumBy
                 )
import Data.Function (on)
import Debug.Trace
import Data.Maybe (isJust)
import Material

epsilon :: Double
epsilon = 1.0e-11


data Sphere = Sphere { center  :: Point
                     , radius  :: Double
                     , mat :: Material
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
   contains s p = distance p (center s) < radius s
   firstIntersection ray s
      | null inters = Nothing
      | otherwise   = Just $
         minimumBy (compare `on` (distance (origin ray))) inters
      where inters = sphereIntersect ray s
   normal s p = p .- center s
   colorAt _ Sphere { mat = m } = color m
   reflectAt _ Sphere { mat = m } = reflect m
