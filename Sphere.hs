module Sphere ( Sphere (Sphere)
              , center
              , radius
              , color
              , reflect
              , sphereIntersect
              ) where 

import Data.List (nub)
import Renderable
import GeometricTypes
import AuxiliaryFunctions
import Color
import Data.List ( minimumBy
                 )
import Data.Function (on)
import Debug.Trace

epsilon :: Double
epsilon = 1.0e-11


data Sphere = Sphere { center  :: Point
                     , radius  :: Double
                     , color   :: Color
                     , reflect :: Double
                     } deriving (Eq)

sphereIntersect :: Ray -> Sphere -> [Point]
sphereIntersect Ray {origin = (a,b,c), dir = (x,y,z)}
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

instance Renderable Sphere where
   hit r s = not $ null $ sphereIntersect r s
   contains s p = distance p (center s) < radius s
   --{-
   firstIntersection ray s
      | null inters = Nothing
      | otherwise   = Just $
         minimumBy (compare `on` (distance (origin ray))) inters
      where inters = sphereIntersect ray s
   normal s p = Ray { origin = p
                    , dir    = p .- center s}
   colorAt p s = color s
   reflectAt p s = reflect s
