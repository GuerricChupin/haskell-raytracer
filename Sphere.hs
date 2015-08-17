module Sphere ( Sphere (Sphere)
              , center
              , radius
              , sphereIntersect
              ) where 

import Renderable
import GeometricTypes
import AuxiliaryFunctions
import Color
import Data.List (minimumBy)
import Data.Function (on)
import Debug.Trace
import Data.Maybe (isJust)
import Material

epsilon :: Double
epsilon = 1.0e-12

data Sphere = Sphere { center  :: Point
                     , radius  :: Double
                     , mat :: Material
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
   hit r s = isJust $ firstIntersection r s
   contains s p = distance p (center s) < radius s
   --{-
   firstIntersection ray s | distance o c > r =
      if tc < 0
      then Nothing
      else if d > r
           then Nothing
           else Just $ o .+ (t1 .* u)
                         | otherwise = Just $ o .+ ((tc + t1c) .* u)
      where tc = (c .- o) `dotProd` u
            d = distance c (o .+ (tc .* u))
            r = radius s
            t1c = sqrt (r^2 - d^2)
            t1 = tc - t1c
            o = origin ray .+ (epsilon .* u)
            u = normalise (dir ray)
            c = center s
   ---}
   normal s p = p .- center s
   colorAt _ Sphere { mat = m } = color m
   reflectAt _ Sphere { mat = m } = reflect m
