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
import Data.List (minimumBy)
import Data.Function (on)
import Debug.Trace

epsilon :: Double
epsilon = 1.0e-12

data Sphere = Sphere { center  :: Point
                     , radius  :: Double
                     , color   :: Color
                     , reflect :: Double
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
   {-
   firstIntersection ray s | null l = Nothing
                           | otherwise = Just $ minimumBy (compare `on` (distance (origin ray))) l
                           -}
                           where l = sphereIntersect ray s
   normal s p = Ray { origin = p
                    , dir    = p .- center s}
   colorAt p s = color s
   reflectAt p s = reflect s
