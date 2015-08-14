module Intersection where

import AuxiliaryFunctions
import Shapes

sphereIntersect :: Ray -> Sphere -> [Point]
sphereIntersect Ray {origin = (a,b,c), dir = (x,y,z)}
                Sphere {center = (d,e,f), radius = r}
  | p < r = [(a,b,c) .+ ((p + rtCond).*(x,y,z)),
             (a,b,c) .+ ((p - rtCond).*(x,y,z))]
  | p == r = [p.*(x,y,z)]
  | otherwise = []
  where
    p = (x,y,z) `dotProd` oc
    cond = p^2 - (sqNorm oc) + r^2
    rtCond = sqrt cond
    oc = (a,b,c) .- (d,e,f)
