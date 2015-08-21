module BiconvexLens ( biconvexLens
                    , lensWithFocalDistance
                    ) where

import Geometry
import Intersection
import Sphere
import Shading
import Material
import Color

biconvexLens :: Point -> Vector -> Double -> Double
             -> Intersection Sphere Sphere
biconvexLens center axis diameter thickness = Inter s s'
   where s = Sphere (center .+ ((r - thickness/2) .* n)) r
         s' = Sphere (center .- ((r - thickness/2) .* n)) r
         r = (thickness^2 + diameter^2)/(4 * thickness)
         n = normalise axis

lensWithFocalDistance :: Point -> Vector -> Double -> Double -> Double
                      -> ShadedObject (Intersection Sphere Sphere) Uniform
lensWithFocalDistance center axis diameter f' n =
   biconvexLens center axis diameter thickness `uniform` Mat black 0 n 0
   where thickness = (n * outerRefr * diameter^2 / 4) / ((n - outerRefr)^2 * f')

