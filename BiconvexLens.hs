module BiconvexLens ( biconvexLens
                    ) where

import Geometry
import Intersection
import Sphere

biconvexLens :: Point -> Vector -> Double -> Double
             -> Intersection Sphere Sphere
biconvexLens center axis diameter thickness = Inter s s'
   where s = Sphere (center .+ ((r - thickness/2) .* n)) r
         s' = Sphere (center .- ((r - thickness/2) .* n)) r
         r = (thickness^2 + diameter^2)/(4 * thickness)
         n = normalise axis

