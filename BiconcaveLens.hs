module BiconcaveLens ( biconcaveLensWithFocalDistance
                     ) where

import Geometry
import Difference
import Sphere
import Shading
import Material
import Color

type BiconcaveLens = Difference (Difference Sphere Sphere) Sphere 

biconcaveLens :: Point -> Vector -> Double -> Double
              -> BiconcaveLens
biconcaveLens center axis diameter thickness =
   Diff (Diff (Sphere center (diameter / 2)) s) s'
   where s = Sphere (center .+ ((r - e/2) .* n)) r
         s' = Sphere (center .- ((r - e/2) .* n)) r
         r = (thickness^2 + diameter^2)/(4 * thickness)
         n = normalise axis
         e = -thickness

-- f' must be negative.
biconcaveLensWithFocalDistance :: Point -> Vector -> Double -> Double -> Double
                               -> ShadedObject BiconcaveLens Uniform
biconcaveLensWithFocalDistance center axis diameter f' n =
   biconcaveLens center axis diameter thickness `uniform` Mat black 0 n 0
   where thickness = (n * outerRefr * diameter^2 / 4) / ((n - outerRefr)^2 * (-f'))

