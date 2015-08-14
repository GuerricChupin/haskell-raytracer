module Shapes ( Sphere (Sphere)
              , center
              , radius
              ) where 

import Renderable
import GeometricTypes

data Sphere = Sphere { center :: Point
                     , radius :: Double
                     }
