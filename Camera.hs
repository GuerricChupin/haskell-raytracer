module Camera ( Camera(..)
              ) where

import Geometry

data Camera = Camera { centreScreenPosition :: Point
                     , orientation :: Vector
                     , toScreenDistance :: Double
                     }
