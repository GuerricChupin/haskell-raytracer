module Camera ( Camera(..)
              ) where

import Geometry

-- A camera is defined with the position in the 3d space of the center
-- of the screen, a vector to show where it points to. By
-- default, the two axes of the camera are "parallel" to the x
-- and y axis of the scene, the "flip" angle changes
-- that. There are two angles defining the horizontal and
-- vertical openings. The last variable is the distance between
-- the center of projection and the screen.
data Camera = Camera { centreScreenPosition :: Point
                     , orientation :: Vector
                     , flip :: Double
                     , openings :: (Double, Double)
                     , toScreenDistance :: Double
                     }
