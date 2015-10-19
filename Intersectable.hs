module Intersectable ( Intersectable
                     , hit
                     , contains
                     , firstIntersection
                     , Direction (..)
                     ) where

import Geometry (Point, Vector, Ray)
import Data.Maybe (isJust)

-- whether a ray enters or leave a solid at an intersection point.
-- for non-closed objects (e.g. a plane), rays should always be Leaving.
data Direction = Entering | Leaving
   deriving (Eq)

class Intersectable a where
   hit :: Ray -> a -> Bool
   hit r obj = isJust $ firstIntersection r obj
   contains :: a -> Point -> Bool
   firstIntersection :: A.Exp Ray -> a -> Maybe ( Point
                                                , Vector
                                                , Direction
                                                )

