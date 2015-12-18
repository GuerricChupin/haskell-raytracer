module Intersectable ( Intersectable
                     , hit
                     , contains
                     , firstIntersection
                     , Direction (..)
                     ) where

import Geometry

import Data.Maybe (isJust, fromJust)

-- whether a ray enters or leave a solid at an intersection point.
-- for non-closed objects (e.g. a plane), rays should always be Leaving.
data Direction = Entering | Leaving
   deriving (Eq, Show)

class Intersectable a where
   hit :: Ray -> a -> Bool
   hit r obj = isJust $ firstIntersection r obj
   contains :: a -> Point -> Bool
   firstIntersection :: Ray -> a -> Maybe ( Point
                                          , Vector
                                          , Direction
                                          )
