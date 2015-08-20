module Difference ( Difference (Diff)
                  , (\\\)
                  ) where

import Renderable
import Data.Maybe (isNothing, fromJust, isJust)
import Geometry
import qualified Intersectable as I

data Difference l r = Diff l r

(\\\) :: l -> r -> Difference l r
l \\\ r = Diff l r

instance (Renderable l, I.Intersectable r) => Renderable (Difference l r) where
   firstIntersection ray d@(Diff l r)
      | isNothing hit  = Nothing
      | r `I.contains` p = firstIntersection (ray { origin = p }) d
      | otherwise      = hit
      where hit = firstIntersection ray l
            info@IntersectInfo { point = p } = fromJust hit

