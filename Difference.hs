module Difference ( Difference (Diff)
                  , (\\\)
                  ) where

import Renderable
import Data.Maybe (isNothing, fromJust, isJust)
import Geometry

data Difference l r = Diff l r

(\\\) :: l -> r -> Difference l r
l \\\ r = Diff l r

instance (Renderable l, Renderable r) => Renderable (Difference l r) where
   hit r d = isJust $ firstIntersection r d
   contains (Diff l r) p = not (r `contains` p) && (l `contains` p)
   firstIntersection ray d@(Diff l r)
      | isNothing hit  = Nothing
      | r `contains` p = firstIntersection (ray { origin = p }) d
      | otherwise      = hit
      where hit = firstIntersection ray l
            info@IntersectInfo { point = p } = fromJust hit

