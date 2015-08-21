module Difference ( Difference (Diff)
                  , (\\\)
                  ) where

import Data.Maybe (isNothing, fromJust, isJust)
import Geometry
import Intersectable

data Difference l r = Diff l r

(\\\) :: l -> r -> Difference l r
l \\\ r = Diff l r

instance (Intersectable l, Intersectable r)
       => Intersectable (Difference l r) where
   contains (Diff l r) p = not (r `contains` p) && l `contains` p
   firstIntersection ray d@(Diff l r)
      | isNothing hit  = Nothing
      | r `contains` p = firstIntersection (ray { origin = p }) d
      | otherwise      = hit
      where hit = firstIntersection ray l
            (p,_,_) = fromJust hit

