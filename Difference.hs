module Difference ( Difference (Diff)
                  , (\\\)
                  ) where

import Renderable
import Data.Maybe (isNothing, fromJust, isJust)
import Geometry
import Material (Shader)

data Difference l s r s' = Diff (l s) (r s')

(\\\) :: l s -> r s' -> Difference l s r s'
l \\\ r = Diff l r

instance (Renderable l, Renderable r, Shader s, Shader s')
       => Renderable (Difference l r) where
   hit r d = isJust $ firstIntersection r d
   contains (Diff l r) p = not (r `contains` p) && (l `contains` p)
   firstIntersection ray d@(Diff l r)
      | isNothing hit  = Nothing
      | r `contains` p = firstIntersection (ray { origin = p }) d
      | otherwise      = hit
      where hit = firstIntersection ray l
            info@IntersectInfo { point = p } = fromJust hit

