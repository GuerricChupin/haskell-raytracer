module Difference ( Difference (Diff)
                  ) where

import Scene
import Renderable

data Difference = Diff SceneObject SceneObject

instance Renderable Difference where
   hit r d = not $ null $ intersections r d
   contains (Diff a b) p = not (b `contains` p) && (a `contains` p)
   intersections r (Diff a b) = filter (not . (b `contains`)) $
      intersections r a
   normal (Diff a b) p = normal a p
   colorAt p (Diff a b) = colorAt p a
   reflectAt p (Diff a b) = reflectAt p a

