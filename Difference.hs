module Difference ( Difference (Diff)
                  ) where

import Scene
import Renderable
import Data.Maybe (isNothing, fromJust, isJust)
import GeometricTypes

data Difference = Diff SceneObject SceneObject

instance Renderable Difference where
   hit r d = isJust $ firstIntersection r d
   contains (Diff a b) p = not (b `contains` p) && (a `contains` p)
   firstIntersection r d@(Diff a b)
      | isNothing hit  = Nothing
      | b `contains` p = firstIntersection (r { origin = p }) d
      | otherwise      = Just p
      where hit = firstIntersection r a
            p   = fromJust hit
   normal (Diff a b) p = normal a p
   colorAt p (Diff a b) = colorAt p a
   reflectAt p (Diff a b) = reflectAt p a

