module Union ( Union (Union)
             , (|||)
             ) where

import Renderable
import Geometry
import AuxiliaryFunctions (minOn)
import Material
import qualified Intersectable as I

data Union a b = Union a b

infixl 6 |||
(|||) :: a -> b -> Union a b
a ||| b = Union a b

instance (I.Intersectable a, I.Intersectable b)
   => I.Intersectable (Union a b) where
   I.contains (Union a b) p = contains a p || contains b p
   I.costlyFirstIntersection r (Union a b) = case (m, m') of
      (Nothing, m') -> m'
      (m, Nothing)  -> m
      (Just i, Just i') -> Just $ minOn (distance o . point) i i'
      where m = I.firstIntersection r a
            m' = I.firstIntersection r b
            o = origin r
      I.boundingBox (Union a b) =
         bimapBB min max (I.boundingBox a) (I.boundingBox b)

instance (Renderable a, Renderable b) => Renderable (Union a b) where
   firstIntersection r u@(Union a b)
      | intersectBB r (boundingBox u) = case (m, m') of
         (Nothing, m') -> m'
         (m, Nothing)  -> m
         (Just i, Just i') -> Just $ minOn (distance o . point) i i'
         where m = firstIntersection r a
               m' = firstIntersection r b
               o = origin r

