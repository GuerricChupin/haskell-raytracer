{-# LANGUAGE ExistentialQuantification #-}

module Scene ( Scene (Scene)
             , source
             , Object (Object)
             , mkScene
             ) where

import Renderable
import LightSource
import Data.Function (on)
import Data.List (minimumBy)
import Geometry
import AuxiliaryFunctions (minOn)

data Object = forall a. Renderable a => Object a

instance Renderable Object where
   firstIntersection r (Object a) = firstIntersection r a

data Scene = Scene { world :: [Object]
                   , source :: LightSource
                   }

instance Renderable Scene where
   firstIntersection r s = f (map (firstIntersection r) $ world s)
      where o = origin r
            f :: [Maybe IntersectInfo] -> Maybe IntersectInfo
            f [] = Nothing
            f (Nothing:xs) = f xs
            f ((Just x):xs) = case f xs of
               Nothing -> Just x
               Just y -> Just $ minOn (distance o . point) x y

mkScene :: LightSource -> [Object] -> Scene
mkScene ls w = Scene { source = ls, world = w }

