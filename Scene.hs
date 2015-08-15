{-# LANGUAGE ExistentialQuantification #-}

module Scene ( hit
             , SceneObject (MkSceneObject)
             , Scene (Scene)
             , objs
             , source
             ) where

import qualified Data.Matrix as M
import Renderable
import LightSource

data SceneObject = forall a. Renderable a => MkSceneObject a

instance Renderable SceneObject where
   hit r (MkSceneObject a) = hit r a
   intersections r (MkSceneObject a) = intersections r a
   normal (MkSceneObject a) p = normal a p

data Scene = Scene { objs :: [SceneObject]
                   , source :: LightSource
                   }

