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
   contains (MkSceneObject a) p = contains a p
   intersections r (MkSceneObject a) = intersections r a
   normal (MkSceneObject a) p = normal a p
   colorAt p (MkSceneObject a) = colorAt p a
   reflectAt p (MkSceneObject a) = reflectAt p a
   
data Scene = Scene { objs :: [SceneObject]
                   , source :: LightSource
                   }

