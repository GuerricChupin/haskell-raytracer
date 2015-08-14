{-# LANGUAGE ExistentialQuantification #-}

module Scene ( hit
             , SceneObject (MkSceneObject)
             , Scene
             ) where

import qualified Data.Matrix as M
import Renderable

data SceneObject = forall a. Renderable a => MkSceneObject a

instance Renderable SceneObject where
   hit r (MkSceneObject a) = hit r a

type Scene = [SceneObject]

