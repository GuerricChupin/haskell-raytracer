module Scene ( hit,
             , SceneObject (MkSceneObject)
             ) where

class Renderable a where
   hit :: Ray -> a -> Bool

data SceneObject = forall a. Renderable a => MkSceneObject a

instance Renderable SceneObject where
   hit r (SceneObject a) = hit r a

