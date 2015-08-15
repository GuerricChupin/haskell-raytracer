import Scene
import Render
import Sphere
import LightSource

scene = Scene {
     objs = [ MkSceneObject $ Sphere { center = (0, 0, -3), radius = 5,
                                       color  = (200, 0, 0) }
            , MkSceneObject $ Sphere { center = (4, 4, -1), radius = 1,
                                       color  = (0, 255, 0) }
            ]
   , source = LightSource { direction = (-1, 1, 1) }
   }

main = putStr . show $ render (500, 500) (20, 20) 20 scene

