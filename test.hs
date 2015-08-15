import Scene
import Render
import Sphere
import LightSource

scene = Scene {
     objs = [ MkSceneObject $ Sphere { center = (0, 0, -10), radius = 6,
                                       color  = (200, 0, 0) }
            , MkSceneObject $ Sphere { center = (4, 4, -1), radius = 1,
                                       color  = (0, 255, 0) }
            , MkSceneObject $ Sphere { center = (-9,9,-10) , radius = 5,
                                       color  = (0,0,255) }
            ]
   , source = LightSource { direction = (-1, 1, 0) }
   }

main = putStr . show $ render (1000, 1000) (20, 20) 20 scene

