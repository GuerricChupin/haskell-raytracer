import Scene
import Render
import Sphere
import LightSource

scene = Scene {
     objs = map MkSceneObject spheres
   , source = LightSource { direction = (1, 1, 1) }
   }

spheres = [ Sphere (0, 0, -10)  6 (200, 0, 0) 0.3
          , Sphere (4, 4, -1)   1 (0, 255, 0) 0.3
          , Sphere (-9, 9, -7) 5 (0, 0, 255) 0.3
          ]

main = putStr . show $ render (2000, 2000) (20, 20) 40 scene

