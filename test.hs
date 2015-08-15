import Scene
import Render
import Sphere
import LightSource

scene = Scene {
     objs = map MkSceneObject spheres
   , source = LightSource { direction = (-1, 1, 0) }
   }

spheres = [ Sphere (0, 0, -10)  6 (200, 0, 0)
          , Sphere (4, 4, -1)   1 (0, 255, 0)
          , Sphere (-9, 9, -10) 5 (0, 0, 255)
          ]

main = putStr . show $ render (1000, 1000) (20, 20) 20 scene

