import Scene
import Render
import Sphere
import LightSource
import Material
import Plane

scene = Scene {
     objs = map MkSceneObject spheres
            ++ map MkSceneObject planes
   , source = LightSource { direction = (1, 1, 1) }
   }

spheres = [ Sphere (0, 0, -10)  6 (Mat (200, 0, 0) 0.3)
          , Sphere (4, 4, -1)   1 (Mat (0, 255, 0) 0.3)
          , Sphere (-9, 9, -7) 5 (Mat (0, 0, 255) 0.3)
          ]

planes = [ Plane { Plane.origin = (0, -6, -10), Plane.normal = (0, 1, 0),
                   Plane.mat = Mat (255, 180, 0) 0.5 }
         ]

main = putStr . show $ render (1000, 1000) (20, 20) 40 scene

