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

spheres = [ Sphere (0, 0, -10)  6 (Mat (200, 0, 0) 0.15 1.4 0.2)
          , Sphere (4, 4, -1)   1 (Mat (0, 255, 0) 0.25 1.2 0.5)
          , Sphere (-9, 9, -7) 5 (Mat (0, 0, 255) 0.4 2 0.9)
          , Sphere (0,5,-30) 10 (Mat (0,255,255) 0.1 1.7 0.8)
          ]

planes = [ Plane { Plane.origin = (0, -6, -10), Plane.normal = (0, 1, 0),
                   Plane.mat = Mat (255, 180, 0) 0.5 1 1}
         ]

main = putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene

