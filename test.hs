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

spheres = [ Sphere (0, 0, -10)  6 (Mat (200, 0, 0) 0.15 1 0)
          , Sphere (4, 4, -1)   1 (Mat (0, 255, 0) 0.15 1 1)
          , Sphere (-9, 9, -7) 5 (Mat (0, 0, 255) 0.15 1 1)
          , Sphere (0,5,-30) 10 (Mat (0,255,255) 0.15 1 1)
          ]

planes = [ Plane { Plane.origin = (0, -6, -10), Plane.normal = (0, 1, 0),
                   Plane.mat = Mat (255, 180, 0) 0.5 1 1}
         ]

main = putStr . show $ render (4000, 2000) (40, 20) 40 scene

