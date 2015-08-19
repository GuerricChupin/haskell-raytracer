import Scene
import Render
import Sphere
import LightSource
import Material
import Plane
import Difference

main =
  putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene -- std for testing
  -- putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene -- bigger

scene = Scene {
     world =   Sphere (0, 0, -10)  6 (Mat (200, 0, 0) 0.3 1.4 0)
           ||| Sphere (4, 4, -1)   1 (Mat (0, 255, 0) 0.25 1.1 0.5)
           ||| Sphere (-9, 9, -7) 5 (Mat (0, 0, 255) 0.4 1 0.9)
           ||| Sphere (0,5,-26) 7 (Mat (0,255,255) 0.1 1 1)
           ||| Sphere (0.6, -0.6, -18) 0.3 (Mat (255, 0, 255) 0 1 1)
           ||| Plane { Plane.origin = (0, -6, -10)
                     , Plane.normal = (0, 1, 0)
                     , Plane.mat    = Mat (255, 180, 0) 0.1 1 1
                     }

   , source = LightSource { direction = (1, 1, 1) }
   }

