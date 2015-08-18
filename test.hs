import Scene
import Render
import Sphere
import LightSource
import Material
import Plane
import Difference

main = putStr . show $ render (400, 400) (20, 20) 40 scene

scene = Scene {
     world = Sphere (4, 4, -1)   1 (Mat (0, 255, 0) 0.3)
         ||| Sphere (-9, 9, -7) 5 (Mat (0, 0, 255) 0.3)
         ||| (   (   Plane { Plane.origin = (0, -6, -10)
                   , Plane.normal = (0, 1, 0)
                   , Plane.mat = Mat (255, 180, 0) 0.5
                   }
                 ||| Sphere (0, 0, -10)  6 (Mat (200, 0, 0) 0.3)
                 )
             \\\ Sphere (5, -5, -5)  6 anyMat
             )

   , source = LightSource { direction = (1, 1, 1) }
   }

