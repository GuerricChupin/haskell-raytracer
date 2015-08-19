import Scene
import Render
import Sphere
import LightSource
import Material
import Plane
import Difference
import qualified Geometry as G

main = putStr . show $ render (400, 400) (20, 20) 40 scene

scene = Scene {
     world =   Sphere (f (0, 0, -10)) 6 (Mat (200, 0, 0) 0.3 1.4 0.1)
           ||| Sphere (f (4, 4, -1))   1 (Mat (0, 255, 0) 0.25 1.1 0.5)
           ||| Sphere (f (-9, 9, -7)) 5 (Mat (0, 0, 255) 0.4 1 0.9)
           ||| Sphere (f (0,5,-26)) 7 (Mat (0,255,255) 0.1 1 0.8)
           ||| Sphere (f (0.6, -0.6, -18)) 0.3 (Mat (255, 0, 255) 0 1 1)
           ||| Plane { origin = f (0, -6, -10)
                     , normal = (0, 1, 0)
                     , mat    = Mat (255, 180, 0) 0.5 1 1
                     }

   , source = LightSource { direction = (1, 1, 1) }
   }

f = G.rotatePt (0, 0, -10) (0,1,0) 0

