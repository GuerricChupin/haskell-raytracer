import Scene
import Render
import Sphere
import LightSource
import Material
import Plane
import Shading
import Chessboard (chessboardShaded)
import Geometry ((.+), (.-))
import qualified Geometry as G
import Color (black, white)
import Intersection
import BiconvexLens
import Image

main = render (1366, 768) (27.32, 15.36) 40 scene >>= putStr . show
       -- render (1366 * 2, 768 * 2) (27.32, 15.36) 40 scene >>= putStr . show

scene = Scene {
   world = chessboardShaded Plane { origin = rotP (0, 0, -25)
                                  , normal = rotV $ (0,0,1)
                                  }
           (rotV (1, 0, -0.1)) 5 (Mat white 0 1 1) (Mat black 0 1 1)
         ||| biconvexLens o (0,0,1) 15 2 `uniform` Mat black 0.1 1.4 0

   , source = LightSource { direction = rotV (0,0,1) }
   }

r = 25.25
o = (0, 2, -10)
angle = pi / 4
axis = (1, 0, 0)
rotV = G.rotateVect axis angle
rotP = G.rotatePt o axis angle

