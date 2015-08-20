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

main =
  putStr . show $ render (1366 , 768) (27.32, 15.36) 40 scene -- std for testing
  -- putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene -- bigger

scene = Scene {
   world =   Sphere (rotP (o .+ (-10,0,-20))) 4 `uniform` Mat (200,0,0) 0.3 1.4 0.3
         ||| Sphere (rotP o) 4 `uniform` Mat (0,200,0) 0.3 1.4 0.3
         ||| Sphere (rotP (o .+ (10,0,-20))) 4 `uniform` Mat (0,0,200) 0.3 1.4 0.3
         ||| chessboardShaded Plane { origin = rotP (0, -5, -10)
                                    , normal = rotV (0, 1, 0)
                                    }
                              (rotV (1, 0, -0.1)) 5 (Mat white 0 1 1) (Mat black 1 1 1)

   , source = LightSource { direction = rotV (1, 1, 1) }
   }

o = (0, 0, -10)
angle = pi/12
rotV = G.rotateVect (1, 0, 0) angle
rotP = G.rotatePt (0, 0, -10) (1, 0, 0) angle

