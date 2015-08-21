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

main =
  putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene -- std for testing
  -- putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene -- bigger

scene = Scene {
   world =   {-Sphere (rotP (o .+ (-10,0,-20))) 4 `uniform` Mat (200,0,0) 0.1 1.4 0.3
         ||| Sphere (rotP o) 4 `uniform` Mat (0,200,0) 0.1 1.4 0.3
         ||| Sphere (rotP (o .+ (10,0,-20))) 4 `uniform` Mat (0,0,200) 0.1 1.4 0.3
         |||-} chessboardShaded Plane { origin = rotP (0, 0, -25)
                                    , normal = rotV $ (0,0,1)
                                    }
                              (rotV (1, 0, -0.1)) 5 (Mat white 0 1 1) (Mat black 0 1 1)
         ||| Inter (Sphere (rotP (o .+ (0,0,-r+1))) r) (Sphere (rotP (o .+ (0,0,r-1))) r) `uniform` Mat black 0.1 1.4 0

   , source = LightSource { direction = rotV (1,1,1) }
   }

r = 25.25
o = (0, 2, -10)
angle = pi / 12
rotV = G.rotateVect (1, 0, 0) angle
rotP = G.rotatePt (0, 0, -10) (1, 0, 0) angle

