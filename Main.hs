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

scene = mkScene
   LightSource { direction = (1,1,1) }

   ([Object $ Sphere (G.rotatePt (0,0,-10) (0,1,0) (i/n*2*pi) (0,0,-1)) 0.5
      `uniform` Mat (28,133,150) 0 1 1 | i <- [1..n] ] ++
   [Object $ Sphere o 5 `uniform` Mat (20,51,85) 0 1 1 ])

o = (0,0,-10)
n = 50

