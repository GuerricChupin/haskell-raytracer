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
import Camera

main = render camera scene >>= putStr . show

camera :: Camera
camera = Camera (1000,1000) (0,0,0) (0,0,1) 0 (pi/4, pi/4) 10

planes :: [Object]
planes = [Object $ Plane (0,0,5*i) (0,0,1) `uniform` Mat (255,255,255) 0 1 1
         | i <- [1..500] ]

scene :: Scene
scene = mkScene
  LightSource { direction = (0,0,1) }
  planes
