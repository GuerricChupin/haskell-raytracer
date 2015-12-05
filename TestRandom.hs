import Scene
import Render
import Sphere
import LightSource
import Material
import Shading
import Geometry ((.+), (.-))
import qualified Geometry as G
import System.Random
import Camera

main = render (1366, 768) camera scene >>= putStr . show
       -- render (1366 * 2, 768 * 2) (27.32, 15.36) 40 scene >>= putStr . show

n = 500

camera :: Camera
camera = Camera (0,0,0) (0,0,1) 0 (pi/4, pi/8) 40

spheres :: [Object]
spheres = [Object $ Sphere (x,y,z) 0.5 `uniform` Mat (28,133,150) 0 1 1
          | (x,y,z) <- take n . group 3 $
                       randomRs (-28,28) (mkStdGen 42) ]
   where toTuple [x,y,z] = (x,y,z)

scene = mkScene
   LightSource { direction = (1,1,1) }
   spheres

group :: Int -> [a] -> [(a,a,a)]
group n [] = []
group n (x:y:z:xs) = (x,y,z) : group n xs
