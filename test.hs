import Scene
import Render
import Shapes
import Intersection

scene = [ MkSceneObject $ Sphere { center = (0, 0, -1), radius = 0.5 }
        , MkSceneObject $ Sphere { center = (0.8, 0.8, -0.4), radius = 0.2 }
        ]

main = putStr . show $ render (500, 500) scene

