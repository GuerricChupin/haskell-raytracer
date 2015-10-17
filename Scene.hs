module Scene ( Scene (Scene)
             , world
             , source
             , (Union.|||)
             ) where

import Renderable
import LightSource
import Union ((|||))
   
data Scene a = Scene { world :: a
                     , source :: LightSource
                     }

