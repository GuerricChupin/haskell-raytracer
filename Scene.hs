module Scene ( Scene (Scene)
             , world
             , source
             , (Union.|||)
             ) where

import qualified Data.Matrix as M
import Renderable
import LightSource
import Union ((|||))
   
data Scene a = Scene { world :: a
                     , source :: LightSource
                     }

