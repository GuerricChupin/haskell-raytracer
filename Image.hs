module Image ( Image (Image)
             , ImageDefinition
             ) where

import qualified Data.Matrix as M
import Data.List (intersperse)
import Color

newtype Image = Image { toMatrix :: M.Matrix Color }

instance Show Image where
   show img = "P3\n"
      ++ show (M.ncols m) ++ " " ++ show (M.nrows m)
      ++ "\n255\n"
      ++ concatMap ((++ "\n") . concat . intersperse " " . map toPPM) (M.toLists m)
      where m = toMatrix img

-- (width, height) in pixels
type ImageDefinition = (Int, Int)

