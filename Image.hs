module Image (Image
             ) where

import qualified Data.Matrix as M
import Data.List (intersperse)
import Pixel

newtype Image = Image { toMatrix :: M.Matrix Pixel }

instance Show Image where
   show img = "P3\n"
      ++ show (M.ncols m) ++ " " ++ show (M.nrows m)
      ++ "\n255\n"
      ++ concatMap ((++ "\n") . concat . intersperse " " . map toPPM) (M.toLists m)
      where m = toMatrix img

