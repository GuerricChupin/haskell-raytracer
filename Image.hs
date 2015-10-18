module Image ( Image (Image)
             , ImageDefinition
             ) where

import Data.List (intersperse)
import Data.Char (isNumber)
import Data.String (words)
import Color
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Array.Sugar as S

newtype Image = Image { toArray :: A.Array A.DIM2 Color }

instance Show Image where
   show img = "P3\n"
      ++ show col ++ " " ++ show row
      ++ "\n255\n"
      ++ concatMap ((++ "\n") . concat . intersperse " " . map toPPM) (toLists $ A.toList m)
      where m = toArray img
            [row, col] = S.shapeToList $ S.shape m
            toLists [] = []
            toLists x = (take col x):(toLists $ drop col x)

-- (width, height) in pixels
type ImageDefinition = (Int, Int)

