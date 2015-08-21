module Image ( Image (Image)
             , ImageDefinition
             ) where

import qualified Data.Array.Repa as A
import Data.Array.Repa.Algorithms.Matrix (row, col)
import Data.List (intersperse)
import Data.Char (isNumber)
import Data.String (words)
import Color

newtype Image = Image { toArray :: A.Array A.U A.DIM2 Color }

instance Show Image where
   show img = "P3\n"
      ++ show (col shm) ++ " " ++ show (row shm)
      ++ "\n255\n"
      ++ concatMap ((++ "\n") . concat . intersperse " " . map toPPM) (toLists $ A.toList m)
      where m = toArray img
            shm = A.extent m
            toLists [] = []
            toLists x = (take (col shm) x):(toLists $ drop (col shm) x)

-- (width, height) in pixels
type ImageDefinition = (Int, Int)

