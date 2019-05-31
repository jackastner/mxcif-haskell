import System.IO

import Foreign.C.Types

import Data.List hiding (insert)

import MxCif.TwoD

import Geom.Rectangle
import Geom.Point

import Draw.Draw

main = do
  tree <- buildTree . map readRectangle . lines <$> getContents
  displayMxCif tree


readRectangle = (\(x0:y0:x1:y1:_) -> Rectangle (Point x0 y0) (Point x1 y1)) . map read . words

buildTree rects = foldl (\a (r, n) -> insert n r a) (Empty $ Rectangle (Point 0 0) (Point 200 200)) $ zip rects [1..]
