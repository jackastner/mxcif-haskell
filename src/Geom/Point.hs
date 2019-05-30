module Geom.Point where

import qualified Geom.Span as S

data Point a = Point {
  x :: a,
  y :: a
} deriving Show

newtype PointX a = PointX (Point a)

instance S.Span PointX where
  start (PointX p) = x p
  end = S.start
  split p = (p, p)

newtype PointY a = PointY (Point a)

instance S.Span PointY where
  start (PointY p) = y p
  end = S.start
  split p = (p, p)
