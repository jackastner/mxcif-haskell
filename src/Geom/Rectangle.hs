module Geom.Rectangle where

import qualified Geom.Span as S

import Geom.Point

data Rectangle a = Rectangle {
  p0 :: Point a,
  p1 :: Point a
} deriving Show

newtype XAxis a = XAxis {xRect :: Rectangle a} deriving Show

instance S.Span XAxis where
  start = x . p0 . xRect
  end   = x . p1 . xRect
  split s@(XAxis (Rectangle p0@(Point x0 y0) p1@(Point x1 y1))) =
    (XAxis $ Rectangle p0 (Point (S.midpoint s) y1),
     XAxis $ Rectangle (Point (S.midpoint s) y0) p1)

newtype YAxis a = YAxis {yRect :: Rectangle a} deriving Show

instance S.Span YAxis where
  start = y . p0 . yRect
  end   = y . p1 . yRect
  split s@(YAxis (Rectangle p0@(Point x0 y0) p1@(Point x1 y1))) =
    (YAxis $ Rectangle p0 (Point x1 (S.midpoint s)),
     YAxis $ Rectangle (Point x0 (S.midpoint s)) p1)

quadrants :: Fractional a => Rectangle a -> (Rectangle a, Rectangle a, Rectangle a, Rectangle a)
quadrants r = (nw, ne, sw, se)
  where (YAxis n, YAxis s)   = S.split $ YAxis r
        (XAxis nw, XAxis ne) = S.split $ XAxis n
        (XAxis sw, XAxis se) = S.split $ XAxis s

contains :: Ord a => Rectangle a -> Rectangle a -> Bool
contains r0 r1 = S.contains (XAxis r0) (XAxis r1) &&
                 S.contains (YAxis r0) (YAxis r1)

midpoint :: Fractional a => Rectangle a -> Point a
midpoint r = Point (S.midpoint . XAxis $ r) (S.midpoint . YAxis $ r)
