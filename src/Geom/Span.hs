module Geom.Span where

class Span s where
  start :: s a -> a
  end   :: s a -> a
  split :: Integral a => s a -> (s a, s a)

contains :: (Span s0, Span s1, Ord a) => s0 a -> s1 a -> Bool
contains s0 s1 = (start s0) <= (start s1) &&
                 (end s0) >= (end s1)

spanLength :: (Span s, Num a) => s a -> a
spanLength s = abs $ (start s) - (end s)

midpoint :: (Span s, Integral a) => s a -> a
midpoint s = ((start s) + (end s)) `div` 2

showSpan :: (Span s, Show a) => s a -> String
showSpan s = "[" ++ (show . start $ s) ++ " -> " ++ (show . end $ s) ++ "]"
