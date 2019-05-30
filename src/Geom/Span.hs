module Geom.Span where

class Span s where
  start :: s a -> a
  end   :: s a -> a
  split :: Fractional a => s a -> (s a, s a)

contains :: (Span s0, Span s1, Ord a) => s0 a -> s1 a -> Bool
contains s0 s1 = (start s0) <= (start s1) &&
                 (end s0) > (end s1)

midpoint :: (Span s, Fractional a) => s a -> a
midpoint s = ((start s) + (end s)) / 2

showSpan :: (Span s, Show a) => s a -> String
showSpan s = "[" ++ (show . start $ s) ++ " -> " ++ (show . end $ s) ++ "]"
