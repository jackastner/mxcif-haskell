{-# LANGUAGE FlexibleInstances #-}

module MxCif.OneD where

import Geom.Span

import Prelude hiding (span)

data MxCif a s =
  Empty  {
    span :: s
  } |
  Tree {
    elems :: [(a, s)],
    span  :: s,
    left  :: MxCif a s,
    right :: MxCif a s
  }

instance (Span s, Show (s a), Show a, Show b) => Show (MxCif b (s a)) where
  show (Empty s) = "Empty {" ++ (showSpan s) ++ "}"
  show t         = "Tree {" ++ (show . elems $ t) ++ ", " ++ (showSpan . span $ t) ++ ", " ++ (show . left $ t) ++ ", " ++ (show . right $ t)  ++ "}"

insert :: (Span s, Ord a, Fractional a) => b -> s a -> MxCif b (s a) -> MxCif b (s a)
insert e s (Tree es s' l r)
  | (span l) `contains` s = Tree es s' (insert e s l) r
  | (span r) `contains` s = Tree es s' l (insert e s r)
  | s'       `contains` s = Tree ((e,s):es) s' l r
  | otherwise             = error "Out of bounds MxCif.OneD insertion"
insert e s (Empty s') = insert e s $ Tree [] s' (Empty r) (Empty l)
  where (l, r) = split s'