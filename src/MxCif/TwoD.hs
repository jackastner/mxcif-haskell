module MxCif.TwoD where

import qualified MxCif.OneD as OneD

import Geom.Rectangle
import Geom.Point
import qualified Geom.Span as S

import Debug.Trace

data MxCif a b =
  Empty {
    bounds :: Rectangle b
  } |
  Tree {
    bounds :: Rectangle b,
    xTree :: OneD.MxCif a (XAxis b),
    yTree :: OneD.MxCif a (YAxis b),
    nw :: MxCif a b,
    ne :: MxCif a b,
    sw :: MxCif a b,
    se :: MxCif a b
  } deriving Show

insert e b t@(Tree b' x y nw ne sw se)
  | (S.spanLength $ YAxis b') <= 1 = trace "pixel" $ t { yTree = OneD.insert e (YAxis b) y }
  | (S.spanLength $ XAxis b') <= 1 = trace "pixel" $ t { xTree = OneD.insert e (XAxis b) x }
  | (bounds nw) `contains` b = trace "nw" $ t { nw = insert e b nw }
  | (bounds ne) `contains` b = trace "ne" $ t { ne = insert e b ne }
  | (bounds sw) `contains` b = trace "sw" $ t { sw = insert e b sw }
  | (bounds se) `contains` b = trace "se" $ t { se = insert e b se }
  | b' `contains` b && (XAxis b) `S.contains` (PointX $ midpoint b') =
      trace "y" $ t { yTree = OneD.insert e (YAxis b) y }
  | b' `contains` b && (YAxis b) `S.contains` (PointY $ midpoint b') =
      trace "x" $ t { xTree = OneD.insert e (XAxis b) x }
  | otherwise = error "Out of bounds MxCif.TwoD insertion"
insert e b (Empty b') = insert e b $ Tree b' (OneD.Empty $ XAxis b') (OneD.Empty $ YAxis b') (Empty nw) (Empty ne) (Empty sw) (Empty se)
  where (nw, ne, sw, se) = quadrants b'
