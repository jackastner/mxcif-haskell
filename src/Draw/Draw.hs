module Draw.Draw where

import SDL.Video
import SDL.Event
import SDL.Init

import Data.Text (pack)
import Data.StateVar (($=))

import System.Exit

import Control.Monad

import Linear.V4
import Linear.V2
import Linear.Affine

import Foreign.C.Types

import qualified MxCif.TwoD as TwoD
import qualified MxCif.OneD as OneD
import qualified Geom.Rectangle as R
import qualified Geom.Point as P

displayMxCif :: RealFrac b => TwoD.MxCif a b -> IO ()
displayMxCif t = do
  initializeAll
  w <- createWindow  (pack "MxCif") defaultWindow
  r <- createRenderer w (-1) defaultRenderer

  draw r t

  present r

  forever $ mapEvents
    (\e -> case e of
      (Event _ QuitEvent) -> exitSuccess
      _ -> return ())

class Drawable a where
  draw :: Renderer -> a -> IO ()

instance RealFrac a =>  Drawable (R.Rectangle a) where
  draw r rect = drawRect r . Just . toSDLRect $ rect

instance RealFrac a => Drawable (R.XAxis a) where
  draw r = draw r . R.xRect

instance RealFrac a => Drawable (R.YAxis a) where
  draw r = draw r . R.yRect

toSDLRect :: RealFrac a => R.Rectangle a -> Rectangle CInt
toSDLRect (R.Rectangle (P.Point _x0 _y0) (P.Point _x1 _y1)) =
  let [x0, y0, x1, y1] = fromIntegral . round <$> [_x0, _y0, _x1, _y1] in
    Rectangle (P $ V2 x0 y0) (V2 (x1 - x0) (y1 - y0))

instance RealFrac b => Drawable (TwoD.MxCif a b) where
  draw r t = do
    rendererDrawColor r $= V4 0xff 0xff 0xff 0xff
    draw r $ TwoD.bounds t
    case t of 
     (TwoD.Empty _) -> return ()
     (TwoD.Tree _ x y nw ne sw se) -> do
       mapM_ (draw r) [nw, ne, sw, se]
       draw r x
       draw r y

instance (Drawable b) => Drawable (OneD.MxCif a b) where
  draw r (OneD.Empty _) = return ()
  draw r (OneD.Tree rects _ left right) = do
    rendererDrawColor r $= V4 0x00 0xff 0x00 0x00
    mapM_ (draw r . snd) rects
    mapM_ (draw r) [left, right]
