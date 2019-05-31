module Draw.Draw where

import SDL.Video
import SDL.Event
import SDL.Init
import SDL.Time

import Data.Text (pack)
import Data.StateVar (($=))
import Data.Int
import Data.Maybe

import System.Exit

import Control.Monad
import Control.Monad.State.Lazy

import Linear.V4
import Linear.V2
import Linear.Affine

import Foreign.C.Types

import qualified MxCif.TwoD as TwoD
import qualified MxCif.OneD as OneD
import qualified Geom.Rectangle as R
import qualified Geom.Point as P

data ControllerState b = ControllerState {
  tree :: TwoD.MxCif Int b,
  firstClick :: Maybe (P.Point b),
  count :: Int
}

displayMxCif :: Integral b => TwoD.MxCif Int b -> IO ()
displayMxCif t = do
  initializeAll
  w <- createWindow  (pack "MxCif") defaultWindow
  r <- createRenderer w (-1) defaultRenderer

  draw r t

  evalStateT (forever $ do
    mapEvents handleEvent
    (tree <$> get) >>= draw r
    present r
    delay 50) (ControllerState t Nothing 0)

handleEvent :: Integral b => Event -> StateT (ControllerState b) IO()
handleEvent (Event _ QuitEvent) = liftIO exitSuccess
handleEvent (Event _ (MouseButtonEvent e)) =
  when ((mouseButtonEventMotion e) == Released) $ do
    prev <- firstClick <$> get
    let p = fromSDLPoint $ mouseButtonEventPos e
    modify $ (\s -> case prev of
      Nothing -> s {
        firstClick = Just p
      }
      (Just p0) -> s {
        tree       = TwoD.insert (count s) (makeRectangle p0 p) (tree s),
        firstClick = Nothing,
        count      = 1 + count s
      })
handleEvent _ = return ()

makeRectangle :: Integral a => P.Point a -> P.Point a -> R.Rectangle a
makeRectangle (P.Point x0 y0) (P.Point x1 y1) =
  R.Rectangle (P.Point (min x0 x1) (min y0 y1)) (P.Point (max x0 x1) (max y0 y1))

fromSDLPoint :: Integral b => (Point V2 Int32) ->  P.Point b
fromSDLPoint (P (V2 x y)) = P.Point (fromIntegral x) (fromIntegral y)

class Drawable a where
  draw :: (MonadIO m) => Renderer -> a -> m ()

instance Integral a => Drawable (R.Rectangle a) where
  draw r rect = drawRect r . Just . toSDLRect $ rect

instance Integral a => Drawable (R.XAxis a) where
  draw r = draw r . R.xRect

instance Integral a => Drawable (R.YAxis a) where
  draw r = draw r . R.yRect

toSDLRect :: Integral a => R.Rectangle a -> Rectangle CInt
toSDLRect (R.Rectangle (P.Point _x0 _y0) (P.Point _x1 _y1)) =
  let [x0, y0, x1, y1] = fromIntegral <$> [_x0, _y0, _x1, _y1] in
    Rectangle (P $ V2 x0 y0) (V2 (x1 - x0) (y1 - y0))

instance Integral b => Drawable (TwoD.MxCif a b) where
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
