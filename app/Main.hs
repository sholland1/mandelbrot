module Main where

import Control.Lens((*~))
import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit(exitSuccess)

main :: IO ()
main = playIO
  (InWindow "Mandelbrot" (screenX, screenY) (0, 0))
  white
  24
  params
  (return . toPicture . mandelbrot)
  eventHandler
  (const return)

-- main = display
--   (InWindow "Mandelbrot" (screenX, screenY) (0, 0))
--   white $ toPicture $ mandelbrot params
-- main = export "myBrot" $ mandelbrot params

params :: MandelbrotParams Float
params = M (Size screenX screenY) (Pos (-0.75) 0) 300 white black

screenX = 200
screenY = 160

eventHandler :: (Enum (Zoom a), RealFloat (Zoom a)) =>
                Event -> MandelbrotParams a -> IO (MandelbrotParams a)
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) = return exitSuccess
eventHandler (EventKey (MouseButton LeftButton) Up _ (clickX, clickY)) =
  return . updatePos clickX clickY
eventHandler (EventKey (Char 'i') Down _ _) = return . (zoom *~ 1.1)
eventHandler (EventKey (Char 'o') Down _ _) = return . (zoom *~ 0.9)
eventHandler (EventKey (Char 'x') Down _ _) = \mp -> export "myBrot" (mandelbrot mp) >> return mp
eventHandler _ = return
