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

-- main = do
--   foo <- return $! toPicture $ mandelbrot params
--   print foo
--   print "done."

params :: FractalParams Double
params = F (Size screenX screenY) (Pos (-0.75) 0) 300 white black

screenX = 1920
screenY = 1080

eventHandler :: (Enum (Zoom a), RealFloat (Zoom a)) =>
                Event -> FractalParams a -> IO (FractalParams a)
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) = return exitSuccess
eventHandler (EventKey (MouseButton LeftButton) Up _ (clickX, clickY)) =
  return . updatePos clickX clickY
eventHandler (EventKey (Char 'i') Down _ _) = return . (zoom *~ 1.1)
eventHandler (EventKey (Char 'o') Down _ _) = return . (zoom *~ 0.9)
eventHandler (EventKey (Char 'x') Down _ _) = \mp -> export "myBrot" (mandelbrot mp) >> return mp
eventHandler _ = return
