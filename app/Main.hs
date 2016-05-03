module Main where

import Control.Lens((*~))
import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit(exitSuccess)

main :: IO ()
main = playIO
  (InWindow windowTitle (screenX, screenY) (0, 0))
  white
  24
  params
  (return . toPicture . fractal)
  eventHandler
  (const return)

-- main = display
--   (InWindow windowTitle (screenX, screenY) (0, 0))
--   white $ toPicture $ fractal params

-- main = export imageName $ fractal params

-- main = do
--   foo <- return $! toPicture $ fractal params
--   print foo
--   print "done."

fractal :: (Enum (Zoom a), RealFloat (Zoom a)) => FractalParams a -> Image
fractal = mandelbrot

params :: FractalParams Double
params = F (Size screenX screenY) (Pos (-0.75) 0) 300 white black

windowTitle = "Mandelbrot"
imageName = "myBrot"
screenX = 1920
screenY = 1080

eventHandler :: (Enum (Zoom a), RealFloat (Zoom a)) =>
                Event -> FractalParams a -> IO (FractalParams a)
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) = return exitSuccess
eventHandler (EventKey (MouseButton LeftButton) Up _ (clickX, clickY)) =
  return . updatePos clickX clickY
eventHandler (EventKey (Char 'i') Down _ _) = return . (zoom *~ 1.1)
eventHandler (EventKey (Char 'o') Down _ _) = return . (zoom *~ 0.9)
eventHandler (EventKey (Char 'x') Down _ _) = \fp -> export imageName (fractal fp) >> return fp
eventHandler _ = return
