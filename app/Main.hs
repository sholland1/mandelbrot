module Main where

import Data.Complex(Complex(..))
import Lib
import GHC.Float(float2Double)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit(exitSuccess)

main :: IO ()
main = playIO
  (InWindow "Mandelbrot" (screenX, screenY) (0, 0))
  white
  24
  (size, (-0.75) :+ 0, 300)
  (\(s, p, z) -> return . toPicture $ mandelbrot s p z)
  eventHandler
  (const return)

-- main = display
--   (InWindow "Hello World" (1920, 1080) (0, 0))
--   white $ toPicture $ mandelbrot size
-- main = export "myBrot" $ mandelbrot size

size = Size {width = screenX, height = screenY}

screenX = 200
screenY = 160

eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) w = return exitSuccess w
eventHandler (EventKey (MouseButton LeftButton) Up _ (clickX, clickY)) (s, p, z) =
  return (s, (float2Double clickX/z :+ float2Double clickY/z) + p, z)
eventHandler (EventKey (Char 'i') Down _ _) (s, p, z) = return (s, p, z*1.1)
eventHandler (EventKey (Char 'o') Down _ _) (s, p, z) = return (s, p, z*0.9)
eventHandler (EventKey (Char 'x') Down _ _) w@(s, p, z) = do
  export "myBrot" $ mandelbrot s p z
  return w
eventHandler _ w = return w
