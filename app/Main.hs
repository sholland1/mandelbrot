module Main where

import Lib
import Graphics.Gloss

main :: IO ()
main = display
  (InWindow "Hello World" (1920, 1080) (0, 0))
  white $ toPicture $ mandelbrot size
-- main = export "myBrot" $ mandelbrot size

size = Size {width = screenX, height = screenY}

screenX = 1920
screenY = 1080
