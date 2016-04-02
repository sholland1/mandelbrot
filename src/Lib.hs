module Lib
    ( mandelbrotPicture
    , ScreenSize(..)
    ) where
import Graphics.Gloss(Picture, bitmapOfByteString)
import qualified Data.ByteString as BS
import Data.Complex(Complex(..), magnitude)
import Data.Word(Word8)

type MyReal = Float
type ColorPart = Word8
data ScreenSize = ScreenSize {width :: Int, height :: Int}
data PixelColor = PixelColor ColorPart ColorPart ColorPart ColorPart --red, green, blue, alpha

toList :: PixelColor -> [ColorPart]
toList (PixelColor r g b alpha) = [alpha, b, g, r]

mandelbrotPicture :: ScreenSize -> Picture
mandelbrotPicture s = bitmapOfByteString (width s) (height s) (mandelbrotByteString s) True
  where mandelbrotByteString = BS.pack . concatMap toList . mandelbrot

mandelbrot :: ScreenSize -> [PixelColor]
mandelbrot = map (getGreyPixel . iterations 255 4) . getCoordinates

getCoordinates :: ScreenSize -> [Complex MyReal]
getCoordinates s = [(r/400 - 0.75) :+ i/400 | i <- range (height s), r <- range (width s)]
  where range n = let half = (fromIntegral n-1)/2 in [-half..half]

iterations :: ColorPart -> MyReal -> Complex MyReal -> ColorPart
iterations iterLimit zlimit c = go c 0
  where go prev count | count == iterLimit || magnitude prev > zlimit = count
                      | otherwise = go (prev*prev + c) (count + 1)

getGreyPixel :: ColorPart -> PixelColor
getGreyPixel n = PixelColor c c c 255
  where c = (fromIntegral::Int->ColorPart) (truncate $ logBase 1.25 (fromIntegral n::Float)) * 10

getBluePixel :: ColorPart -> PixelColor
getBluePixel n = PixelColor n n 255 255

getGradientPixel :: ColorPart -> PixelColor
getGradientPixel n = PixelColor n 255 (255-n) 255
