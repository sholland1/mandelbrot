module Lib
    ( toPicture
    , export
    , mandelbrot
    , Size(..)
    ) where
import Graphics.Gloss(Picture, bitmapOfByteString)
import qualified Data.ByteString as BS
import Data.Complex(Complex(..), magnitude)
import Data.Word(Word8)
import Codec.BMP(writeBMP, packRGBA32ToBMP24)
import Codec.Picture(readBitmap, writeDynamicPng)
import System.Directory(removeFile)

type MyReal = Double
type ColorPart = Word8
type Image = ([PixelColor], Size)

data Size = Size {width :: Int, height :: Int}
data PixelColor = PixelColor ColorPart ColorPart ColorPart ColorPart --red, green, blue, alpha

toList :: PixelColor -> [ColorPart]
toList (PixelColor r g b alpha) = [r, g, b, alpha]

toPicture :: Image -> Picture
toPicture (pxs, s) = bitmapOfByteString (width s) (height s) (pixelsToByteString pxs) True
  where pixelsToByteString = BS.pack . concatMap (reverse . toList)

mandelbrot :: Size -> Complex MyReal -> MyReal -> Image
mandelbrot s p z = (map (getGreyPixel . iterations 255 2) $ getCoordinates s p z, s)

getCoordinates :: Size -> Complex MyReal -> MyReal -> [Complex MyReal]
getCoordinates s p z = [ (r/z :+ i/z) + p
                       | i <- range $ height s
                       , r <- range $ width s]
  where range n = let half = (fromIntegral n-1)/2 in [-half..half]

iterations :: (Eq a, Num a, RealFloat b) => a -> b -> Complex b -> a
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

export :: FilePath -> Image -> IO ()
export path (pxs, s) = do
  writeBMP path . packRGBA32ToBMP24 (width s) (height s) .
    BS.pack . concatMap toList $ pxs
  (Right im) <- readBitmap path
  _ <- writeDynamicPng (path ++ ".png") im
  removeFile path
