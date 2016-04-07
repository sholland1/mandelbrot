module Lib
    ( toPicture
    , export
    , mandelbrot
    , Size(..)
    ) where
import Codec.BMP(writeBMP, packRGBA32ToBMP24)
import Codec.Picture(readBitmap, writeDynamicPng)
import qualified Data.ByteString as BS
import Data.Complex(Complex(..), magnitude)
import Data.Word(Word8)
import Graphics.Gloss(Picture, bitmapOfByteString)
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
mandelbrot s p z = (map (getPixel' . iterations (255::Int) 4) $ getCoordinates s p z, s)
  where getPixel' = getPixel (PixelColor 0 255 255 255) (PixelColor 255 0 255 255)

getCoordinates :: Size -> Complex MyReal -> MyReal -> [Complex MyReal]
getCoordinates s p z = [ (r/z :+ i/z) + p
                       | i <- range $ height s
                       , r <- range $ width s]
  where range n = let half = (fromIntegral n-1)/2 in [-half..half]

iterations :: (Eq a, Num a, RealFloat b) => a -> b -> Complex b -> a
iterations iterLimit zlimit c = go c 0
  where go prev count | count == iterLimit || magnitude prev > zlimit = count
                      | otherwise = go (prev*prev + c) (count + 1)

getPixel :: Real a => PixelColor -> PixelColor -> a -> PixelColor
getPixel (PixelColor r1 g1 b1 _) (PixelColor r2 g2 b2 _) n =
  PixelColor (avg r1 r2) (avg g1 g2) (avg b1 b2) 255
  where avg s e = s - (c * (s - e))
        c = 10 * truncate (logBase 1.25 (realToFrac n::Float))

export :: FilePath -> Image -> IO ()
export path (pxs, s) = do
  writeBMP path . packRGBA32ToBMP24 (width s) (height s) .
    BS.pack . concatMap toList $ pxs
  (Right im) <- readBitmap path
  _ <- writeDynamicPng (path ++ ".png") im
  removeFile path
