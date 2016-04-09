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
import qualified Graphics.Gloss as G
import System.Directory(removeFile)
import Control.Lens

type ColorPart = Word8
type Color = (ColorPart, ColorPart, ColorPart)
type Image = ([Color], Size)

data Size = Size {width :: Int, height :: Int}

toList :: Color -> [ColorPart]
toList (r, g, b) = [r, g, b, 255]

toPicture :: Image -> G.Picture
toPicture (pxs, s) = G.bitmapOfByteString (width s) (height s) (pixelsToByteString pxs) True
  where pixelsToByteString = BS.pack . concatMap (reverse . toList)

{-# SPECIALIZE mandelbrot :: Size -> Complex Float -> Float -> Image #-}
{-# SPECIALIZE mandelbrot :: Size -> Complex Double -> Double -> Image #-}
mandelbrot :: (Enum a, RealFloat a) => Size -> Complex a -> a -> Image
mandelbrot s p z = (map (getPixel' . iterations (255::Int) 4) $ getCoordinates s p z, s)
  where getPixel' = getPixel (toInts G.white) (toInts G.black)
        toInts = (each %~ truncate . (* 255)) . G.rgbaOfColor

getCoordinates :: (Enum a, RealFloat a) => Size -> Complex a -> a -> [Complex a]
getCoordinates s p z = [ (r/z :+ i/z) + p
                       | i <- range $ height s
                       , r <- range $ width s]
  where range n = let half = (fromIntegral n-1)/2 in [-half..half]

iterations :: (Eq a, Num a, RealFloat b) => a -> b -> Complex b -> a
iterations iterLimit zlimit c = go c 0
  where go prev count | count == iterLimit || magnitude prev > zlimit = count
                      | otherwise = go (prev*prev + c) (count + 1)

getPixel :: (Integral a, Real b) => (a,a,a,x) -> (a,a,a,y) -> b -> (a,a,a)
getPixel (r1, g1, b1, _) (r2, g2, b2, _) n = (avg r1 r2, avg g1 g2, avg b1 b2)
  where avg start end = start - (c * (start - end))
        c = 10 * truncate (logBase 1.25 (realToFrac n::Float))

export :: FilePath -> Image -> IO ()
export path (pxs, s) = do
  writeBMP path . packRGBA32ToBMP24 (width s) (height s) .
    BS.pack . concatMap toList $ pxs
  (Right im) <- readBitmap path
  _ <- writeDynamicPng (path ++ ".png") im
  removeFile path
