{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( toPicture, export, mandelbrot
    , FractalParams(..), Size(..), Position(..), Zoom, Image
    , size, position, zoom, innerColor, outerColor
    , updatePos
    ) where
import Codec.BMP(writeBMP, packRGBA32ToBMP24)
import Codec.Picture(readBitmap, writeDynamicPng)
import Control.Lens(makeLenses, (^.), (+~))
import qualified Data.ByteString as BS
import Data.Complex(Complex(..))
import Data.Word(Word8)
import qualified Graphics.Gloss as G
import System.Directory(removeFile)

type ColorPart = Word8
type Color = (ColorPart, ColorPart, ColorPart)
type Image = ([Color], Size)
type Zoom a = a

data Size = Size {width :: Int, height :: Int}

data Position a = Pos {_x :: a, _y :: a}
makeLenses ''Position

data FractalParams a =
  F { _size :: Size
    , _position :: Position a
    , _zoom :: Zoom a
    , _innerColor :: G.Color
    , _outerColor :: G.Color
    }
makeLenses ''FractalParams

updatePos :: (Real a, Fractional (Zoom b)) => a -> a -> FractalParams b -> FractalParams b
updatePos x' y' mp = newCoord x x' . newCoord y y' $ mp
  where newCoord c c' = position.c +~ realToFrac c'/_zoom mp

toList :: Color -> [ColorPart]
toList (r, g, b) = [r, g, b, 255]

toPicture :: Image -> G.Picture
toPicture (pxs, s) = G.bitmapOfByteString (width s) (height s) (pixelsToByteString pxs) True
  where pixelsToByteString = BS.pack . concatMap (reverse . toList)

{-# SPECIALIZE mandelbrot :: FractalParams Float -> Image #-}
{-# SPECIALIZE mandelbrot :: FractalParams Double -> Image #-}
mandelbrot :: (Enum (Zoom a), RealFloat (Zoom a)) => FractalParams a -> Image
mandelbrot mp = (map (getPixel' . iterations (255::Int) 3) coords, _size mp)
  where getPixel' = getPixel (toFloats innerColor) (toFloats outerColor)
        toFloats = G.rgbaOfColor . (mp ^.)
        coords = getCoordinates (_size mp) (toComplex $ _position mp) (_zoom mp)
        toComplex p = _x p :+ _y p

getCoordinates :: (Enum a, RealFloat a) => Size -> Complex a -> a -> [Complex a]
getCoordinates s p z = [ (r/z :+ i/z) + p
                       | i <- range $ height s
                       , r <- range $ width s]
  where range n = let half = (fromIntegral n-1)/2 in [-half..half]

iterations :: (Eq a, Num a, RealFloat b) => a -> b -> Complex b -> a
iterations iterLimit zlimit c = go c 0
  where magIsGTzLimit (r:+i) = r * r + i * i > zlimit * zlimit
        go prev count | prev `seq` count == iterLimit || magIsGTzLimit prev = count
                      | otherwise = go (prev*prev + c) (count + 1)

getPixel :: (Integral a, Real b) => (Float,Float,Float,x) -> (Float,Float,Float,y) -> b -> (a,a,a)
getPixel (r1, g1, b1, _) (r2, g2, b2, _) n = (avg r1 r2, avg g1 g2, avg b1 b2)
  where avg start end = truncate $ end + (frac * (start - end))
        frac = 9 * logBase 1.25 (1+realToFrac n::Float)

export :: FilePath -> Image -> IO ()
export path (pxs, s) = do
  writeBMP path . packRGBA32ToBMP24 (width s) (height s) .
    BS.pack . concatMap toList $ pxs
  (Right im) <- readBitmap path
  _ <- writeDynamicPng (path ++ ".png") im
  removeFile path
