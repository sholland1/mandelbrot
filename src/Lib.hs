{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( toPicture, export, mandelbrot, burningShip
    , FractalParams(..), Size(..), Position(..), Zoom, Image
    , size, position, zoom, innerColor, outerColor
    , updatePos
    ) where
import Codec.BMP(writeBMP, packRGBA32ToBMP24)
import Codec.Picture(readBitmap, writeDynamicPng)
import Control.Lens(makeLenses, (^.), (+~))
import qualified Data.ByteString as BS
import Data.Complex(Complex(..),realPart,imagPart)
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

updatePos :: (Real a, Fractional (Zoom b)) =>
             a -> a -> FractalParams b -> FractalParams b
updatePos x' y' fp = newCoord x x' . newCoord y (-y') $ fp
  where newCoord c c' = position.c +~ realToFrac c'/_zoom fp

toList :: Color -> [ColorPart]
toList (r, g, b) = [r, g, b, 255]

toPicture :: Image -> G.Picture
toPicture (pxs, s) = G.bitmapOfByteString (width s) (height s) (pixelsToByteString pxs) True
  where pixelsToByteString = BS.pack . concatMap (reverse . toList)

{-# SPECIALIZE burningShip :: FractalParams Float -> Image #-}
{-# SPECIALIZE burningShip :: FractalParams Double -> Image #-}
burningShip :: (Enum (Zoom a), RealFloat (Zoom a)) => FractalParams a -> Image
burningShip = generateFractal $ iterations abs (255::Int) 3

{-# SPECIALIZE mandelbrot :: FractalParams Float -> Image #-}
{-# SPECIALIZE mandelbrot :: FractalParams Double -> Image #-}
mandelbrot :: (Enum (Zoom a), RealFloat (Zoom a)) => FractalParams a -> Image
mandelbrot = generateFractal $ iterations id (255::Int) 3

generateFractal :: (Enum (Zoom a), RealFloat (Zoom a), Real b) =>
                   (Complex (Zoom a) -> b) -> FractalParams a -> Image
generateFractal iterationFun fp = (map (getPixel' . iterationFun) coords, _size fp)
  where getPixel' = getPixel (toFloats innerColor) (toFloats outerColor)
        toFloats = G.rgbaOfColor . (fp ^.)
        coords = getCoordinates (_size fp) (toComplex $ _position fp) (_zoom fp)
        toComplex p = _x p :+ _y p

getCoordinates :: (Enum a, RealFloat a) => Size -> Complex a -> a -> [Complex a]
getCoordinates s p z = [ (r/z :+ i/z) + p
                       | i <- reverse . range $ height s
                       , r <- range $ width s]
  where range n = let half = (fromIntegral n-1)/2 in [-half..half]

{-# SPECIALIZE iterations :: (Float -> Float) -> Int -> Float -> Complex Float -> Int #-}
{-# SPECIALIZE iterations :: (Double -> Double) -> Int -> Double -> Complex Double -> Int #-}
iterations :: (Eq a, Num a, RealFloat b) => (b -> b) -> a -> b -> Complex b -> a
iterations f iterLimit zlimit c = go c 0
  where magIsGTzLimit (r:+i) = r * r + i * i > zlimit * zlimit
        go prev count | prev `seq` count == iterLimit || magIsGTzLimit prev = count
                      | otherwise = go (prev'*prev' + c) (count + 1)
          where prev' = f (realPart prev) :+ f (imagPart prev)

getPixel :: (Integral a, Real b) => (Float,Float,Float,x) -> (Float,Float,Float,y) -> b -> (a,a,a)
getPixel (r1, g1, b1, _) (r2, g2, b2, _) n = (avg r1 r2, avg g1 g2, avg b1 b2)
  where avg start end = truncate $ 255 * (end + frac * (start - end))
        frac = logBase 1.25 (1+realToFrac n::Float)/25

export :: FilePath -> Image -> IO ()
export path (pxs, s) = do
  writeBMP path . packRGBA32ToBMP24 (width s) (height s) .
    BS.pack . concatMap toList $ pxs
  (Right im) <- readBitmap path
  _ <- writeDynamicPng (path ++ ".png") im
  removeFile path
