module Graphics where

import Data.Text (Text, foldl')
import Data.Char (ord)
import Data.Bits (shiftR)
import Data.Int (Int32, Int64)

import Pattern (patterns)

type Point = (Double, Double)
type Size = (Double, Double)
type Angle = Double

type FontSize = Double
data Anchor = Start | Middle | End

type StrokeWidth = Double
type Opacity = Double
data RGB = RGB Double Double Double

type PatternID = Text

data Graphics =
  Graphics
  { text     :: Maybe Angle -> Anchor -> FontSize -> Point -> [Text] -> [Text]
  , rect     :: Point -> Size -> [Text]
  , line     :: Point -> Point -> [Text]
  , polygon  :: [Point] -> [Text]
  , pattern  :: Text -> (PatternID, [Text])
  , visual   :: Maybe (Either PatternID RGB) -> Maybe Opacity -> Maybe RGB -> Maybe StrokeWidth -> [Text] -> [Text]
  , document :: Size -> [Text] -> [Text] -> [Text]
  }

rescalePoint :: (Point,Point) -> (Point,Point) -> Point -> Point
rescalePoint ((inX0,inY0),(inX1,inY1)) ((outX0,outY0),(outX1,outY1)) (x,y) =
  let inW = inX1 - inX0
      inH = inY1 - inY0
      outW = outX1 - outX0
      outH = outY1 - outY0
  in  ((x - inX0) / inW * outW + outX0, (y - inY0) / inH * outH + outY0)

black, white :: RGB
black = RGB 0 0 0
white = RGB 1 1 1

colour :: Text -> RGB
colour s = hsvToRGB (c 0x12345) (c 0x6789a) (c 0xbcdef)
  where
    c m = fromIntegral (hashText m s) * 667 / fromIntegral (maxBound :: Int32)

hsvToRGB :: Double -> Double -> Double -> RGB
hsvToRGB h0 s0 v0 =
  let s = 0.5 + 0.5 * (s0 - (fromIntegral (floor s0 :: Int)))
      v = 0.5 + 0.5 * (v0 - (fromIntegral (floor v0 :: Int)))
      h = floor h0 :: Int
      i = h `mod` 6
      f = h0 - fromIntegral h
      p = v * (1 - s)
      q = v * (1 - s * f)
      t = v * (1 - s * (1 - f))
  in  case i of
        0 -> RGB v t p
        1 -> RGB q v p
        2 -> RGB p v t
        3 -> RGB p q v
        4 -> RGB t p v
        _ -> RGB v p q

patternIx :: Text -> Int
patternIx s = fromIntegral (hashText 0x54321 s * 555) `mod` length patterns

-- hashing functions copied and modified from BSD-style LICENSE'd
-- base-4.3.1.0:Data.HashTable (c) The University of Glasgow 2003

hashText :: Int32 -> Text -> Int32
hashText magic = foldl' f golden
  where f m c = fromIntegral (ord c) * magic + hashInt32 m

hashInt32 :: Int32 -> Int32
hashInt32 x = mulHi x golden + x

mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
  where r :: Int64
        r = fromIntegral a * fromIntegral b

golden :: Int32
golden = 1013904242
