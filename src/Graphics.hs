module Graphics where

import Data.ByteString.Char8 (ByteString)

type Point = (Double, Double)
type Size = (Double, Double)
type Angle = Double

type FontSize = Double
data Anchor = Start | Middle | End

type StrokeWidth = Double
type Opacity = Double
data RGB = RGB Double Double Double

data Graphics =
  Graphics
  { text     :: Maybe Angle -> Anchor -> FontSize -> Point -> [ByteString] -> [ByteString]
  , rect     :: Point -> Size -> [ByteString]
  , line     :: Point -> Point -> [ByteString]
  , polygon  :: [Point] -> [ByteString]
  , visual   :: Maybe RGB -> Maybe Opacity -> Maybe RGB -> Maybe StrokeWidth -> [ByteString] -> [ByteString]
  , document :: Size -> [ByteString] -> [ByteString]
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

colours :: [RGB]
colours = map hueToRGB [0, 2 * pi / (phi * phi) ..]
  where
    phi = (sqrt 5 + 1) / 2
    hueToRGB h =
      let s = 1
          v = 1
          hh = h * 3 / pi
          i = floor hh `mod` 6 :: Int
          f = hh - fromIntegral (floor hh :: Int)
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
