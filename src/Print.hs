module Print where

import Data.Array.Unboxed (bounds, (!))
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Numeric (showHex, showFFloat)

import Types

print :: Graph -> ByteString
print hpg =
  let fwd = hpgSamples hpg
      rwd = reverse fwd --)) (hpgBands hpg)
      ((b0,s0),(b1,s1)) = bounds (hpgBands hpg)
      bands =
        [ (fwd ++ rwd) `zip` (bfwd ++ brwd)
        | b <- [b0 + 1 .. b1]
        , let bfwd = [ hpgBands hpg ! (b - 1, s) | s <- [s0 .. s1] ]
        , let brwd = [ hpgBands hpg ! (b, s) | s <- [s1, s1 - 1 .. s0] ]
        ] -- zipWith (\s t -> s ++ reverse t) paths (tail paths)
      polygons = zipWith polygon colours . map (map p) . reverse $ bands
      key = zipWith3 (keyBox (gW + border * 2.5) (border * 1.5) (gH / 16)) [0..] colours . reverse . map unpack . hpgLabels $ hpg
      w = 1280
      h = 720
      gW = 960 - 2 * border
      gH = 720 - 3 * border
      border = 60
      textOffset = 10
      (xMin, xMax) = hpgSampleRange hpg
      (yMin, yMax) = hpgValueRange hpg
      gRange@((gx0,gy0),(gx1,gy1)) = ((border*1.5, gH + border*1.5), (gW + border*1.5, border*1.5))
      p = rescalePoint ((xMin, yMin), (xMax, yMax)) gRange
      title = "<text font-size='25' text-anchor='middle' x='" ++ showF (fromIntegral w / 2) ++ "' y='" ++ showF (border * 0.75) ++ "'>" ++ unpack (hpgJob hpg) ++ " (" ++ unpack (hpgDate hpg) ++ ")</text>"
      background = "<rect fill='white' x='0' y='0' width='" ++ show w ++ "' height='" ++ show h ++ "' />"
      box = "<rect fill='white' x='" ++ showF gx0 ++ "' y='" ++ showF gy1 ++ "' width='" ++ showF gW ++ "' height='" ++ showF gH ++ "' />"
      gStart = "<g fill-opacity='0.5' fill='black' stroke='black' stroke-width='1'>"
      leftLabel   = "<text font-size='20' text-anchor='middle' transform='translate(" ++ showF (border/2) ++ "," ++ showF ((gy0 + gy1)/2) ++ ") rotate(-90)'>" ++ unpack (hpgValueUnit hpg) ++ "</text>"
      leftTicks   = map (\(y,l) -> let { (x1, y1) = p (xMin, y) ; (x2, y2) = p (xMax, y) } in
          "<line x1='" ++ showF (x1 - border/2) ++ "' x2='" ++ showF x2 ++ "' y1='" ++ showF y1 ++ "' y2='" ++ showF y2 ++ "' />" ++
          if l then "" else "<text font-size='15' text-anchor='end'   x='" ++ showF (x1 - textOffset) ++ "' y='" ++ showF (y1 - textOffset) ++ "'>" ++ showSI y ++ "</text>"
        ) (zip (hpgValueTicks hpg) (replicate (length (hpgValueTicks hpg) - 1) False ++ [True]))
      bottomLabel = "<text font-size='20' text-anchor='middle' x='" ++ showF ((gx0 + gx1)/2) ++ "' y='" ++ showF (gy0 + border) ++ "'>" ++ unpack (hpgSampleUnit hpg) ++ "</text>"
      bottomTicks = map (\(x,l) -> let { (x1, y1) = p (x, yMin) ; (x2, y2) = p (x, yMax) } in
          "<line y1='" ++ showF (y1 + border/2) ++ "' y2='" ++ showF y2 ++ "' x1='" ++ showF x1 ++ "' x2='" ++ showF x2 ++ "' />" ++
          if l then "" else "<text font-size='15' text-anchor='start' x='" ++ showF (x1 + textOffset) ++ "' y='" ++ showF (y1+2*textOffset) ++ "'>" ++ showSI x ++ "</text>"
        ) (zip (hpgSampleTicks hpg) (replicate (length (hpgSampleTicks hpg) - 1) False ++ [True]))
      gEnd = "</g>"
  in  pack $ unlines [ xmldecl, svgStart w h, background, gStart, title, leftLabel, unlines leftTicks, bottomLabel, unlines bottomTicks, box, unlines polygons, unlines key, gEnd, svgEnd ]

showSI :: Double -> String
showSI x | x < 1e3   = showF x
         | x < 1e6   = showF (x/1e3 ) ++ "k"
         | x < 1e9   = showF (x/1e6 ) ++ "M"
         | x < 1e12  = showF (x/1e9 ) ++ "G"
         | x < 1e15  = showF (x/1e12) ++ "T"
         | otherwise = showF (x/1e15) ++ "P"

showF :: Double -> String
showF x = showFFloat Nothing x ""

keyBox :: Double -> Double -> Double -> Int -> String -> String -> String
keyBox x y0 dy i c l =
  let y = y0 + fromIntegral i * dy
      box = "<rect fill-opacity='0.7' fill='" ++ c ++ "' x='" ++ showF x ++ "' y='" ++ showF (y + 0.1 * dy) ++ "' width='" ++ show (dy * 0.8) ++ "' height='" ++ show (dy * 0.8) ++ "' />"
      txt = "<text font-size='15' text-anchor='start' x='" ++ showF (x + dy) ++ "' y='" ++ showF (y + dy * 0.6) ++ "'>" ++ l ++ "</text>"
  in  box ++ txt


polygon :: String -> [(Double,Double)] -> String
polygon c ps = "<path fill-opacity='0.7' fill='" ++ c ++ "' d='" ++ path ps ++ "' />"

path :: [(Double,Double)] -> String
path [] = error "Print.path: []"
path (p0:ps) =
  let lineTo p = " L " ++ toSVGPoint p
  in  "M " ++ toSVGPoint p0 ++ concatMap lineTo (ps ++ [p0]) ++ " Z"

rescalePoint :: ((Double,Double),(Double,Double)) -> ((Double,Double),(Double,Double)) -> (Double, Double) -> (Double,Double)
rescalePoint ((inX0,inY0),(inX1,inY1)) ((outX0,outY0),(outX1,outY1)) (x,y) =
  let inW = inX1 - inX0
      inH = inY1 - inY0
      outW = outX1 - outX0
      outH = outY1 - outY0
  in  ((x - inX0) / inW * outW + outX0, (y - inY0) / inH * outH + outY0)

toSVGPoint :: (Double,Double) -> String
toSVGPoint (x,y) = showF x ++ "," ++ showF y

xmldecl :: String
xmldecl = "<?xml version='1.0' encoding='UTF-8' ?>"

svgStart :: Int -> Int -> String
svgStart w h = "<svg xmlns='http://www.w3.org/2000/svg' version='1.0' width='" ++ show w ++ "' height='" ++ show h ++ "'>"

svgEnd :: String
svgEnd = "</svg>"

phi :: Double
phi = (sqrt 5 + 1) / 2

hues :: [Double]
hues = [0, 2 * pi / (phi * phi) ..]

sats :: [Double]
sats = repeat 1

vals :: [Double]
vals = repeat 1

wrap :: Double -> Double
wrap x = x - fromIntegral (floor x :: Int)

colours :: [String]
colours = map toSVGColour $ zipWith3 toRGB hues sats vals

toRGB :: Double -> Double -> Double -> (Double, Double, Double)
toRGB h s v =
  let hh = h * 3 / pi
      i = floor hh `mod` 6 :: Int
      f = hh - fromIntegral (floor hh :: Int)
      p = v * (1 - s)
      q = v * (1 - s * f)
      t = v * (1 - s * (1 - f))
  in  case i of
        0 -> (v,t,p)
        1 -> (q,v,p)
        2 -> (p,v,t)
        3 -> (p,q,v)
        4 -> (t,p,v)
        _ -> (v,p,q)

toSVGColour :: (Double, Double, Double) -> String
toSVGColour (r,g,b) =
  let toInt x = let y = floor (256 * x) in 0 `max` y `min` 255 :: Int
      hex2 i = (if i < 16 then ('0':) else id) (showHex i "")
  in  '#' : concatMap (hex2 . toInt) [r,g,b]
