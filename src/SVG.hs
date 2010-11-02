{-# LANGUAGE OverloadedStrings #-}
module SVG (svg) where

import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as BS
import Numeric (showHex, showFFloat)

import Graphics (Graphics(Graphics), Point, Size, Angle, FontSize, Anchor(..), StrokeWidth, Opacity, RGB(..))
import qualified Graphics as G

svg :: Graphics
svg =
  Graphics
  { G.text     = text
  , G.rect     = rect
  , G.line     = line
  , G.polygon  = polygon
  , G.visual   = visual
  , G.document = document
  }

text :: Maybe Angle -> Anchor -> FontSize -> Point -> [ByteString] -> [ByteString]
text r a s (x,y) inner =
  let coords = case r of
        Nothing -> ["x='", showF x, "' y='", showF y, "'"]
        Just ra -> ["transform='translate(" , showF x, "," , showF y, ") rotate(", showF ra, ")'"]
      showAnchor Start = "start"
      showAnchor Middle = "middle"
      showAnchor End = "end"
  in  ["<text "] ++ coords ++ [" font-size='", showF s, "' text-anchor='", showAnchor a, "'>"] ++ map escape inner ++ ["</text>\n"]

escape :: ByteString -> ByteString
escape = BS.concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar c   = BS.singleton c

rect :: Point -> Size -> [ByteString]
rect (x,y) (w,h) = ["<rect x='", showF x, "' y='", showF y, "' width='" , showF w , "' height='" , showF h , "' />\n"]

line :: Point -> Point -> [ByteString]
line (x1,y1) (x2,y2) = ["<line x1='" , showF x1, "' x2='", showF x2, "' y1='", showF y1, "' y2='", showF y2, "' />\n"]

visual :: Maybe RGB -> Maybe Opacity -> Maybe RGB -> Maybe StrokeWidth -> [ByteString] -> [ByteString]
visual mfill mfillo mstroke mstrokew inner =
  let fill = maybe [] (\f -> [" fill='", showRGB f, "'"]) mfill
      fillo = maybe [] (\o -> [" fill-opacity='", showF o, "'"]) mfillo
      stroke = maybe [] (\s -> [" stroke='", showRGB s, "'"]) mstroke
      strokew = maybe [] (\w -> [" stroke-width='", showF w, "'"]) mstrokew
  in ["<g"] ++ fill ++ fillo ++ stroke ++ strokew ++ [">\n"] ++ inner ++ ["</g>\n"]

document :: Size -> [ByteString] -> [ByteString]
document (w,h) inner =
  [ "<?xml version='1.0' encoding='UTF-8' ?>\n"
  , "<svg xmlns='http://www.w3.org/2000/svg' version='1.0'"
  , " width='", showF w, "' height='", showF h, "'>\n"
  ] ++ inner ++ ["</svg>\n"]

polygon :: [Point] -> [ByteString]
polygon ps = ["<path d='"] ++ path ps ++ ["' />"]

path :: [(Double,Double)] -> [ByteString]
path [] = error "SVG.path: empty"
path (p0:ps) =
  let lineTo p = [ " L " ] ++ showP p
  in  [ "M " ] ++ showP p0 ++ concatMap lineTo (ps ++ [p0]) ++ [ " Z" ]

showRGB :: RGB -> ByteString
showRGB (RGB r g b) =
  let toInt x = let y = floor (256 * x) in 0 `max` y `min` 255 :: Int
      hex2 i = (if i < 16 then ('0':) else id) (showHex i "")
  in  pack $ '#' : concatMap (hex2 . toInt) [r,g,b]

showP :: Point -> [ByteString]
showP (x,y) = [showF x, ",", showF y]

showF :: Double -> ByteString
showF x = pack $ showFFloat Nothing x ""
