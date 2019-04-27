{-# LANGUAGE OverloadedStrings #-}
module SVG (svg, fillStyleName) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid ((<>), mconcat)
import Numeric (showHex)
import Text.FShow.RealFloat (fshow, Double7(D7))

import Graphics (Graphics(Graphics), Point, Size, Angle, FontSize, Anchor(..), StrokeWidth, Opacity, RGB(..), PatternID, colour, patternIx)
import qualified Graphics as G
import Pattern (patterns)
import Graphics.Svg

svg :: Graphics
svg =
  Graphics
  { G.text     = text
  , G.rect     = rect
  , G.line     = line
  , G.polygon  = polygon
  , G.pattern  = pattern
  , G.visual   = visual
  , G.document = document
  }

text :: Maybe Angle -> Anchor -> FontSize -> Point -> [Text] -> Element
text r a s (x,y) inner =
  let coords = case r of
        Nothing -> [X_ <<- showF x, Y_ <<- showF y]
        Just ra -> [Transform_ <<- translate x y <> rotate ra]
      showAnchor Start = "start"
      showAnchor Middle = "middle"
      showAnchor End = "end"
  in  text_ ([Stroke_ <<- "none", Font_size_ <<- showF s, Text_anchor_ <<- showAnchor a] ++ coords )
            (toElement (T.unlines (map escape inner)))

escape :: Text -> Text
escape = T.concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar c   = T.singleton c

rect :: Point -> Size -> Element
rect (x,y) (w,h) =
  rect_ [X_ <<- showF x, Y_ <<- showF y, Width_ <<- showF w, Height_ <<- showF h ]

line :: Point -> Point -> Element
line (x1,y1) (x2,y2) =
  line_ [X1_ <<- showF x1, X2_ <<- showF x2, Y1_ <<- showF y1, Y2_ <<- showF y2]

pattern :: Text -> (PatternID, Element)
pattern t = ("url(#" <> pid <> ")",
  pattern_ [Id_ <<- pid, X_ <<- "0", Y_ <<- "0", Width_ <<- "16", Height_ <<- "16"
           , PatternUnits_ <<- "userSpaceOnUse"]
           (path_ [ Fill_ <<- "none", Stroke_width_ <<- "2", makeAttribute "line-cap" "round"
                 , D_ <<- foldMap p ps ]))

  where
    pid = mconcat ["P", pack (show ix), "p", T.tail s]
    s = showRGB rgb
    p ((x0, y0), (x1, y1)) = mA x0 y0 <> lA x1 y1
    rgb = colour t
    ix = patternIx t
    ps = patterns !! ix

visual :: Maybe (Either PatternID RGB) -> Maybe Opacity -> Maybe RGB -> Maybe StrokeWidth -> Element -> Element
visual mfill mfillo mstroke mstrokew inner =
  let fill = maybe [] (\f -> [Fill_ <<- either id showRGB f]) mfill
      fillo = maybe [] (\o -> [Fill_opacity_ <<- showF o]) mfillo
      stroke = maybe [] (\s -> [Stroke_ <<- showRGB s]) mstroke
      strokew = maybe [] (\w -> [Stroke_width_ <<- showF w]) mstrokew
  in g_ (fill ++ fillo ++ stroke ++ strokew) inner

document :: Size -> Element -> Element -> Element
document (w,h) defs inner =
    doctype
  <> with (svg11_ (defs_ [] defs <> inner)) [Version_ <<- "1.0", Width_ <<- showF w, Height_ <<- showF h]

{-
  [ "<?xml version='1.0' encoding='UTF-8' ?>\n"
  , "<svg xmlns='http://www.w3.org/2000/svg' version='1.0'"

  , " width='", showF w, "' height='", showF h, "'>\n"
  ] ++ ["<defs>\n"] ++ defs ++ ["</defs>\n"] ++ inner ++ ["</svg>\n"]
  -}

polygon :: [Point] -> Element
polygon ps = path_ [ D_ <<- path ps ]

path :: [(Double,Double)] -> Text
path [] = error "SVG.path: empty"
path (p0@(x, y):ps) =
  let lineTo (x1, y1) = lA x1 y1
  in  mA x y <> foldMap lineTo (ps ++ [p0]) <> z

showRGB :: RGB -> Text
showRGB (RGB r g b) =
  let toInt x = let y = floor (256 * x) in 0 `max` y `min` 255 :: Int
      hex2 i = (if i < 16 then ('0':) else id) (showHex i "")
  in  pack $ '#' : concatMap (hex2 . toInt) [r,g,b]

showF :: Double -> Text
showF x = pack $ fshow (D7 x)

fillStyleName :: Either PatternID RGB -> Text
fillStyleName (Left c) = T.drop 5 (T.init c)
fillStyleName (Right c) = T.singleton 'C' <> T.tail (showRGB c)
