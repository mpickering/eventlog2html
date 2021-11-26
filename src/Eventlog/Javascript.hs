{-# LANGUAGE TemplateHaskell #-}
module Eventlog.Javascript
  (
    vegaLite
  , vegaEmbed
  , vega
  , jquery
  , bootstrap
  , bootstrapCSS
  , fancytable
  , sparkline

  , datatables
  , datatablesHtml5
  , datatablesButtons
  , datatablesEllipsis
  , datatablesCSS
  , datatablesButtonsCSS
  , imagesCSS

  , stylesheet
  , tablogic
  , treevega
  ) where

import Data.Text
import Data.Text.Encoding
import Data.FileEmbed
import Eventlog.AssetVersions

vegaLite :: Text
vegaLite = decodeUtf8 $(embedFile ("javascript/generated/vega-lite@" ++ vegaLiteVersion))

vegaEmbed :: Text
vegaEmbed = decodeUtf8 $(embedFile ("javascript/generated/vega-embed@" ++ vegaEmbedVersion))

vega :: Text
vega = decodeUtf8 $(embedFile ("javascript/generated/vega@" ++ vegaVersion))

jquery :: Text
jquery = decodeUtf8 $(embedFile ("javascript/generated/jquery-" ++ jqueryVersion ++ ".min.js"))

datatables :: Text
datatables = decodeUtf8 $(embedFile ("javascript/generated/jquery.dataTables.min.js"))

datatablesButtons :: Text
datatablesButtons = decodeUtf8 $(embedFile ("javascript/generated/dataTables.buttons.min.js"))

datatablesHtml5 :: Text
datatablesHtml5 = decodeUtf8 $(embedFile ("javascript/generated/buttons.html5.min.js"))

datatablesCSS :: Text
datatablesCSS =  decodeUtf8 $(embedFile ("javascript/generated/jquery.dataTables.min.css"))

datatablesButtonsCSS :: Text
datatablesButtonsCSS =  decodeUtf8 $(embedFile ("javascript/generated/buttons.dataTables.min.css"))

datatablesEllipsis :: Text
datatablesEllipsis =  decodeUtf8 $(embedFile ("javascript/ellipsis.js"))

imagesCSS :: Text
imagesCSS =  decodeUtf8 $(embedFile ("javascript/generated/overide_images.css"))

bootstrap :: Text
bootstrap = decodeUtf8 $(embedFile ("javascript/generated/bootstrap.min.js"))

bootstrapCSS :: Text
bootstrapCSS = decodeUtf8 $(embedFile ("javascript/generated/bootstrap.min.css"))

fancytable :: Text
fancytable = decodeUtf8 $(embedFile ("javascript/generated/fancyTable.min.js"))

sparkline :: Text
sparkline = decodeUtf8 $(embedFile ("javascript/generated/jquery.sparkline.min.js"))

stylesheet :: Text
stylesheet = decodeUtf8 $(embedFile "javascript/stylesheet.css")

tablogic :: Text
tablogic = decodeUtf8 $(embedFile "javascript/tablogic.js")

treevega :: Text
treevega = decodeUtf8 $(embedFile "javascript/ccmap.vg")
