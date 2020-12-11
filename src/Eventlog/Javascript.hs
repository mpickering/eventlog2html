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
