{-# LANGUAGE TemplateHaskell #-}
module Eventlog.Javascript
  (
    vegaLite
  , vegaEmbed
  , vega
  , stylesheet
  , tablogic
  , milligram
  , normalizecss
  , treevega
  ) where

import Data.Text
import Data.Text.Encoding
import Data.FileEmbed
import Eventlog.VegaVersions

vegaLite :: Text
vegaLite = decodeUtf8 $(embedFile ("javascript/vega-lite@" ++ vegaLiteVersion))

vegaEmbed :: Text
vegaEmbed = decodeUtf8 $(embedFile ("javascript/vega-embed@" ++ vegaEmbedVersion))

vega :: Text
vega = decodeUtf8 $(embedFile ("javascript/vega@" ++ vegaVersion))

stylesheet :: Text
stylesheet = decodeUtf8 $(embedFile "javascript/stylesheet.css")

tablogic :: Text
tablogic = decodeUtf8 $(embedFile "javascript/tablogic.js")

milligram :: Text
milligram = decodeUtf8 $(embedFile "javascript/milligram.min.css")

normalizecss :: Text
normalizecss = decodeUtf8 $(embedFile "javascript/normalize.min.css")

treevega :: Text
treevega = decodeUtf8 $(embedFile "javascript/ccmap.vg")

