{-# LANGUAGE TemplateHaskell #-}
module Javascript
  (
    vegaLite
  , vegaEmbed
  , vega
  , stylesheet
  , tablogic
  ) where

import Data.Text
import Data.Text.Encoding
import Data.FileEmbed

vegaLite :: Text
vegaLite = decodeUtf8 $(embedFile "javascript/vega-lite@3.3.0")

vegaEmbed :: Text
vegaEmbed = decodeUtf8 $(embedFile "javascript/vega-embed@4.2.0")

vega :: Text
vega = decodeUtf8 $(embedFile "javascript/vega@5.4.0")

stylesheet :: Text
stylesheet = decodeUtf8 $(embedFile "javascript/stylesheet.css")

tablogic :: Text
tablogic = decodeUtf8 $(embedFile "javascript/tablogic.js")
