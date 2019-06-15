{-# LANGUAGE TemplateHaskell #-}
module Javascript
  (
    vegaLite
  , vegaEmbed
  , vega
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
