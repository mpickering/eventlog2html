{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplate where

import Data.Text (Text, append)
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

encloseScript :: Text -> Html
encloseScript vegaspec = toHtml $
  "var yourVlSpec = " `append` vegaspec `append` ";\n vegaEmbed('#vis', yourVlSpec);"

template :: Html -> Html
template vegaSpec = docTypeHtml $ do
  H.head $ do
    H.title "Heap Profile"
    meta ! charset "UTF-8"
    script ! src "https://cdn.jsdelivr.net/npm/vega@5.4.0" $ ""
    script ! src "https://cdn.jsdelivr.net/npm/vega-lite@3.3.0" $ ""
    script ! src "https://cdn.jsdelivr.net/npm/vega-embed@4.2.0" $ ""
  body $ do
    H.div ! A.id "vis" $ ""
    script ! type_ "text/javascript" $ do
      vegaSpec


