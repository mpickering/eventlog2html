{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplate where

import Data.Text (Text, append)
--import Text.Blaze.Html
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Javascript

encloseScript :: Text -> Html
encloseScript vegaspec = preEscapedToHtml $
  "var yourVlSpec = " `append` vegaspec `append` ";\n vegaEmbed('#vis', yourVlSpec);"

template :: Html -> Html
template vegaSpec = docTypeHtml $ do
  H.head $ do
    H.title "Heap Profile"
    meta ! charset "UTF-8"
    script $ preEscapedToHtml vegaLite
    script $ preEscapedToHtml vega
    script $ preEscapedToHtml vegaEmbed
  body $ do
    H.div ! A.id "vis" $ ""
    script ! type_ "text/javascript" $ do
      vegaSpec
