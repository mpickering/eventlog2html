{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplate where

import Data.Text (Text, append)
import qualified Data.Text as T
--import Text.Blaze.Html
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Javascript
import Args

encloseScript :: Text -> Html
encloseScript vegaspec = preEscapedToHtml $ T.unlines [
  "var yourVlSpec = " `append` vegaspec  `append` ";"
  , "vegaEmbed('#vis', yourVlSpec)" ]


template :: Args -> Text -> Html
template as vegaSpec = docTypeHtml $ do
  H.head $ do
    H.title "Heap Profile"
    meta ! charset "UTF-8"
    if includejs as
      then do
        script $ preEscapedToHtml vegaLite
        script $ preEscapedToHtml vega
        script $ preEscapedToHtml vegaEmbed
      else do
        script ! src "https://cdn.jsdelivr.net/npm/vega@5.4.0" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/vega-lite@3.3.0" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/vega-embed@4.2.0" $ ""

  body $ do
    h1 $ "Heap Profile"
    H.div ! A.id "vis" $ ""
    script ! type_ "text/javascript" $ do
      (encloseScript vegaSpec)
