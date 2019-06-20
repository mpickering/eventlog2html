{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplate where

import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as TL
--import Text.Blaze.Html
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Javascript
import Args
import Data.Aeson (Value, encode)

encloseScript :: (Value, Value) -> Text -> Html
encloseScript (dh, dt) vegaspec = preEscapedToHtml $ T.unlines [
  "var yourVlSpec = " `append` vegaspec  `append` ";"
  , "data_heap = " `append` dht `append` ";"
  , "data_traces =" `append` dtt `append` ";"
  , "console.log(data_traces);"
  , "console.log(data_heap);"
  , "vegaEmbed('#vis', yourVlSpec)"
  , ".then((res) => "
  , "res.view"
  , ".insert(\"heap\", data_heap)"
  , ".insert(\"traces\", data_traces)"
  , ".runAsync());" ]
  where
    dht = TL.toStrict (T.decodeUtf8 (encode dh))
    dtt = TL.toStrict (T.decodeUtf8 (encode dt))

template :: (Value, Value) -> Args -> Text -> Html
template dat as vegaSpec = docTypeHtml $ do
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
    renderChart dat vegaSpec


renderChart :: (Value, Value) -> Text -> Html
renderChart dat vegaSpec = do
    H.div ! A.id "vis" $ ""
    script ! type_ "text/javascript" $ do
      (encloseScript dat vegaSpec)

templateString :: (Value, Value) -> Args -> Text -> String
templateString dat as vegaSpec = renderHtml $ template dat as vegaSpec

