{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplate where

import Data.String
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

type VizID = Int

insertJsonData :: (Value, Value) -> Html
insertJsonData (dh, dt) = preEscapedToHtml $ T.unlines [
    "data_heap = " `append` dht `append` ";"
  , "data_traces =" `append` dtt `append` ";"
  , "console.log(data_traces);"
  , "console.log(data_heap);" ]
  where
    dht = TL.toStrict (T.decodeUtf8 (encode dh))
    dtt = TL.toStrict (T.decodeUtf8 (encode dt))


encloseScript :: VizID -> (Value, Value) -> Text -> Html
encloseScript vid (dh, dt) vegaspec = preEscapedToHtml $ T.unlines [
  "var yourVlSpec" `append` vidt `append`"= " `append` vegaspec  `append` ";"
  , "vegaEmbed('#vis" `append` vidt `append` "', yourVlSpec" `append` vidt `append` ")"
  , ".then((res) => "
  , "res.view"
  , ".insert(\"heap\", data_heap)"
  , ".insert(\"traces\", data_traces)"
  , ".runAsync());" ]
  where
    vidt = T.pack $ show vid
  
htmlHeader :: (Value, Value) -> Args -> Html
htmlHeader dat as =
    H.head $ do
    H.title "Heap Profile"
    meta ! charset "UTF-8"
    H.style $ preEscapedToHtml stylesheet
    script $ insertJsonData dat
    if includejs as
      then do
        script $ preEscapedToHtml vegaLite
        script $ preEscapedToHtml vega
        script $ preEscapedToHtml vegaEmbed
      else do
        script ! src "https://cdn.jsdelivr.net/npm/vega@5.4.0" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/vega-lite@3.3.0" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/vega-embed@4.2.0" $ ""

template :: (Value, Value) -> Args -> Text -> Html
template dat as vegaSpec = docTypeHtml $ do
  htmlHeader dat as
  body $ do
    H.div ! A.id "areachart" ! class_ "tabcontent" $ do
      h1 $ "eventlog2html"
      p $ "Area Chart"
      
    H.div ! A.id "normalizedchart" ! class_ "tabcontent" $ do
      h1 $ "eventlog2html"
      p $ "Normalized"
      
    H.div ! A.id "streamgraph" ! class_ "tabcontent" $ do
      h1 $ "eventlog2html"
      p $ "Streamgraph"
      
    H.div ! A.id "linechart" ! class_ "tabcontent" $ do
      h1 $ "eventlog2html"
      p $ "Linechart"

    button ! class_ "tablink" ! onclick "changeTab('areachart','areachart-viz', this)" ! A.id "defaultOpen" $ "Area Chart"
    button ! class_ "tablink" ! onclick "changeTab('normalizedchart','normalizedchart-viz', this)" $ "Normalized"
    button ! class_ "tablink" ! onclick "changeTab('streamgraph', 'streamgraph-viz',this)" $ "Streamgraph"
    button ! class_ "tablink" ! onclick "changeTab('linechart', 'linechart-viz', this)" $ "Linechart"

    H.div ! A.id "areachart-viz" ! class_ "tabviz" $ do
      p $ "Area chart"
      renderChart 1 dat vegaSpec
    H.div ! A.id "normalizedchart-viz" ! class_ "tabviz" $ do
      p $ "Normalized chart"
      renderChart 2 dat vegaSpec
    H.div ! A.id "streamgraph-viz" ! class_ "tabviz" $ do
      p $ "Streamgraph"
      renderChart 3 dat vegaSpec
    H.div ! A.id "linechart-viz" ! class_ "tabviz" $ do
      p $ "linechart"
      renderChart 4 dat vegaSpec

    script $ preEscapedToHtml tablogic

      
renderChart :: VizID -> (Value, Value) -> Text -> Html
renderChart vid dat vegaSpec = do
    H.div ! A.id (fromString $ "vis" ++ show vid) $ ""
    script ! type_ "text/javascript" $ do
      (encloseScript vid dat vegaSpec)

templateString :: (Value, Value) -> Args -> Text -> String
templateString dat as vegaSpec = renderHtml $ template dat as vegaSpec

