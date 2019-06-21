{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplate where

import Data.Aeson (Value, encode)
import Data.Aeson.Text (encodeToLazyText)
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
import VegaTemplate

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


encloseScript :: VizID -> Text -> Html
encloseScript vid vegaspec = preEscapedToHtml $ T.unlines [
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
        link ! rel "stylesheet" ! href "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
        link ! rel "stylesheet" ! href "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css"
        link ! rel "stylesheet" ! href "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css"

template :: (Value, Value) -> Args -> Html
template dat as = docTypeHtml $ do
  htmlHeader dat as
  body $ H.div ! class_ "container" $ do
    H.div ! class_ "tabcontent" $ do
      h1 $ "eventlog2html"

    button ! class_ "tablink button-black" ! onclick "changeTab('areachart', this)" ! A.id "defaultOpen" $ "Area Chart"
    button ! class_ "tablink button-black" ! onclick "changeTab('normalizedchart', this)" $ "Normalized"
    button ! class_ "tablink button-black" ! onclick "changeTab('streamgraph', this)" $ "Streamgraph"
    button ! class_ "tablink button-black" ! onclick "changeTab('linechart', this)" $ "Linechart"

    mapM_ (\(vid, name, conf) ->
             H.div ! A.id name ! class_ "tabviz" $ do
               renderChart vid
                (TL.toStrict (encodeToLazyText (vegaJson (htmlConf conf)))))
      [(1, "areachart",  AreaChart Stacked)
      ,(2, "normalizedchart", AreaChart Normalized)
      ,(3, "streamgraph", AreaChart StreamGraph)
      ,(4, "linechart", LineChart)]

    script $ preEscapedToHtml tablogic

htmlConf :: ChartType -> ChartConfig
htmlConf = ChartConfig 1200 1000 True

renderChart :: VizID -> Text -> Html
renderChart vid vegaSpec = do
    H.div ! A.id (fromString $ "vis" ++ show vid) ! class_ "chart" $ ""
    script ! type_ "text/javascript" $ do
      (encloseScript vid vegaSpec)

renderChartWithJson :: Int -> (Value, Value) -> Text -> Html
renderChartWithJson k dat vegaSpec = do
    script $ insertJsonData dat
    renderChart k vegaSpec


templateString :: (Value, Value) -> Args -> String
templateString dat as =
  renderHtml $ template dat as
