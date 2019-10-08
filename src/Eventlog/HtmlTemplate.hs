{-# LANGUAGE OverloadedStrings #-}
module Eventlog.HtmlTemplate where

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

import Eventlog.Javascript
import Eventlog.Args
import Eventlog.Types (Header(..))
import Eventlog.VegaTemplate
import Paths_eventlog2html
import Data.Version
import Control.Monad

type VizID = Int

insertJsonData :: Value -> Html
insertJsonData dat = preEscapedToHtml $ T.unlines [
    "data_json= " `append` dat' `append` ";"
  , "console.log(data_json);" ]
  where
    dat' = TL.toStrict (T.decodeUtf8 (encode dat))

insertJsonDesc :: Value -> Html
insertJsonDesc dat = preEscapedToHtml $ T.unlines [
    "desc_json= " `append` dat' `append` ";"
  , "console.log(desc_json);" ]
  where
    dat' = TL.toStrict (T.decodeUtf8 (encode dat))



encloseScript :: VizID -> Text -> Html
encloseScript vid vegaspec = preEscapedToHtml $ T.unlines [
  "var yourVlSpec" `append` vidt `append`"= " `append` vegaspec  `append` ";"
  , "vegaEmbed('#vis" `append` vidt `append` "', yourVlSpec" `append` vidt `append` ")"
  , ".then((res) => "
  , "res.view"
  , ".insert(\"data_json_samples\", data_json.samples)"
  , ".insert(\"data_json_traces\", data_json.traces)"
  , ".runAsync());" ]
  where
    vidt = T.pack $ show vid


htmlHeader :: Value -> Value -> Args -> Html
htmlHeader dat desc as =
    H.head $ do
    H.title "eventlog2html - Heap Profile"
    meta ! charset "UTF-8"
    script $ insertJsonData dat
    script $ insertJsonDesc desc
    if not (noIncludejs as)
      then do
        script $ preEscapedToHtml vegaLite
        script $ preEscapedToHtml vega
        script $ preEscapedToHtml vegaEmbed
        H.style  $ preEscapedToHtml milligram
        H.style  $ preEscapedToHtml normalizecss
      else do
        script ! src "https://cdn.jsdelivr.net/npm/vega@5.4.0" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/vega-lite@3.3.0" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/vega-embed@4.2.0" $ ""
        link ! rel "stylesheet" ! href "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
        link ! rel "stylesheet" ! href "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css"
        link ! rel "stylesheet" ! href "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css"
    -- Include this last to overwrite some milligram styling
    H.style $ preEscapedToHtml stylesheet


template :: Header -> Value -> Value -> Args -> Html
template header' dat descs as = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader dat descs as
  body $ H.div ! class_ "container" $ do
    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        h1 $ a ! href "https://mpickering.github.io/eventlog2html" $ "eventlog2html"

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Options: "
        code $ toHtml $ hJob header'

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Created at: "
        code $ toHtml $ hDate header'

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Type of profile: "
        code $ toHtml $ hHeapProfileType header'

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Sampling rate in seconds: "
        code $ toHtml $ hSamplingRate header'

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        button ! class_ "tablink button-black" ! onclick "changeTab('areachart', this)" ! A.id "defaultOpen" $ "Area Chart"
        button ! class_ "tablink button-black" ! onclick "changeTab('normalizedchart', this)" $ "Normalized"
        button ! class_ "tablink button-black" ! onclick "changeTab('streamgraph', this)" $ "Streamgraph"
        button ! class_ "tablink button-black" ! onclick "changeTab('linechart', this)" $ "Linechart"
        button ! class_ "tablink button-black" ! onclick "changeTab('cost-centres', this)" $ "Cost Centres"

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        mapM_ (\(vid, chartname, conf) ->
                  H.div ! A.id chartname ! class_ "tabviz" $ do
                    renderChart vid
                      (TL.toStrict (encodeToLazyText (vegaJson (htmlConf as conf)))))
          [(1, "areachart",  AreaChart Stacked)
          ,(2, "normalizedchart", AreaChart Normalized)
          ,(3, "streamgraph", AreaChart StreamGraph)
          ,(4, "linechart", LineChart)]

        H.div ! A.id "cost-centres" ! class_ "tabviz" $ do
          renderChart 5 treevega
    script $ preEscapedToHtml tablogic


htmlConf :: Args -> ChartType -> ChartConfig
htmlConf as = ChartConfig 1200 1000 (not (noTraces as)) (userColourScheme as)

renderChart :: VizID -> Text -> Html
renderChart vid vegaSpec = do
    H.div ! A.id (fromString $ "vis" ++ show vid) ! class_ "chart" $ ""
    script ! type_ "text/javascript" $ do
      (encloseScript vid vegaSpec)

renderChartWithJson :: Int -> Value -> Text -> Html
renderChartWithJson k dat vegaSpec = do
    script $ insertJsonData dat
    renderChart k vegaSpec


templateString :: Header -> Value -> Value -> Args -> String
templateString header' dat descs as =
  renderHtml $ template header' dat descs as
