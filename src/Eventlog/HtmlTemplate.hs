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
import Eventlog.Types (Header(..), HeapProfBreakdown(..))
import Eventlog.VegaTemplate
import Eventlog.AssetVersions
import Paths_eventlog2html
import Data.Version
import Control.Monad
import Data.Maybe

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

-- Dynamically bound in ccs tree
insertColourScheme :: Text -> Html
insertColourScheme scheme = preEscapedToHtml $ T.unlines [
    "colour_scheme= \"" `append` scheme `append` "\";"
  , "console.log(colour_scheme);" ]


data_sets :: IncludeTraceData -> [Text]
data_sets itd = ["res.view.insert(\"data_json_samples\", data_json.samples)"]
    ++ ["; res.view.insert(\"data_json_traces\", data_json.traces)" | TraceData <- [itd] ]

data IncludeTraceData = TraceData | NoTraceData

encloseScript :: IncludeTraceData -> VizID -> Text -> Html
encloseScript = encloseScriptX True

encloseRawVegaScript :: IncludeTraceData -> VizID -> Text -> Html
encloseRawVegaScript = encloseScriptX False

encloseScriptX :: Bool -> IncludeTraceData -> VizID -> Text -> Html
encloseScriptX insert_data_sets itd vid vegaspec = preEscapedToHtml $ T.unlines ([
  "var yourVlSpec" `append` vidt `append`"= " `append` vegaspec  `append` ";"
  , "vegaEmbed('#vis" `append` vidt `append` "', yourVlSpec" `append` vidt `append` ")"
  , ".then((res) => { " ]
-- For the 4 vega lite charts we dynamically insert the data after the
-- chart is created to avoid duplicating it. For the vega chart, this
-- causes a harmless error so we just don't do it.
  ++ (if insert_data_sets then data_sets itd else []) ++
  [ "; res.view.resize()"
  , "; res.view.runAsync()"
  , "})" ])
  where
    vidt = T.pack $ show vid

jsScript :: String -> Html
jsScript url = script ! src (fromString $ url) $ ""
css :: AttributeValue -> Html
css url = link ! rel "stylesheet" ! href url

htmlHeader :: Value -> Maybe Value -> Args -> Html
htmlHeader dat desc as =
    H.head $ do
    H.title "eventlog2html - Heap Profile"
    meta ! charset "UTF-8"
    script $ insertJsonData dat
    maybe (return ()) (script . insertJsonDesc) desc
    script $ insertColourScheme (userColourScheme as)
    if not (noIncludejs as)
      then do
        script $ preEscapedToHtml vegaLite
        script $ preEscapedToHtml vega
        script $ preEscapedToHtml vegaEmbed
        script $ preEscapedToHtml jquery
        H.style  $ preEscapedToHtml bootstrapCSS
        script $ preEscapedToHtml bootstrap
        script $ preEscapedToHtml fancytable
        script $ preEscapedToHtml sparkline
      else do
        jsScript vegaURL
        jsScript vegaLiteURL
        jsScript vegaEmbedURL
        jsScript jqueryURL
        css (preEscapedStringValue bootstrapCSSURL)
        jsScript bootstrapURL
        css "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
        jsScript fancyTableURL
        jsScript sparklinesURL
    -- Include this last to overwrite some milligram styling
    H.style $ preEscapedToHtml stylesheet


template :: Header -> Value -> Maybe Value -> Maybe Html -> Args -> Html
template header' dat cc_descs closure_descs as = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader dat cc_descs as
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

    forM_ (hHeapProfileType header') $ \prof_type -> do
      H.div ! class_ "row" $ do
        H.div ! class_ "column" $ do
          "Type of profile: "
          code $ toHtml $ ppHeapProfileType prof_type

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
        when (isJust cc_descs) $ do
          button ! class_ "tablink button-black" ! onclick "changeTab('cost-centres', this)" $ "Cost Centres"
        when (isJust closure_descs) $ do
          button ! class_ "tablink button-black" ! onclick "changeTab('closures', this)" $ "Detailed"
    let itd = if noTraces as then NoTraceData else TraceData
    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        mapM_ (\(vid, chartname, conf) ->
                  H.div ! A.id chartname ! class_ "tabviz" $ do
                    renderChart itd True vid
                      (TL.toStrict (encodeToLazyText (vegaJson (htmlConf as conf)))))
          [(1, "areachart",  AreaChart Stacked)
          ,(2, "normalizedchart", AreaChart Normalized)
          ,(3, "streamgraph", AreaChart StreamGraph)
          ,(4, "linechart", LineChart)]

        when (isJust cc_descs) $ do
          H.div ! A.id "cost-centres" ! class_ "tabviz" $ do
            renderChart itd False 5 treevega
        forM_ closure_descs $ \v -> do
          H.div ! A.id "closures" ! class_ "tabviz" $ do
            v
    script $ preEscapedToHtml tablogic



htmlConf :: Args -> ChartType -> ChartConfig
htmlConf as ct = ChartConfig 1200 1000 (not (noTraces as)) (userColourScheme as) ct (fromIntegral <$> (fixedYAxis as))

renderChart :: IncludeTraceData -> Bool -> VizID -> Text -> Html
renderChart itd vega_lite vid vegaSpec = do
    H.div ! A.id (fromString $ "vis" ++ show vid) ! class_ "chart" $ ""
    script ! type_ "text/javascript" $ do
      if vega_lite
        then encloseScript itd vid vegaSpec
        else encloseRawVegaScript itd vid vegaSpec

renderChartWithJson :: IncludeTraceData -> Int -> Value -> Text -> Html
renderChartWithJson itd k dat vegaSpec = do
    script $ insertJsonData dat
    renderChart itd True k vegaSpec


templateString :: Header -> Value -> Maybe Value -> Maybe Html -> Args -> String
templateString header' dat cc_descs closure_descs as =
  renderHtml $ template header' dat cc_descs closure_descs as

ppHeapProfileType :: HeapProfBreakdown -> Text
ppHeapProfileType (HeapProfBreakdownCostCentre) = "Cost centre profiling (implied by -hc)"
ppHeapProfileType (HeapProfBreakdownModule) = "Profiling by module (implied by -hm)"
ppHeapProfileType (HeapProfBreakdownClosureDescr) = "Profiling by closure description (implied by -hd)"
ppHeapProfileType (HeapProfBreakdownTypeDescr) = "Profiling by type (implied by -hy)"
ppHeapProfileType (HeapProfBreakdownRetainer) = "Retainer profiling (implied by -hr)"
ppHeapProfileType (HeapProfBreakdownBiography) = "Biographical profiling (implied by -hb)"
ppHeapProfileType (HeapProfBreakdownClosureType) = "Basic heap profile (implied by -hT)"
ppHeapProfileType (HeapProfBreakdownInfoTable) = "Info table profile (implied by -hi)"
