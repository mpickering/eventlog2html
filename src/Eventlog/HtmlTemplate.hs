{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Eventlog.HtmlTemplate where

import Data.Aeson (Value, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.String
import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
--import Text.Blaze.Html
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

import Data.FileEmbed
import Eventlog.Data
import Eventlog.Javascript
import Eventlog.Args
import Eventlog.Types (Header(..), HeapProfBreakdown(..))
import Eventlog.Rendering.Bootstrap
import Eventlog.Rendering.Types
import Eventlog.VegaTemplate
import Eventlog.AssetVersions
import Paths_eventlog2html
import Data.Version
import Control.Monad
import Data.Maybe

insertJsonData :: Value -> Html
insertJsonData dat = preEscapedToHtml $ T.unlines [
    "data_json= " `append` dat' `append` ";"
  , "console.log(data_json);" ]
  where
    dat' = TL.toStrict (TL.decodeUtf8 (encode dat))

insertJsonDesc :: Value -> Html
insertJsonDesc dat = preEscapedToHtml $ T.unlines [
    "desc_json= " `append` dat' `append` ";"
  , "console.log(desc_json);" ]
  where
    dat' = TL.toStrict (TL.decodeUtf8 (encode dat))

-- Dynamically bound in ccs tree
insertColourScheme :: Text -> Html
insertColourScheme scheme = preEscapedToHtml $ T.unlines [
    "colour_scheme= \"" `append` scheme `append` "\";"
  , "console.log(colour_scheme);" ]


data_sets :: [Text] -> [Text]
data_sets itd = Prelude.map line itd
 where
  line t = "res.view.insert(\"data_json_" <> t <>"\", data_json."<> t <>");"

encloseScript :: [Text] -> VizID -> Text -> Html
encloseScript = encloseScriptX

encloseRawVegaScript :: VizID -> Text -> Html
encloseRawVegaScript = encloseScriptX []

encloseScriptX :: [Text] -> VizID -> Text -> Html
encloseScriptX insert_data_sets vid vegaspec = preEscapedToHtml $ T.unlines ([
  "var yourVlSpec" `append` vidt `append`"= " `append` vegaspec  `append` ";"
  , "vegaEmbed('#vis" `append` vidt `append` "', yourVlSpec" `append` vidt `append` ")"
  , ".then((res) => { " ]
-- For the 4 vega lite charts we dynamically insert the data after the
-- chart is created to avoid duplicating it. For the vega chart, this
-- causes a harmless error so we just don't do it.
  ++ (data_sets insert_data_sets) ++
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


template :: HeapProfileData -> Args -> Html
template (HeapProfileData header' dat cc_descs closure_descs) as = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader dat cc_descs as
  body $ H.div ! class_ "container-fluid" $ do
    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        navbar tabs
        --forM_ tabs $ \(n, tab) -> do
        --  button ! class_ "tablink button-black"
        --         ! onclick ("changeTab('" <> toValue (tabId tab) <> "', this)")
        --         !? (n == 1, A.id "defaultOpen")
        --         $ toHtml (tabName tab)

    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        forM_ tabs $ \(n, tab) ->
          H.div ! A.id (toValue (tabId tab)) ! class_ "tabviz" $ H.div ! class_ "row" $ do
            H.div ! class_ "col" $ vizIdToHtml (tabContent tab) n
            forM_ (tabDocs . tabContent $ tab) $ \docs -> H.div ! class_ "col" $ docs

    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        toHtml $ maybe "No heap profile" ppHeapProfileType (hHeapProfileType header')
        ", created at "
        code $ toHtml $ hDate header'
        ", rendered by "
        a ! href "https://mpickering.github.io/eventlog2html" $ "eventlog2html " <> toHtml (showVersion version)

    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        code $ toHtml $ hJob header'

    when has_heap_profile $
      H.div ! class_ "row" $ do
        H.div ! class_ "col" $ do
          "Sampling rate: "
          code $ toHtml $ hSamplingRate header'
          " seconds between heap samples"


    script $ preEscapedToHtml tablogic

  where
    has_heap_profile = isJust (hHeapProfileType header')

    tabs :: [(Int, Tab VizTab)]
    tabs = zip [1..] $
           [ Tab "Heap" "heapchart" $ VizTab (mk HeapChart) (Just heapDocs)] ++
           (if has_heap_profile
           then [ Tab "Area Chart" "areachart" $ VizTab (mk (AreaChart Stacked)) noDocs
                , Tab "Normalized" "normalizedchart" $ VizTab (mk (AreaChart Normalized)) noDocs
                , Tab "Streamgraph" "streamgraph" $ VizTab (mk (AreaChart StreamGraph)) noDocs
                , Tab "Linechart" "linechart" $ VizTab (mk LineChart) noDocs
                ]
           else []) ++
           [ Tab "Cost Centres" "cost-centres" (VizTab (const (renderChart itd LineChart False 6 treevega)) noDocs) | isJust cc_descs ] ++
           [ Tab "Detailed" "closures" (VizTab (const v) noDocs) | Just v <- [closure_descs] ]

    itd = if noTraces as then NoTraceData else TraceData

    mk conf vid = renderChart itd conf True vid
                      (TL.toStrict (encodeToLazyText (vegaJson (htmlConf as conf))))

heapDocs :: Html
heapDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/heap.html")


select_data :: IncludeTraceData -> ChartType -> [Text]
select_data itd c =
  case c of
    AreaChart {} -> prof_data
    LineChart {} -> prof_data
    HeapChart {} -> ["heap"] ++ ["traces" | TraceData <- [itd]]
  where
    prof_data =  ["samples"] ++ ["traces" | TraceData <- [itd]]



htmlConf :: Args -> ChartType -> ChartConfig
htmlConf as ct =
  ChartConfig
    { cwidth = 1200
    , cheight = 1000
    , traces = not (noTraces as)
    , colourScheme = userColourScheme as
    , lineColourScheme = "set1"
    , chartType = ct
    , fixedYAxisExtent = fromIntegral <$> fixedYAxis as
    }

renderChart :: IncludeTraceData -> ChartType -> Bool -> VizID -> Text -> Html
renderChart itd ct vega_lite vid vegaSpec = do
    let fields = select_data itd ct
    H.div ! A.id (fromString $ "vis" ++ show vid) ! class_ "chart" $ ""
    script ! type_ "text/javascript" $ do
      if vega_lite
        then encloseScript fields vid vegaSpec
        else encloseRawVegaScript vid vegaSpec

renderChartWithJson :: IncludeTraceData -> ChartType -> Int -> Value -> Text -> Html
renderChartWithJson itd ct k dat vegaSpec = do
    script $ insertJsonData dat
    renderChart itd ct True k vegaSpec


templateString :: HeapProfileData -> Args -> String
templateString x as =
  renderHtml $ template x as

ppHeapProfileType :: HeapProfBreakdown -> Text
ppHeapProfileType (HeapProfBreakdownCostCentre) = "Cost centre profiling (implied by -hc)"
ppHeapProfileType (HeapProfBreakdownModule) = "Profiling by module (implied by -hm)"
ppHeapProfileType (HeapProfBreakdownClosureDescr) = "Profiling by closure description (implied by -hd)"
ppHeapProfileType (HeapProfBreakdownTypeDescr) = "Profiling by type (implied by -hy)"
ppHeapProfileType (HeapProfBreakdownRetainer) = "Retainer profiling (implied by -hr)"
ppHeapProfileType (HeapProfBreakdownBiography) = "Biographical profiling (implied by -hb)"
ppHeapProfileType (HeapProfBreakdownClosureType) = "Basic heap profile (implied by -hT)"
ppHeapProfileType (HeapProfBreakdownInfoTable) = "Info table profile (implied by -hi)"
