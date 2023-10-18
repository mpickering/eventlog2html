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
import Eventlog.Ticky (tickyTab)
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

htmlHeader :: Maybe HeapProfileData -> Args -> Html
htmlHeader mb_hpd as =
    H.head $ do
    H.title "eventlog2html - Heap Profile"
    meta ! charset "UTF-8"
    forM_ mb_hpd $ \ (HeapProfileData dat desc _) -> do
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
        script $ preEscapedToHtml tablogic
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


template :: Header -> Maybe HeapProfileData -> Args -> [Tab VizTab] -> Html
template header' x as tabs = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader x as
  body $ H.div ! class_ "container-fluid" $ do
    H.div ! class_ "row" $ navbar indexed_tabs
    H.div ! class_ "row" $ do
      H.div ! class_ "col tab-content" $ do
        forM_ indexed_tabs $ \(n, tab) -> do
          let status = if n == 1 then "show active" else mempty
          H.div ! A.id (toValue (tabId tab)) ! class_ ("tab-pane fade tabviz " <> status) $ H.div ! class_ "row" $ do
            H.div ! class_ "col" $ vizIdToHtml (tabContent tab) n
            forM_ (tabDocs . tabContent $ tab) $ \docs -> H.div ! class_ "col" $ docs

    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        toHtml $ maybe "No heap profile" ppHeapProfileType (hHeapProfileType header')
        ", created at "
        code $ toHtml $ hDate header'

    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
        code $ toHtml $ hJob header'

    script $ preEscapedToHtml tablogic

  where
    indexed_tabs :: [(Int, Tab VizTab)]
    indexed_tabs = zip [1..] tabs



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


templateString :: Header -> Maybe HeapProfileData -> Maybe TickyProfileData -> Args -> String
templateString h x y as =
  renderHtml $ template h x as $ allTabs h x y as


ppHeapProfileType :: HeapProfBreakdown -> Text
ppHeapProfileType (HeapProfBreakdownCostCentre) = "Cost centre profiling (implied by -hc)"
ppHeapProfileType (HeapProfBreakdownModule) = "Profiling by module (implied by -hm)"
ppHeapProfileType (HeapProfBreakdownClosureDescr) = "Profiling by closure description (implied by -hd)"
ppHeapProfileType (HeapProfBreakdownTypeDescr) = "Profiling by type (implied by -hy)"
ppHeapProfileType (HeapProfBreakdownRetainer) = "Retainer profiling (implied by -hr)"
ppHeapProfileType (HeapProfBreakdownBiography) = "Biographical profiling (implied by -hb)"
ppHeapProfileType (HeapProfBreakdownClosureType) = "Basic heap profile (implied by -hT)"
ppHeapProfileType (HeapProfBreakdownInfoTable) = "Info table profile (implied by -hi)"


allTabs :: Header -> Maybe HeapProfileData -> Maybe TickyProfileData -> Args -> [Tab VizTab]
allTabs h x y as =
    [metaTab h as] ++
    maybe [] (allHeapTabs h as) x ++
    maybe [] tickyProfileTabs y

metaTab :: Header -> Args -> Tab VizTab
metaTab header' _as =
    Tab "Meta" "meta" $ VizTab (const metadata) Nothing
  where
    metadata = do
      "Rendered by "
      a ! href "https://mpickering.github.io/eventlog2html" $ "eventlog2html " <> toHtml (showVersion version)
      when (has_heap_profile header') $
        H.div ! class_ "row" $ do
          H.div ! class_ "col" $ do
            "Sampling rate: "
            code $ toHtml $ hSamplingRate header'
            " seconds between heap samples"

has_heap_profile :: Header -> Bool
has_heap_profile h = isJust (hHeapProfileType h)

allHeapTabs :: Header -> Args -> HeapProfileData -> [Tab VizTab]
allHeapTabs header' as x =
    heapTab as ++
    heapProfileTabs header' as x ++
    costCentresTab as x ++
    detailedTab x

heapTab :: Args -> [Tab VizTab]
heapTab as = [ Tab "Heap" "heapchart" $ VizTab (mk as HeapChart) (Just heapDocs)]

heapDocs :: Html
heapDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/heap.html")

heapProfileTabs :: Header -> Args -> HeapProfileData -> [Tab VizTab]
heapProfileTabs header' as _
  | has_heap_profile header' =
    [ Tab "Area Chart" "areachart"       $ VizTab (mk as (AreaChart Stacked))     noDocs
    , Tab "Normalized" "normalizedchart" $ VizTab (mk as (AreaChart Normalized))  noDocs
    , Tab "Streamgraph" "streamgraph"    $ VizTab (mk as (AreaChart StreamGraph)) noDocs
    , Tab "Linechart" "linechart"        $ VizTab (mk as LineChart)               noDocs
    ]
  | otherwise = []

mk :: Args -> ChartType -> VizID -> Html
mk as conf vid = renderChart itd conf True vid
                      (TL.toStrict (encodeToLazyText (vegaJson (htmlConf as conf))))
  where
    itd = if noTraces as then NoTraceData else TraceData

detailedTab :: HeapProfileData -> [Tab VizTab]
detailedTab (HeapProfileData _dat _cc_descs closure_descs) =
    [ Tab "Detailed" "closures" (VizTab (const v) noDocs) | Just v <- [closure_descs] ]

costCentresTab :: Args -> HeapProfileData -> [Tab VizTab]
costCentresTab as (HeapProfileData _dat cc_descs _) =
    [ Tab "Cost Centres" "cost-centres" (VizTab (\tabIx -> renderChart itd LineChart False tabIx treevega) noDocs) | isJust cc_descs ]
  where
    itd = if noTraces as then NoTraceData else TraceData

tickyProfileTabs :: TickyProfileData -> [Tab VizTab]
tickyProfileTabs y = [Tab "Ticky" "ticky" $ VizTab (const (tickyTab y)) Nothing]
