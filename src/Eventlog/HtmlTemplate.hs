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

encloseScript :: [Text] -> TabID -> Text -> Html
encloseScript = encloseScriptX

encloseRawVegaScript :: TabID -> Text -> Html
encloseRawVegaScript = encloseScriptX []

encloseScriptX :: [Text] -> TabID -> Text -> Html
encloseScriptX insert_data_sets (TabID vidt) vegaspec = preEscapedToHtml $ T.unlines ([
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

jsScript :: String -> Html
jsScript url = script ! src (fromString $ url) $ ""
css :: AttributeValue -> Html
css url = link ! rel "stylesheet" ! href url

htmlHeader :: Maybe HeapProfileData -> Maybe TickyProfileData -> Args -> Html
htmlHeader mb_hpd mb_ticky as =
    H.head $ do
    H.title "eventlog2html - Heap Profile"
    meta ! charset "UTF-8"
    forM_ mb_hpd $ \ (HeapProfileData dat desc _) -> do
      script $ insertJsonData dat
      maybe (return ()) (script . insertJsonDesc) desc
    script $ insertColourScheme (userColourScheme as)
    if not (noIncludejs as)
      then do
        script $ preEscapedToHtml popper
        script $ preEscapedToHtml vegaLite
        script $ preEscapedToHtml vega
        script $ preEscapedToHtml vegaEmbed
        script $ preEscapedToHtml jquery
        H.style  $ preEscapedToHtml bootstrapCSS
        script $ preEscapedToHtml bootstrap
        script $ preEscapedToHtml fancytable
        script $ preEscapedToHtml sparkline
        when has_ticky $ do
          H.style  $ preEscapedToHtml datatablesCSS
          H.style  $ preEscapedToHtml datatablesButtonsCSS
          script $ preEscapedToHtml datatables
          script $ preEscapedToHtml datatablesButtons
          script $ preEscapedToHtml datatablesHtml5
          H.style $ preEscapedToHtml imagesCSS
      else do
        jsScript popperURL
        jsScript vegaURL
        jsScript vegaLiteURL
        jsScript vegaEmbedURL
        jsScript jqueryURL
        css (preEscapedStringValue bootstrapCSSURL)
        jsScript bootstrapURL
        jsScript fancyTableURL
        jsScript sparklinesURL
        when has_ticky $ do
          css (preEscapedStringValue datatablesCSSURL)
          css (preEscapedStringValue datatablesButtonsCSSURL)
          jsScript datatablesURL
          jsScript datatablesButtonsURL
          jsScript datatablesButtonsHTML5URL
    when has_ticky $
      script $ preEscapedToHtml datatablesEllipsis
    -- Include this last to overwrite some milligram styling
    H.style $ preEscapedToHtml stylesheet
  where
    has_ticky = isJust mb_ticky

template :: EventlogType
         -> Args
         -> [TabGroup]
         -> Html
template (EventlogType header' x y) as tab_groups = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader x y as
  body $ H.div ! class_ "container-fluid" $ do
    H.div ! class_ "row" $ navbar tab_groups
    H.div ! class_ "row" $ do
      H.div ! class_ "col tab-content custom-tab" $ do
        forM_ tab_groups $ \group -> do
          case group of
            SingleTab tab -> renderTab header' tab
            ManyTabs _ tabs -> mapM_ (renderTab header') tabs

    script $ preEscapedToHtml tablogic

renderTab :: Header -> Tab -> Html
renderTab header' tab =
  H.div ! A.id (toValue (tabIDToTabID (tabId tab))) ! class_ ("tab-pane tabviz " <> status) $ H.div ! class_ "row" $ do
    forM_ (tabContent tab) $ \stuff -> H.div ! class_ "col" $ do
      stuff
      perTabFooter header'
    forM_ (tabDocs tab) $ \docs -> H.div ! class_ "col" $ docs
  where
    status = if tabActive tab then "show active" else ""

perTabFooter :: Header -> Html
perTabFooter header' = do
    H.div ! class_ "row" $ do
      H.div ! class_ "col" $ do
          toHtml $ maybe "No heap profile" ppHeapProfileType (hHeapProfileType header')
          ", created at "
          code $ toHtml $ hDate header'
          " by "
          code $ toHtml $ hJob header'


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

renderChart :: IncludeTraceData -> ChartType -> Bool -> TabID -> Text -> Html
renderChart itd ct vega_lite vid vegaSpec = do
    let fields = select_data itd ct
    H.div ! A.id (toValue (tabIDToVizID vid)) ! class_ "chart" $ ""
    script ! type_ "text/javascript" $ do
      if vega_lite
        then encloseScript fields vid vegaSpec
        else encloseRawVegaScript vid vegaSpec

renderChartWithJson :: IncludeTraceData -> ChartType -> TabID -> Value -> Text -> Html
renderChartWithJson itd ct k dat vegaSpec = do
    script $ insertJsonData dat
    renderChart itd ct True k vegaSpec


templateString :: EventlogType
               -> Args
               -> String
templateString x as =
  renderHtml $ template x as $ allTabs x as


ppHeapProfileType :: HeapProfBreakdown -> Text
ppHeapProfileType (HeapProfBreakdownCostCentre) = "Cost centre profiling (implied by -hc)"
ppHeapProfileType (HeapProfBreakdownModule) = "Profiling by module (implied by -hm)"
ppHeapProfileType (HeapProfBreakdownClosureDescr) = "Profiling by closure description (implied by -hd)"
ppHeapProfileType (HeapProfBreakdownTypeDescr) = "Profiling by type (implied by -hy)"
ppHeapProfileType (HeapProfBreakdownRetainer) = "Retainer profiling (implied by -hr)"
ppHeapProfileType (HeapProfBreakdownBiography) = "Biographical profiling (implied by -hb)"
ppHeapProfileType (HeapProfBreakdownClosureType) = "Basic heap profile (implied by -hT)"
ppHeapProfileType (HeapProfBreakdownInfoTable) = "Info table profile (implied by -hi)"


allTabs :: EventlogType
        -> Args
        -> [TabGroup]
allTabs (EventlogType h x y) as =
    [SingleTab (metaTab h as)] ++
    maybe [] (allHeapTabs h as) x ++
    [tickyProfileTabs y]

metaTab :: Header -> Args -> Tab
metaTab header' _as =
    (mkTab "Meta" "meta" metadata Nothing) { tabActive = True }
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

allHeapTabs :: Header -> Args -> HeapProfileData -> [TabGroup]
allHeapTabs header' as x =
    [ heapTab as
    , heapProfileTabs header' as x
    , costCentresTab as x
    , detailedTab x
    ]

heapTab :: Args -> TabGroup
heapTab as = SingleTab $ mkTab "Heap" tabid (mk as HeapChart tabid) (Just heapDocs)
  where
    tabid = "heapchart"

heapDocs :: Html
heapDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/heap.html")


heapProfileTabs :: Header -> Args -> HeapProfileData -> TabGroup
heapProfileTabs header' as _
  | has_heap_profile header' = ManyTabs "Heap Profile" $
    [ mkTab "Area Chart"  "areachart"       (mk as (AreaChart Stacked)     "areachart")       noDocs
    , mkTab "Normalized"  "normalizedchart" (mk as (AreaChart Normalized)  "normalizedchart") noDocs
    , mkTab "Streamgraph" "streamgraph"     (mk as (AreaChart StreamGraph) "streamgraph")     noDocs
    , mkTab "Linechart"   "linechart"       (mk as LineChart               "linechart")       noDocs
    ]
  | otherwise = SingleTab noHeapProfileTab

noHeapProfileTab :: Tab
noHeapProfileTab = mkUnavailableTab "Heap Profile" "heap_profile"  noHeapProfileDocs

noHeapProfileDocs :: Html
noHeapProfileDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-heap-profile.html")


mk :: Args -> ChartType -> TabID -> Html
mk as conf vid = renderChart itd conf True vid
                      (TL.toStrict (encodeToLazyText (vegaJson (htmlConf as conf))))
  where
    itd = if noTraces as then NoTraceData else TraceData

detailedTab :: HeapProfileData -> TabGroup
detailedTab (HeapProfileData _dat _cc_descs closure_descs) =
    SingleTab $ mkOptionalTab "Detailed" "closures" Prelude.id noDocs noDetailedDocs closure_descs

noDetailedDocs :: Html
noDetailedDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-detailed.html")


costCentresTab :: Args -> HeapProfileData -> TabGroup
costCentresTab as (HeapProfileData _dat cc_descs _) =
    SingleTab $ mkOptionalTab "Cost Centres" "costcentres" (const stuff) noDocs noCostCentresDocs cc_descs
  where
    tabIx = "costcentres"
    itd = if noTraces as then NoTraceData else TraceData
    stuff = renderChart itd LineChart False tabIx treevega

noCostCentresDocs :: Html
noCostCentresDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-cost-centres.html")


tickyProfileTabs :: Maybe TickyProfileData -> TabGroup
tickyProfileTabs = SingleTab . mkOptionalTab "Ticky" "ticky" tickyTab noDocs noTickyDocs

noTickyDocs :: Html
noTickyDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-ticky.html")
