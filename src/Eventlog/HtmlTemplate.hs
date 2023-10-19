{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Eventlog.HtmlTemplate where

import Data.Aeson (Value, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.QQ
import Data.String
import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
--import Text.Blaze.Html
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

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
import qualified Data.Aeson as JSON
import qualified Profiteur.Renderer as Profiteur

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
    vidt = T.pack vid

jsScript :: String -> Html
jsScript url = script ! src (fromString $ url) $ ""
css :: AttributeValue -> Html
css url = link ! rel "stylesheet" ! href url

htmlHeader :: Maybe HeapProfileData
           -> Maybe TickyProfileData
           -> Args
           -> Html
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
        --mconcat $ Profiteur._CssAssets pdCssAssets
        --mconcat $ Profiteur._JsAssets pdJsAssets
        --Profiteur.encodedProfToHtml cannedProfJson
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

template :: Header
         -> Maybe HeapProfileData
         -> Maybe TickyProfileData
         -> Args
         -> [TabGroup] -> Html
template header' x y as tabs = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader x y as
  body $ H.div ! class_ "container-fluid vh-100" $ do
    H.div ! class_ "row" $ navbar indexed_tabs
    H.div ! class_ "row h-100" $ do
      H.div ! class_ "col tab-content custom-tab" $ do
        forM_ indexed_tabs $ \(_, group) -> do
          case group of
            SingleTab tab    -> renderTab header' tab
            ManyTabs _ mtabs -> mapM_ (renderTab header') mtabs

    script $ preEscapedToHtml tablogic

  where
    indexed_tabs :: [(Int, TabGroup)]
    indexed_tabs = zip [1..] tabs

renderTab :: Header -> Tab -> Html
renderTab header' tab =
  H.div ! A.id (toValue (tabId tab)) ! class_ ("tab-pane fade tabviz " <> status) $ H.div ! class_ "row" $ do
    forM_ (tabContent tab) $ \stuff -> H.div ! class_ "col" $ do
      stuff (tabId tab)
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

renderChart :: IncludeTraceData -> ChartType -> Bool -> VizID -> Text -> Html
renderChart itd ct vega_lite vid vegaSpec = do
    let fields = select_data itd ct
    H.div ! A.id (fromString $ "vis" ++ vid) ! class_ "chart" $ ""
    script ! type_ "text/javascript" $ do
      if vega_lite
        then encloseScript fields vid vegaSpec
        else encloseRawVegaScript vid vegaSpec

renderChartWithJson :: IncludeTraceData -> ChartType -> VizID -> Value -> Text -> Html
renderChartWithJson itd ct k dat vegaSpec = do
    script $ insertJsonData dat
    renderChart itd ct True k vegaSpec


templateString :: Header
               -> Maybe HeapProfileData
               -> Maybe TickyProfileData
               -> Args
               -> ProfiteurData
               -> String
templateString h x y as pd =
  renderHtml $ template h x y as $ allTabs h x y as pd


ppHeapProfileType :: HeapProfBreakdown -> Text
ppHeapProfileType (HeapProfBreakdownCostCentre) = "Cost centre profiling (implied by -hc)"
ppHeapProfileType (HeapProfBreakdownModule) = "Profiling by module (implied by -hm)"
ppHeapProfileType (HeapProfBreakdownClosureDescr) = "Profiling by closure description (implied by -hd)"
ppHeapProfileType (HeapProfBreakdownTypeDescr) = "Profiling by type (implied by -hy)"
ppHeapProfileType (HeapProfBreakdownRetainer) = "Retainer profiling (implied by -hr)"
ppHeapProfileType (HeapProfBreakdownBiography) = "Biographical profiling (implied by -hb)"
ppHeapProfileType (HeapProfBreakdownClosureType) = "Basic heap profile (implied by -hT)"
ppHeapProfileType (HeapProfBreakdownInfoTable) = "Info table profile (implied by -hi)"


allTabs :: Header
        -> Maybe HeapProfileData
        -> Maybe TickyProfileData
        -> Args
        -> ProfiteurData
        -> [TabGroup]
allTabs h x y as pd =
    [SingleTab (metaTab h as)] ++
    maybe [] (allHeapTabs h as) x ++
    [tickyProfileTabs y] ++
    [profiteurTab pd]

metaTab :: Header -> Args -> Tab
metaTab header' _as =
    Tab "Meta" "meta" (Just (const metadata)) Nothing True False
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
heapTab as = SingleTab $ Tab "Heap" "heapchart" (Just (mk as HeapChart)) (Just heapDocs) False False

heapDocs :: Html
heapDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/heap.html")


heapProfileTabs :: Header -> Args -> HeapProfileData -> TabGroup
heapProfileTabs header' as _
  | has_heap_profile header' = ManyTabs "Heap Profile" $
    [ Tab "Area Chart" "areachart"       (Just (mk as (AreaChart Stacked)))     noDocs False False
    , Tab "Normalized" "normalizedchart" (Just (mk as (AreaChart Normalized)))  noDocs False False
    , Tab "Streamgraph" "streamgraph"    (Just (mk as (AreaChart StreamGraph))) noDocs False False
    , Tab "Linechart" "linechart"        (Just (mk as LineChart))               noDocs False False
    ]
  | otherwise = SingleTab noHeapProfileTab

noHeapProfileTab :: Tab
noHeapProfileTab = Tab "Heap Profile" "heap_profile" Nothing (Just noHeapProfileDocs) False True

noHeapProfileDocs :: Html
noHeapProfileDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-heap-profile.html")


mk :: Args -> ChartType -> VizID -> Html
mk as conf vid = renderChart itd conf True vid
                      (TL.toStrict (encodeToLazyText (vegaJson (htmlConf as conf))))
  where
    itd = if noTraces as then NoTraceData else TraceData

detailedTab :: HeapProfileData -> TabGroup
detailedTab (HeapProfileData _dat _cc_descs closure_descs) = case closure_descs of
    Just v -> SingleTab $ Tab "Detailed" "closures" (Just (const v)) noDocs False False
    Nothing -> SingleTab $ Tab "Detailed" "closures" Nothing (Just noDetailedDocs) False True

noDetailedDocs :: Html
noDetailedDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-detailed.html")


costCentresTab :: Args -> HeapProfileData -> TabGroup
costCentresTab as (HeapProfileData _dat cc_descs _) = case cc_descs of
    Just _ -> SingleTab $ Tab "Cost Centres" "costcentres" (Just (\tabIx -> renderChart itd LineChart False tabIx treevega)) noDocs False False
    Nothing -> SingleTab noCostCentresTab
  where
    itd = if noTraces as then NoTraceData else TraceData

noCostCentresTab :: Tab
noCostCentresTab = Tab "Cost Centres" "costcentres" Nothing (Just noCostCentresDocs) False True

noCostCentresDocs :: Html
noCostCentresDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-cost-centres.html")


tickyProfileTabs :: Maybe TickyProfileData -> TabGroup
tickyProfileTabs (Just y) = SingleTab $ Tab "Ticky" "ticky" (Just (const (tickyTab y))) Nothing False False
tickyProfileTabs Nothing  = SingleTab $ Tab "Ticky" "ticky" Nothing (Just noTickyDocs) False True

noTickyDocs :: Html
noTickyDocs = H.div $ preEscapedToHtml $ T.decodeUtf8 $(embedFile "inline-docs/no-ticky.html")

profiteurTab :: ProfiteurData -> TabGroup
profiteurTab ProfiteurData{..} =
  SingleTab $ Tab "Profiteur" "profiteur" (Just (const cannedProfiteur)) Nothing False False
  where
    --cannedProfiteur :: Html
    --cannedProfiteur = H.preEscapedToHtml T.empty -- pdBodyContent
    cannedProfiteur =
      H.iframe ! A.id "profiteur-iframe"
               ! A.srcdoc ( fromString $ renderHtml $ Profiteur.jsonReportToHtml "rncryptor-test.prof"
                                 pdJsAssets
                                 pdCssAssets
                                 cannedProfJson)
               ! A.width "100%"
               ! A.height "100%"
               $ mempty

cannedProfJson :: JSON.Value
cannedProfJson = [aesonQQ| [{"1119":["main","Main","",0,0.9,1.3,["1239","1386","1524","1662","1801","1972"]],"1131":["testForeignEncryption","Tests","",0,0.1,0.1,["1136"]],"1136":["decrypt","Crypto.RNCryptor.V3.Decrypt","",1,0.1,0.1,["1151"]],"1144":["makeKey","Crypto.RNCryptor.Types","",1,0.1,0.1,[]],"1151":["decrypt.ctx","Crypto.RNCryptor.V3.Decrypt","",1,0.1,0.1,["1152"]],"1152":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.1,["1217"]],"1217":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1218"]],"1218":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1239":["allEmptyOrZero","PasswordBasedVectors","",0,0.1,0.2,["1240"]],"1240":["withTestVector","PasswordBasedVectors","",0,0.1,0.2,["1241"]],"1241":["withTestVector.ctx","PasswordBasedVectors","",1,0.1,0.2,["1242"]],"1242":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.2,["1277","1296"]],"1277":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1278"]],"1278":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1296":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1297"]],"1297":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1386":["oneByte","PasswordBasedVectors","",0,0.1,0.2,["1387"]],"1387":["withTestVector","PasswordBasedVectors","",0,0.1,0.2,["1388"]],"1388":["withTestVector.ctx","PasswordBasedVectors","",1,0.1,0.2,["1389"]],"1389":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.2,["1421","1439"]],"1421":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1422"]],"1422":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1439":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1440"]],"1440":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1524":["exactlyOneBlock","PasswordBasedVectors","",0,0.1,0.2,["1525"]],"1525":["withTestVector","PasswordBasedVectors","",0,0.1,0.2,["1526"]],"1526":["withTestVector.ctx","PasswordBasedVectors","",1,0.1,0.2,["1527"]],"1527":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.2,["1559","1577"]],"1559":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1560"]],"1560":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1577":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1578"]],"1578":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1662":["moreThanOneBlock","PasswordBasedVectors","",0,0.1,0.2,["1663"]],"1663":["withTestVector","PasswordBasedVectors","",0,0.1,0.2,["1664"]],"1664":["withTestVector.ctx","PasswordBasedVectors","",1,0.1,0.2,["1665"]],"1665":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.2,["1697","1715"]],"1697":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1698"]],"1698":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1715":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1716"]],"1716":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1801":["multibytePassword","PasswordBasedVectors","",0,0.1,0.2,["1802"]],"1802":["withTestVector","PasswordBasedVectors","",0,0.1,0.2,["1803"]],"1803":["withTestVector.ctx","PasswordBasedVectors","",1,0.1,0.2,["1804"]],"1804":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.2,["1836","1854"]],"1836":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1837"]],"1837":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1854":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["1855"]],"1855":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"1972":["longerTextAndPassword","PasswordBasedVectors","",0,0.2,0.2,["1973"]],"1973":["withTestVector","PasswordBasedVectors","",0,0.2,0.2,["1974"]],"1974":["withTestVector.ctx","PasswordBasedVectors","",1,0.1,0.2,["1975"]],"1975":["newRNCryptorContext","Crypto.RNCryptor.Types","",1,0.1,0.2,["2007","2025"]],"2007":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["2008"]],"2008":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"2025":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",1,0.1,0.1,["2026"]],"2026":["makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,[]],"2096":["testEncryptDecryptRoundtrip","Tests","",0,29.2,44,["2099"]],"2099":["testEncryptDecryptRoundtrip.\\","Tests","",139,29.2,44,["2103","2160"]],"2103":["testEncryptDecryptRoundtrip.\\.ctx","Tests","",100,14.7,22,["2104"]],"2104":["newRNCryptorContext","Crypto.RNCryptor.Types","",100,14.7,22,["2123","2153"]],"2123":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",100,7.3,11,["2124"]],"2124":["makeKey","Crypto.RNCryptor.Types","",0,7.3,11,[]],"2153":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",100,7.4,11,["2154"]],"2154":["makeKey","Crypto.RNCryptor.Types","",0,7.4,11,[]],"2160":["decrypt","Crypto.RNCryptor.V3.Decrypt","",100,14.5,22,["2164","2171"]],"2164":["decrypt.hmac","Crypto.RNCryptor.V3.Decrypt","",100,7.3,11,["2165"]],"2165":["makeHMAC","Crypto.RNCryptor.Types","",100,7.3,11,["2167"]],"2167":["makeHMAC.key","Crypto.RNCryptor.Types","",100,7.3,11,["2168"]],"2168":["makeKey","Crypto.RNCryptor.Types","",0,7.3,11,[]],"2171":["decrypt.ctx","Crypto.RNCryptor.V3.Decrypt","",100,7.2,11,["2172"]],"2172":["newRNCryptorContext","Crypto.RNCryptor.Types","",100,7.2,11,["2215"]],"2215":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",100,7.2,11,["2216"]],"2216":["makeKey","Crypto.RNCryptor.Types","",0,7.2,11,[]],"2222":["streamingRoundTrip","Tests","",0,63.7,50.9,["2222.indiv","2225","2271"]],"2222.indiv":["streamingRoundTrip (indiv)","Tests","",0,0.6,1.7,[]],"2225":["encryptStream","Crypto.RNCryptor.V3.Encrypt","",100,33.6,25.5,["2233","2279"]],"2233":["encryptStreamWithContext","Crypto.RNCryptor.V3.Encrypt","",100,18.3,3.5,["2251"]],"2251":["processStream","Crypto.RNCryptor.V3.Stream","",100,18.3,3.5,["2252"]],"2252":["processStream.go","Crypto.RNCryptor.V3.Stream","",100,18.3,3.5,["2258"]],"2258":["encryptStreamWithContext.finaliseEncryption","Crypto.RNCryptor.V3.Encrypt","",100,18.3,3.5,["2288"]],"2271":["decryptStream","Crypto.RNCryptor.V3.Decrypt","",100,29.5,23.7,["2345","2359"]],"2279":["encryptStream.ctx","Crypto.RNCryptor.V3.Encrypt","",100,15.3,22,["2280"]],"2280":["newRNCryptorContext","Crypto.RNCryptor.Types","",100,15.3,22,["2291","2332"]],"2288":["encryptStreamWithContext.finaliseEncryption.(...)","Crypto.RNCryptor.V3.Encrypt","",100,18.3,3.5,["2288.indiv","2322"]],"2288.indiv":["encryptStreamWithContext.finaliseEncryption.(...) (indiv)","Crypto.RNCryptor.V3.Encrypt","",100,0.6,1.7,[]],"2291":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",100,7.7,11,["2292"]],"2292":["makeKey","Crypto.RNCryptor.Types","",0,7.7,11,[]],"2302":["arbitrary","Tests","",0,5.6,3.5,["2304"]],"2304":["arbitrary","Data.ByteString.Arbitrary","",0,5.6,3.5,["2307"]],"2307":["fastRandBs","Data.ByteString.Arbitrary","",0,5.6,3.5,["2307.indiv","2312"]],"2307.indiv":["fastRandBs (indiv)","Data.ByteString.Arbitrary","",0,0.4,1.7,[]],"2312":["fastRandBs.hashes","Data.ByteString.Arbitrary","",100,5.3,1.7,[]],"2322":["encryptBlock","Crypto.RNCryptor.V3.Encrypt","",100,17.7,1.7,["2322.indiv","2324"]],"2322.indiv":["encryptBlock (indiv)","Crypto.RNCryptor.V3.Encrypt","",100,12.5,0,[]],"2324":["encryptBlock.cipherText","Crypto.RNCryptor.V3.Encrypt","",100,5.2,1.7,["2327"]],"2327":["encryptBytes","Crypto.RNCryptor.V3.Encrypt","",100,5.2,1.7,["2327.indiv"]],"2327.indiv":["encryptBytes (indiv)","Crypto.RNCryptor.V3.Encrypt","",100,5.2,1.7,[]],"2332":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",100,7.7,11,["2333"]],"2333":["makeKey","Crypto.RNCryptor.Types","",0,7.7,11,[]],"2345":["processStream","Crypto.RNCryptor.V3.Stream","",100,14,1.7,["2346"]],"2346":["processStream.go","Crypto.RNCryptor.V3.Stream","",100,14,1.7,["2352"]],"2352":["decryptStream.finaliseDecryption","Crypto.RNCryptor.V3.Decrypt","",100,14,1.7,["2358"]],"2358":["decryptStream.finaliseDecryption.(...)","Crypto.RNCryptor.V3.Decrypt","",100,14,1.7,["2394"]],"2359":["decryptStream.ctx","Crypto.RNCryptor.V3.Decrypt","",100,15.5,22,["2360"]],"2360":["newRNCryptorContext","Crypto.RNCryptor.Types","",100,15.5,22,["2390","2428"]],"2390":["newRNCryptorContext.hmacKey","Crypto.RNCryptor.Types","",100,7.8,11,["2391"]],"2391":["makeKey","Crypto.RNCryptor.Types","",0,7.8,11,[]],"2394":["decryptBlock","Crypto.RNCryptor.V3.Decrypt","",100,14,1.7,["2394.indiv","2420"]],"2394.indiv":["decryptBlock (indiv)","Crypto.RNCryptor.V3.Decrypt","",100,12.5,0,[]],"2420":["decryptBlock.clearText","Crypto.RNCryptor.V3.Decrypt","",100,1.5,1.7,["2423"]],"2423":["decryptBytes","Crypto.RNCryptor.V3.Decrypt","",100,1.5,1.7,["2423.indiv"]],"2423.indiv":["decryptBytes (indiv)","Crypto.RNCryptor.V3.Decrypt","",100,1.5,1.7,[]],"2428":["newRNCryptorContext.encKey","Crypto.RNCryptor.Types","",100,7.7,11,["2429"]],"2429":["makeKey","Crypto.RNCryptor.Types","",0,7.7,11,[]],"559":["MAIN","MAIN","",0,100,100,["559.indiv","849","937","1119","2096","2222","2302"]],"559.indiv":["MAIN (indiv)","MAIN","",0,0.3,0,[]],"849":["CAF:makeKey","Crypto.RNCryptor.Types","",0,0.1,0.1,["1144"]],"937":["CAF:testForeignEncryption5","Tests","",0,0.1,0.1,["1131"]]},"559"] |]

