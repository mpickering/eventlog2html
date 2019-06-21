{-# LANGUAGE OverloadedStrings #-}
module VegaTemplate
  (
    AreaChartType(..)
  , ChartConfig(..)
  , ChartType(..)
  , vegaResult
  , vegaJson
  , vegaJsonText
  ) where

import Prelude hiding (filter, lookup)
import Graphics.Vega.VegaLite as VL
import Data.Aeson.Types hiding (Number)
import Data.Text (Text)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (toStrict)

-- | Workaround for some limitations in the HVega library.
-- This function should be removed once the features are merged into HVega.
injectJSON :: Text -> Value -> BuildLabelledSpecs
injectJSON t val = \x -> x ++ [(t,val)]

injectJSONs :: [(Text, Value)] -> BuildLabelledSpecs
injectJSONs ts = \x -> x ++ ts

data AreaChartType
  = Stacked
  | Normalized
  | StreamGraph

data ChartType
  = AreaChart AreaChartType
  | LineChart


-- Arguments for directly outputting javascript
data ChartConfig =
  ChartConfig { cwidth :: Double
              , cheight :: Double
              , traces :: Bool
              , chartType :: ChartType
              }

-----------------------------------------------------------------------------------
-- The visualization consists of:
-- - AreaChart (on the left top)
-- - SelectionChart (on the left bottom)
-- - Legend (on the right)
-----------------------------------------------------------------------------------


vegaJson :: ChartConfig -> Value
vegaJson conf = fromVL (vegaResult conf)

vegaJsonText :: ChartConfig -> Text
vegaJsonText conf = toStrict (encodeToLazyText (vegaJson conf))

vegaResult :: ChartConfig -> VegaLite
vegaResult conf = toVegaLite
  [
    VL.width (cwidth conf),
    VL.height (cheight conf),
    config [],
    description "Heap Profile",
    case chartType conf of
      LineChart -> lineChartFull conf
      AreaChart ct -> areaChartFull ct conf
  ]

areaChartFull :: AreaChartType -> ChartConfig -> (VLProperty, VLSpec)
areaChartFull ct c = hConcat
  [
    asSpec [vConcat [areaChart ct c, selectionChart c]]
  , legendDiagram
  ]

lineChartFull :: ChartConfig -> (VLProperty, VLSpec)
lineChartFull c = hConcat
  [
    asSpec [vConcat [lineChart c, selectionChart c]]
  , legendDiagram
  ]

config :: [LabelledSpec] -> (VLProperty, VLSpec)
config =
  configure
    . configuration (TextStyle [(MAlign AlignRight), (MdX (-5)), (MdY 5)])

-----------------------------------------------------------------------------------
-- The Line Chart
-----------------------------------------------------------------------------------

lineChart :: ChartConfig -> VLSpec
lineChart c = asSpec [layer ([linesLayer c] ++ [tracesLayer | traces c])]

linesLayer :: ChartConfig -> VLSpec
linesLayer c = asSpec
  [
    VL.width (0.75 * cwidth c),
    VL.height (0.7 * cheight c),
    dataFromSource "heap" [],
    VL.mark Line [],
    encodingLineLayer [],
    transformLineLayer
  ]

encodingLineLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingLineLayer
 = encoding
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""],
                  PScale [SDomain (DSelection "brush")]]
    . position Y [PName "norm_y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat ".1f"]]

transformLineLayer :: (VLProperty, VLSpec)
transformLineLayer =
  -- We need to get the `VLTransform` data constructor but it's not
  -- exported
  let (label, _vs) = transform . filter (FSelection "legend") $ []
  in (label,
  toJSON [object ["window" .= [object ["field" .= String "y"
                                      , "op" .= String "max"
                                      , "as" .= String "max_y"]]
                              , "frame" .= toJSON [Null, Null]
                              , "groupby" .= toJSON [String "k"]]
         , object ["calculate" .= String "datum.y / datum.max_y"
                          , "as" .= String "norm_y"]
         , object ["filter" .= object ["selection" .= String "legend"]]])


-----------------------------------------------------------------------------------
-- The Selection Chart
-----------------------------------------------------------------------------------

encodingSelection :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingSelection =
  encoding
    . order [OName "k", OmType Quantitative]
    . injectJSON "tooltip" Null
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle "Time (s)"]]
    . position Y [PName "y", PmType Quantitative, PAxis [{-AxTitle "Allocation", AxFormat "s"-}], PAggregate Sum, PStack StZero]

brush :: (VLProperty, VLSpec)
brush = (selection . injectJSON "brush" (object [ "type" .= String "interval"
                                                , "init" .= object [ "x" .= [Null, Null] ] ])) []
-- init field is not supported and necessary for dynamic loading

selectionChart :: ChartConfig -> VLSpec
selectionChart c = asSpec [
    VL.width (0.75 * cwidth c),
    VL.height (0.1 * cheight c),
    dataFromSource "heap" [],
    VL.mark Area [],
    encodingSelection [],
    brush
  ]

-----------------------------------------------------------------------------------
-- The Area Chart consists of:
-- - Traces Layer
-- - Bands Layer
-----------------------------------------------------------------------------------

areaChart :: AreaChartType -> ChartConfig -> VLSpec
areaChart ct c = asSpec [layer ([bandsLayer ct c] ++ [tracesLayer | traces c])]

-----------------------------------------------------------------------------------
-- The bands layer:
-----------------------------------------------------------------------------------

bandsLayer :: AreaChartType -> ChartConfig -> VLSpec
bandsLayer ct c = asSpec
  [
    VL.width (0.75 * cwidth c),
    VL.height (0.7 * cheight c),
    dataFromSource "heap" [],
    VL.mark Area [],
    encodingBandsLayer ct [],
    transformBandsLayer []
  ]

encodingBandsLayer :: AreaChartType -> [LabelledSpec] -> (VLProperty, VLSpec)
encodingBandsLayer ct  =
  encoding
    . order [OName "k", OmType Quantitative]
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""]
                 , PScale [SDomain (DSelection "brush")]]
    . position Y [PName "y"
                 , PmType Quantitative
                 , PAxis $ case ct of
                             Stacked -> [AxTitle "Allocation", AxFormat "s", AxMaxExtent 20.0, AxLabelOverlap OGreedy]
                             Normalized -> [AxTitle "Allocation (Normalized)", AxFormat "p"]
                             StreamGraph -> [AxTitle "Allocation (Streamgraph)", AxLabels False, AxTicks False, AxTitlePadding 10.0]
                 , PAggregate Sum
                 , PStack (case ct of
                             Stacked -> StZero
                             Normalized -> StNormalize
                             StreamGraph -> StCenter)]

transformBandsLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
transformBandsLayer =
  transform
    . filter (FSelection "legend")

-----------------------------------------------------------------------------------
-- The traces layer:
-----------------------------------------------------------------------------------

tracesLayer :: VLSpec
tracesLayer = asSpec
  [
    dataFromSource "traces" [],
    VL.mark Rule [],
    encodingTracesLayer [],
    selectionTracesLayer []
  ]

encodingTracesLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingTracesLayer =
  encoding
    . color [MString "grey"]
    . position X [PmType Quantitative, PAxis [], PName "tx", PScale [SDomain (DSelection "brush")]]
    . VL.size [MNumber 2]
    . opacity [MSelectionCondition (Expr "index") [MNumber 1] [MNumber 0.5]]
    -- The "tooltips" feature is not in the current version of HVega
    . injectJSON "tooltip" (toJSON [object ["field" .= String "tx", "type" .= String "quantitative"],
                             object ["field" .= String "desc", "type" .= String "nominal"]])

selectionTracesLayer ::  [LabelledSpec] -> (VLProperty, VLSpec)
selectionTracesLayer =
  selection
    . VL.select "index" Single [On "mousemove", Encodings [ChX], Nearest True]

-----------------------------------------------------------------------------------
-- The legend
-- In order to make the legend interactive we make it into another chart.
-- Workaround comes from https://github.com/vega/vega-lite/issues/1657
-----------------------------------------------------------------------------------

legendDiagram :: VLSpec
legendDiagram  = asSpec
  [
    VL.mark Point [MStroke "transparent"],
    dataFromSource "heap" [],
    encodingRight [],
    selectionRight []
  ]

encodingRight :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingRight =
  encoding
  . injectJSON "tooltip" Null
  . injectJSON "color" (object [
                    ("value", String "lightgray")
                    , ("condition", object [
                                           ("aggregate", String "min")
                                           ,("field", String "c")
                                           ,("legend", Null)
                                           ,("selection", String "legend")
                                           ,("type", String "nominal")])
                    ])
  . position Y [PName "c"
               , PmType Nominal
               , PAxis [AxOrient SRight, AxDomain False, AxTicks False, AxGrid False]
               , PSort [(ByField "k"), Descending]]

selectionRight :: [LabelledSpec] -> (VLProperty, VLSpec)
selectionRight =
    selection
     . select "legend" Multi [On "click", Encodings [ChColor], ResolveSelections Global, Toggle "event.shiftKey"]

