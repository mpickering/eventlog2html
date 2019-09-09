{-# LANGUAGE OverloadedStrings #-}
module Eventlog.VegaTemplate
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
              , colourScheme :: Text
              , chartType :: ChartType
              }

colourProperty :: ChartConfig -> ScaleProperty
colourProperty c = SScheme (colourScheme c) []

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
vegaResult conf = toVegaLite $
  -- Subtract 100 from the width for the fixed size label allocation.
  let c' = conf { cwidth = cwidth conf - 130 }
  in
  [
    VL.width (cwidth conf),
    VL.height (cheight conf),
    config [],
    description "Heap Profile",
    case chartType conf of
      LineChart -> lineChartFull c'
      AreaChart ct -> areaChartFull ct c'
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
lineChart c = asSpec [layer ([linesLayer c] ++ [tracesLayer])]

linesLayer :: ChartConfig -> VLSpec
linesLayer c = asSpec
  [
    VL.width (0.9 * cwidth c),
    VL.height (0.7 * cheight c),
    dataFromSource "data_json_samples" [],
    VL.mark Line [],
    encodingLineLayer c [],
    transformLineLayer []
  ]

encodingLineLayer :: ChartConfig -> [LabelledSpec] -> (VLProperty, VLSpec)
encodingLineLayer c
 = encoding
    . color [MName "c", MmType Nominal, MScale [colourProperty c], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""],
                  PScale [SDomain (DSelection "brush")]]
    . position Y [PName "norm_y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat ".1f"]]

transformLineLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
transformLineLayer =
  transform
  . window [([WField "y", WAggregateOp Max], "max_y")] [WFrame Nothing Nothing, WGroupBy ["k"]]
  . calculateAs "datum.y / datum.max_y" "norm_y"
  . filter (FSelection "legend")

-----------------------------------------------------------------------------------
-- The Selection Chart
-----------------------------------------------------------------------------------

encodingSelection :: ChartConfig -> [LabelledSpec] -> (VLProperty, VLSpec)
encodingSelection c =
  encoding
    . order [OName "k", OmType Quantitative]
    . injectJSON "tooltip" Null
    . color [MName "c", MmType Nominal, MScale [colourProperty c], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle "Time (s)"]]
    . position Y [PName "y", PmType Quantitative, PAxis [{-AxTitle "Allocation", AxFormat "s"-}], PAggregate Sum, PStack StZero]

brush :: (VLProperty, VLSpec)
brush = (selection . injectJSON "brush" (object [ "type" .= String "interval"
                                                , "init" .= object [ "x" .= [Null, Null] ] ])) []
-- init field is not supported and necessary for dynamic loading

selectionChart :: ChartConfig -> VLSpec
selectionChart c = asSpec [
    VL.width (0.9 * cwidth c),
    VL.height (0.1 * cheight c),
    dataFromSource "data_json_samples" [],
    VL.mark Area [],
    encodingSelection c [],
    brush
  ]

-----------------------------------------------------------------------------------
-- The Area Chart consists of:
-- - Traces Layer
-- - Bands Layer
-----------------------------------------------------------------------------------

areaChart :: AreaChartType -> ChartConfig -> VLSpec
areaChart ct c = asSpec [layer ([bandsLayer ct c] ++ [tracesLayer])]

-----------------------------------------------------------------------------------
-- The bands layer:
-----------------------------------------------------------------------------------

bandsLayer :: AreaChartType -> ChartConfig -> VLSpec
bandsLayer ct c = asSpec
  [
    VL.width (0.9 * cwidth c),
    VL.height (0.7 * cheight c),
    dataFromSource "data_json_samples" [],
    VL.mark Area [],
    encodingBandsLayer ct c [],
    transformBandsLayer []
  ]

encodingBandsLayer :: AreaChartType
                   -> ChartConfig
                   -> [LabelledSpec]
                   -> (VLProperty, VLSpec)
encodingBandsLayer ct c =
  encoding
    . order [OName "k", OmType Quantitative]
    . color [MName "c", MmType Nominal, MScale [colourProperty c], MLegend []]
    . tooltips
        [ [TName "y", TmType Quantitative, TFormat "s", TTitle "Allocation"]
        , [TName "c", TmType Nominal, TTitle "Type"]
        ]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""]
                 , PScale [SDomain (DSelection "brush")]]
    . position Y [PName "y"
                 , PmType Quantitative
                 , PAxis $ case ct of
                             Stacked -> [AxTitle "Allocation"
                                        , AxFormat "s"
                                        , AxTitlePadding 15.0
                                        , AxMaxExtent 15.0]
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
    dataFromSource "data_json_traces" [],
    VL.mark Rule [],
    encodingTracesLayer []
  ]

encodingTracesLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingTracesLayer =
  encoding
    . color [MString "grey"]
    . position X [PmType Quantitative, PAxis [], PName "tx", PScale [SDomain (DSelection "brush")]]
    . VL.size [MNumber 2]
    . tooltip [TName "desc", TmType Nominal]

-----------------------------------------------------------------------------------
-- The legend
-- In order to make the legend interactive we make it into another chart.
-- Workaround comes from https://github.com/vega/vega-lite/issues/1657
-----------------------------------------------------------------------------------

legendDiagram :: VLSpec
legendDiagram  = asSpec
  [
    VL.mark Point [MStroke "transparent"],
    dataFromSource "data_json_samples" [],
    encodingRight [],
    selectionRight []
  ]

encodingRight :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingRight =
  encoding
  . injectJSON "tooltip" Null
  . color
     [
       MSelectionCondition (SelectionName "legend") [MName "c", MmType Nominal, MAggregate Min, MLegend []] [MString "lightgray"]
     ]
  . position Y [PName "c"
               , PmType Nominal
               , PAxis [ AxOrient SRight
                       , AxDomain False
                       , AxTicks False
                       , AxGrid False
                       , AxMinExtent 100
                       , AxMaxExtent 100]
               , PSort [(ByFieldOp "k" Mean), Descending]]

selectionRight :: [LabelledSpec] -> (VLProperty, VLSpec)
selectionRight =
    selection
     . select "legend" Multi [On "click", Encodings [ChColor], ResolveSelections Global, Toggle "event.shiftKey"]

