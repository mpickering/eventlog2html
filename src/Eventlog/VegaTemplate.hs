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
              , metrics :: Bool
              , colourScheme :: Text
              , chartType :: ChartType
              , fixedYAxisExtent :: Maybe Double
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
areaChartFull ct c = vConcat [areaChart ct c,  selectionChart c]

lineChartFull :: ChartConfig -> (VLProperty, VLSpec)
lineChartFull c = vConcat [lineChart c, selectionChart c]

config :: [ConfigureSpec] -> (VLProperty, VLSpec)
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
    VL.width (0.9 * cwidth c),
    VL.height (0.7 * cheight c),
    dataFromSource "data_json_samples" [],
    VL.mark Line [MPoint (PMMarker [])],
    encodingLineLayer c [],
    transformLineLayer [],
    selectionRight []
  ]

encodingLineLayer :: ChartConfig -> [EncodingSpec] -> (VLProperty, VLSpec)
encodingLineLayer c
 = encoding
    . color [MName "c", MmType Nominal, MScale [colourProperty c]
            , MSort [ByFieldOp "k" Max, Descending]
            , MLegend [LNoTitle]]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""]
                 ,PScale [SDomain (DSelection "brush")]]
    . position Y [PName "norm_y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat ".1f"]]

transformLineLayer :: [TransformSpec] -> (VLProperty, VLSpec)
transformLineLayer =
  transform
  . window [([WField "y", WAggregateOp Max], "max_y")] [WFrame Nothing Nothing, WGroupBy ["k"]]
  . calculateAs "datum.y / datum.max_y" "norm_y"
  . filter (FSelection "legend")

-----------------------------------------------------------------------------------
-- The Selection Chart
-----------------------------------------------------------------------------------

encodingSelection :: ChartConfig -> [EncodingSpec] -> (VLProperty, VLSpec)
encodingSelection c =
  encoding
    . order [OName "k", OmType Quantitative]
    . tooltip []
    . color [MName "c", MmType Nominal, MScale [colourProperty c], MLegend []
            , MSort [ByFieldOp "k" Max, Descending]]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle "Time (s)"]]
    . position Y [PName "y", PmType Quantitative, PAxis [{-AxTitle "Allocation", AxFormat "s"-}], PAggregate Sum, PStack StZero]

brush :: [SelectSpec] -> PropertySpec
brush = selection .
          select "brush" Interval [SInitInterval (Just (NullValue, NullValue)) Nothing]

-- init field is not supported and necessary for dynamic loading

selectionChart :: ChartConfig -> VLSpec
selectionChart c = asSpec [
    VL.width (0.9 * cwidth c),
    VL.height (0.1 * cheight c),
    dataFromSource "data_json_samples" [],
    VL.mark Area [],
    encodingSelection c [],
    brush []
  ]

-----------------------------------------------------------------------------------
-- The Area Chart consists of:
-- - Traces Layer
-- - Bands Layer
-----------------------------------------------------------------------------------

areaChart :: AreaChartType -> ChartConfig -> VLSpec
areaChart ct c = asSpec [layer ([bandsLayer ct c]
                                ++ [tracesLayer | traces c]
                                ++ [metricsLayer | metrics c])]

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
    transformBandsLayer [],
    selectionRight []
  ]

encodingBandsLayer :: AreaChartType
                   -> ChartConfig
                   -> [EncodingSpec]
                   -> (VLProperty, VLSpec)
encodingBandsLayer ct c =
  encoding
    . order [OName "k", OmType Quantitative]
    . color [MName "c", MmType Nominal, MScale [colourProperty c]
            , MSort [ByFieldOp "k" Max, Descending]
            , MLegend [LNoTitle]
            ]
    . tooltips
        [ [TName "y", TmType Quantitative, TFormat "s", TTitle "Allocation"]
        , [TName "c", TmType Nominal, TTitle "Type"]
        ]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""]
                 , PScale [SDomain (DSelection "brush")]]
    . position Y ([PName "y"
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
                 ++
                  [PScale [SDomain (DNumbers [0,  extent])] | Just extent <- [fixedYAxisExtent c]] )

transformBandsLayer :: [TransformSpec] -> (VLProperty, VLSpec)
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

encodingTracesLayer :: [EncodingSpec] -> (VLProperty, VLSpec)
encodingTracesLayer =
  encoding
    . color [MString "grey"]
    . position X [PmType Quantitative, PAxis [], PName "tx"
                 , PScale [SDomain (DSelection "brush")] ]
    . VL.size [MNumber 2]
    . tooltip [TName "desc", TmType Nominal]

-----------------------------------------------------------------------------------
-- The metrics layer:
-----------------------------------------------------------------------------------

metricsLayer :: VLSpec
metricsLayer = asSpec
  [
    dataFromSource "data_json_metrics" [],
    VL.mark Line [],
    encodingMetricsLayer []
  ]

encodingMetricsLayer :: [EncodingSpec] -> (VLProperty, VLSpec)
encodingMetricsLayer =
  encoding
    . color [MString "grey"]
    . position X [PmType Quantitative, PAxis [], PName "tx"
                 , PScale [SDomain (DSelection "brush")] ]
    . position Y2 [PmType Quantitative, PAxis [], PName "m" ]


-----------------------------------------------------------------------------------
-- The legend selection
-----------------------------------------------------------------------------------

selectionRight :: [SelectSpec] -> (VLProperty, VLSpec)
selectionRight =
    selection
      . select "legend" Multi [BindLegend (BLField "c")]

