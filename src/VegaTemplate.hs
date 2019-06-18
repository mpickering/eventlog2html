{-# LANGUAGE OverloadedStrings #-}
module VegaTemplate
  (
    vegaResult
  ) where

import Prelude hiding (filter, lookup)
import Graphics.Vega.VegaLite as VL
import Data.Aeson.Types
import Data.Text (Text)

-- | Workaround for some limitations in the HVega library.
-- This function should be removed once the features are merged into HVega.
injectJSON :: Text -> Value -> BuildLabelledSpecs
injectJSON t val = \x -> x ++ [(t,val)]

injectJSONs :: [(Text, Value)] -> BuildLabelledSpecs
injectJSONs ts = \x -> x ++ ts

-----------------------------------------------------------------------------------
-- The visualization consists of:
-- - AreaChart (on the left top)
-- - SelectionChart (on the left bottom)
-- - Legend (on the right)
-----------------------------------------------------------------------------------

-- | Takes as arguments the URLs of the JSON files for the bands and traces.
vegaResult :: Text -> Text -> VegaLite
vegaResult bands traces = toVegaLite
  [
    VL.width 1200,
    VL.height 1000,
    config [],
    description "Heap Profile",
    hConcat [lineChartFull bands traces, legendDiagram bands]
  ]


lineChartFull, areaChartFull :: Text -> Text -> VLSpec
areaChartFull bands traces = asSpec [vConcat [areaChart bands traces, selectionChart bands]]

lineChartFull bands traces = asSpec [vConcat [lineChart bands traces, selectionChart bands]]

config :: [LabelledSpec] -> (VLProperty, VLSpec)
config =
  configure
    . configuration (View [ViewWidth 400, ViewHeight 300])
    . configuration (TextStyle [(MAlign AlignRight), (MdX (-5)), (MdY 5)])


-----------------------------------------------------------------------------------
-- The Line Chart
-----------------------------------------------------------------------------------

lineChart :: Text -> Text -> VLSpec
lineChart bands traces = asSpec [layer [linesLayer bands , tracesLayer traces]]

linesLayer :: Text -> VLSpec
linesLayer bands = asSpec
  [
    VL.width 800,
    VL.height 700,
    dataFromUrl bands [],
    VL.mark Line [],
    encodingLineLayer [],
    transformLineLayer
  ]

encodingLineLayer
 = encoding
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""],
                  PScale [SDomain (DSelection "brush")]]
    . position Y [PName "norm_y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat "s"]]

transformLineLayer :: (VLProperty, VLSpec)
transformLineLayer =
  -- We need to get the `VLTransform` data constructor but it's not
  -- exported
  let (label, vs) = transform . filter (FSelection "legend") $ []
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
brush = (selection . select "brush" Interval [Encodings [ChX]]) []

selectionChart :: Text -> VLSpec
selectionChart bands = asSpec [
    VL.width 800,
    VL.height 100,
    dataFromUrl bands [],
    VL.mark Area [],
    encodingSelection [],
    brush
  ]

-----------------------------------------------------------------------------------
-- The Area Chart consists of:
-- - Traces Layer
-- - Bands Layer
-----------------------------------------------------------------------------------

areaChart :: Text -> Text -> VLSpec
areaChart bands traces = asSpec [layer [bandsLayer bands, tracesLayer traces]]

-----------------------------------------------------------------------------------
-- The bands layer:
-----------------------------------------------------------------------------------

bandsLayer :: Text -> VLSpec
bandsLayer bands = asSpec
  [
    VL.width 800,
    VL.height 700,
    dataFromUrl bands [],
    VL.mark Area [],
    encodingBandsLayer [],
    transformBandsLayer []
  ]

encodingBandsLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingBandsLayer =
  encoding
    . order [OName "k", OmType Quantitative]
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""],
                    PScale [SDomain (DSelection "brush")]]
    . position Y [PName "y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat "s"], PAggregate Sum, PStack StZero]

transformBandsLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
transformBandsLayer =
  transform
    . filter (FSelection "legend")

-----------------------------------------------------------------------------------
-- The traces layer:
-----------------------------------------------------------------------------------

tracesLayer :: Text -> VLSpec
tracesLayer traces = asSpec
  [
    dataFromUrl traces [],
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
-----------------------------------------------------------------------------------

legendDiagram :: Text -> VLSpec
legendDiagram bands = asSpec
  [
    VL.mark Point [MStroke "transparent"],
    dataFromUrl bands [],
    encodingRight [],
    selectionRight []
  ]

encodingRight :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingRight =
  encoding
  . injectJSON "tooltip" Null
  . order [OName "k", OmType Quantitative]
  . injectJSON "color" (object [
                    ("value", String "lightgray")
                    , ("condition", object [
                                           ("aggregate", String "min")
                                           ,("field", String "c")
                                           ,("legend", Null)
                                           ,("selection", String "legend")
                                           ,("type", String "nominal")])
                    ])
  . position Y [PName "c", PmType Nominal, PAxis [AxOrient SRight, AxDomain False, AxTicks False, AxGrid False]]

selectionRight :: [LabelledSpec] -> (VLProperty, VLSpec)
selectionRight =
    selection
     . select "legend" Multi [On "click", Encodings [ChColor], ResolveSelections Global, Toggle "event.shiftKey"]

