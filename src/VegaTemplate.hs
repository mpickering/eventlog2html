{-# LANGUAGE OverloadedStrings #-}
module VegaTemplate
  (
    vegaResult, vegaJson, vegaJsonText
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

-----------------------------------------------------------------------------------
-- The visualization consists of:
-- - AreaChart (on the left top)
-- - SelectionChart (on the left bottom)
-- - Legend (on the right)
-----------------------------------------------------------------------------------

vegaJsonText :: Text
vegaJsonText = toStrict (encodeToLazyText vegaJson)

vegaJson :: Value
vegaJson = fromVL vegaResult

vegaResult :: VegaLite
vegaResult = toVegaLite
  [
    VL.width 1200,
    VL.height 1000,
    config [],
    description "Heap Profile",
    hConcat [asSpec [vConcat [areaChart
                             , selectionChart]]
            , legendDiagram]
  ]

config :: [LabelledSpec] -> (VLProperty, VLSpec)
config =
  configure
    . configuration (TextStyle [(MAlign AlignRight), (MdX (-5)), (MdY 5)])

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

selectionChart :: VLSpec
selectionChart  = asSpec [
    VL.width 800,
    VL.height 100,
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

areaChart :: VLSpec
areaChart = asSpec [layer [bandsLayer, tracesLayer]]

-----------------------------------------------------------------------------------
-- The bands layer:
-----------------------------------------------------------------------------------

bandsLayer :: VLSpec
bandsLayer = asSpec
  [
    VL.width 800,
    VL.height 700,
    dataFromSource "heap" [],
    VL.mark Area [],
    encodingBandsLayer [],
    transformBandsLayer []
  ]

encodingBandsLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingBandsLayer =
  encoding
    . order [OName "k", OmType Quantitative]
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle ""]
                 , PScale [SDomain (DSelection "brush")]]
    . position Y [PName "y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat "s"], PAggregate Sum, PStack StZero]

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
    . position X [PmType Quantitative, PAxis [], PName "tx"
                 , PScale [SDomain (DSelection "brush")]]
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

