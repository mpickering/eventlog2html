{-# LANGUAGE OverloadedStrings #-}
module VegaTemplate
  (
    vegaResult
  ) where

import Prelude hiding (filter, lookup)
import Graphics.Vega.VegaLite as VL
import Data.Aeson.Types 
import Data.Text (Text)

foo :: Text -> Value -> BuildLabelledSpecs
foo t val = \x -> x ++ [(t,val)]

-----------------------------------------------------------------------------------
-- The visualization consists of:
-- - Area Chart (on the left)
-- - Legend (on the right)
-----------------------------------------------------------------------------------

vegaResult :: Text -> Text -> VegaLite
vegaResult bands traces = toVegaLite
  [
    VL.width 1000,
    VL.height 1000,
    config [],
    description "Heap Profile",
    hConcat [asSpec (areaChart bands traces), asSpec (legendDiagram bands)]
  ]

config :: [LabelledSpec] -> (VLProperty, VLSpec)
config =
  configure
    . configuration (View [ViewWidth 400, ViewHeight 300])
    . configuration (TextStyle [(MAlign AlignRight), (MdX (-5)), (MdY 5)])

-----------------------------------------------------------------------------------
-- The Area Chart consists of:
-- - Traces Layer
-- - Bands Layer
-----------------------------------------------------------------------------------

areaChart :: Text -> Text -> [(VLProperty, VLSpec)]
areaChart bands traces = [layer [asSpec (bandsLayer bands), asSpec (tracesLayer traces)]]

-----------------------------------------------------------------------------------
-- The bands layer:
-----------------------------------------------------------------------------------

bandsLayer :: Text -> [(VLProperty, VLSpec)]
bandsLayer bands =
  [
    VL.width 800,
    VL.height 800,
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
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle "Time (s)"]]
    . position Y [PName "y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat "s"], PAggregate Sum, PStack StZero]

transformBandsLayer :: [LabelledSpec] -> (VLProperty, VLSpec)
transformBandsLayer =
  transform
    . filter (FSelection "legend")
    
-----------------------------------------------------------------------------------
-- The traces layer:
-----------------------------------------------------------------------------------

tracesLayer :: Text -> [(VLProperty, VLSpec)]
tracesLayer traces =
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
    . position X [PmType Quantitative, PAxis [], PName "tx"]
    . foo "x" (object [("type", String "quantitative"), ("field", String "tx"), ("axis", object [])])
    . VL.size [MNumber 2]
    . opacity [MSelectionCondition (Expr "index") [MNumber 1] [MNumber 0.5]]
    . foo "tooltip" (toJSON [object ["field" .= String "tx", "type" .= String "quantitative"],
                             object ["field" .= String "desc", "type" .= String "nominal"]])
    -- . tooltip [TName "tx", TmType Quantitative]
    -- . tooltip [TName "desc", TmType Nominal]

selectionTracesLayer ::  [LabelledSpec] -> (VLProperty, VLSpec)
selectionTracesLayer =
  selection
    . VL.select "index" Single [On "mousemove", Encodings [ChX], Nearest True]

-----------------------------------------------------------------------------------
-- The legend
-----------------------------------------------------------------------------------

legendDiagram :: Text -> [(VLProperty, VLSpec)]
legendDiagram bands =
  [
    VL.mark Point [MStroke "transparent"],
    dataFromUrl bands [],
    encodingRight [],
    selectionRight []
  ]

encodingRight :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingRight =
  encoding
  . foo "tooltip" Null
  . order [OName "k", OmType Quantitative]
  . foo "color" (object [
                    ("value", String "lightgray")
                    , ("condition", object [
                                           ("aggregate", String "min")
                                           ,("field", String "c")
                                           ,("legend", Null)
                                           ,("selection", String "legend")
                                           ,("type", String "nominal")])
                    ])
  -- . color
  --    [
  --      MString "lightgray",
  --      MSelectionCondition
  --        (Selection "legend")
  --        [MmType Nominal, MAggregate Min, MName "c", MLegend []]
  --        []
  --    ]
--  . foo "y" (object [("field", String "c"), ("title", Null), ("type", String "nominal"]))
  . position Y [PName "c", PmType Nominal, PAxis [AxOrient SRight, AxDomain False, AxTicks False, AxGrid False]]
--  . VL.select "legend" Multi [On "click", Toggle "event.shiftKey", ResolveSelections Global, Encodings [ChColor], Empty ]
  
selectionRight :: [LabelledSpec] -> (VLProperty, VLSpec)
selectionRight =
    selection
     . foo "legend" (object [
                        "empty" .= String "all",
                        "encodings" .= toJSON [String "color"],
                        "on" .= String "click",
                        "resolve" .= String "global",
                        "type" .= String "multi",
                        "toggle" .= String "event.shiftKey"])

