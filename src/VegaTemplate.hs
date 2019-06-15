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

config :: [LabelledSpec] -> (VLProperty, VLSpec)
config =
  configure
    . configuration (View [ViewWidth 400, ViewHeight 300])
    . configuration (TextStyle [(MAlign AlignRight), (MdX (-5)), (MdY 5)])
    
vegaResult :: Text -> Text -> VegaLite
vegaResult bands traces = toVegaLite
  [
    VL.width 1000,
    VL.height 1000,
    config [],
    description "Heap Profile",
    hConcat [asSpec (leftDiagram bands traces), asSpec (rightDiagram bands)]
  ]

-----------------------------------------------------------------------------------
-- Left Diagram 
-----------------------------------------------------------------------------------

leftDiagram :: Text -> Text -> [(VLProperty, VLSpec)]
leftDiagram bands traces = [layer [ asSpec (layerOne bands), asSpec (layerTwo traces)]]

-----------------------------------------------------------------------------------
-- LayerOne of Left Diagram
-----------------------------------------------------------------------------------

encodingLayerOne :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingLayerOne =
  encoding
    . order [OName "k", OmType Quantitative]
    . color [MName "c", MmType Nominal, MScale [SScheme "category20" []], MLegend []]
    . position X [PName "x", PmType Quantitative, PAxis [AxTitle "Time (s)"]]
    . position Y [PName "y", PmType Quantitative, PAxis [AxTitle "Allocation", AxFormat "s"], PAggregate Sum, PStack StZero]

transformLayerOne :: [LabelledSpec] -> (VLProperty, VLSpec)
transformLayerOne =
  transform
    . filter (FSelection "legend")
    
layerOne :: Text -> [(VLProperty, VLSpec)]
layerOne bands =
  [
    VL.width 800,
    VL.height 800,
    dataFromUrl bands [],
    VL.mark Area [],
    encodingLayerOne [],
    transformLayerOne []
  ]

-----------------------------------------------------------------------------------
-- LayerTwo of Left Diagram
-----------------------------------------------------------------------------------

encodingLayerTwo :: [LabelledSpec] -> (VLProperty, VLSpec)
encodingLayerTwo =
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

selectionLayerTwo ::  [LabelledSpec] -> (VLProperty, VLSpec)
selectionLayerTwo =
  selection
    . VL.select "index" Single [On "mousemove", Encodings [ChX], Nearest True]

layerTwo :: Text -> [(VLProperty, VLSpec)]
layerTwo traces =
  [
    dataFromUrl traces [],
    layer $ return $ asSpec
      [
        VL.mark Rule [],
        encodingLayerTwo [],
        selectionLayerTwo []
      ]
      
  ]

-----------------------------------------------------------------------------------
-- Right Diagram 
-----------------------------------------------------------------------------------

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

rightDiagram :: Text -> [(VLProperty, VLSpec)]
rightDiagram bands =
  [
    VL.mark Point [MStroke "transparent"],
    dataFromUrl bands [],
    encodingRight [],
    selectionRight []
  ]
