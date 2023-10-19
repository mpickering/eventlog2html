
module Eventlog.Rendering.Types where

import Text.Blaze.Html5
import Profiteur.Renderer as Profiteur

data ProfiteurData
  = ProfiteurData
  { pdCssAssets   :: Profiteur.CssAssets
  , pdJsAssets    :: Profiteur.JsAssets
  }

data IncludeTraceData
  = TraceData
  | NoTraceData

type VizID = String

type Tabs = [TabGroup]

data TabGroup = ManyTabs String [Tab]
              | SingleTab Tab

data Tab = Tab { tabName     :: String
               , tabId       :: VizID
               , tabContent  :: Maybe (VizID -> Html)
               , tabDocs     :: Maybe Html
               , tabActive   :: Bool -- ^ Active by default?
               , tabDisabled :: Bool
               }

noDocs :: Maybe Html
noDocs = Nothing
