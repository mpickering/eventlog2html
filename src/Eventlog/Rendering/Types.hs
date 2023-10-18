
module Eventlog.Rendering.Types where

import Text.Blaze.Html5

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
