
module Eventlog.Rendering.Types where

import Text.Blaze.Html5

data IncludeTraceData
  = TraceData
  | NoTraceData

type VizID = Int

data Tab a = Tab { tabName    :: String
                 , tabId      :: String
                 , tabContent :: a
                 }

data VizTab =
  VizTab { vizIdToHtml :: VizID -> Html
         , tabDocs     :: Maybe Html
         }

noDocs :: Maybe Html
noDocs = Nothing
