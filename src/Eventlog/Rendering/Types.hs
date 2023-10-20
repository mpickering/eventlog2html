{-# LANGUAGE OverloadedStrings #-}

module Eventlog.Rendering.Types where

import Data.Char
import Data.String
import qualified Data.Text as T
import Text.Blaze.Html5

data IncludeTraceData
  = TraceData
  | NoTraceData

-- | Tab IDs must be usable as HTML and Javascript identifiers, so we allow a
-- limited selection of characters. This is enforced only at runtime by
-- 'mkTabID' and the 'IsString' instance, but that seems good enough for now.
newtype TabID = TabID T.Text

instance IsString TabID where
  fromString = mkTabID

mkTabID :: String -> TabID
mkTabID s
    | all valid s = TabID (T.pack s)
    | otherwise   = error $ "mkTabID: invalid tab ID: " ++ s
    where
      valid c = isAscii c && (isAlpha c || isDigit c || c == '_')

tabIDToVizID :: TabID -> T.Text
tabIDToVizID (TabID t) = "vis" <> t

tabIDToNavItemID :: TabID -> T.Text
tabIDToNavItemID (TabID t) = t <> "-tab"

tabIDToTabID :: TabID -> T.Text
tabIDToTabID (TabID t) = t

tabIDToHref :: TabID -> T.Text
tabIDToHref (TabID t) = "#" <> t


data TabGroup = ManyTabs String [Tab]
              | SingleTab Tab

data Tab = Tab { tabName     :: String
               , tabId       :: TabID
               , tabContent  :: Maybe Html
               , tabDocs     :: Maybe Html
               , tabActive   :: Bool -- ^ Active by default?
               , tabDisabled :: Bool
               }

mkTab :: String -> TabID -> Html -> Maybe Html -> Tab
mkTab name id_ content docs = Tab name id_ (Just content) docs False False

mkUnavailableTab :: String -> TabID -> Html -> Tab
mkUnavailableTab name id_ docs = Tab name id_ Nothing (Just docs) False True

mkOptionalTab :: String -> TabID -> (a -> Html) -> Maybe Html -> Html -> Maybe a -> Tab
mkOptionalTab name id_ mk_content docs no_docs mb = case mb of
    Nothing -> mkUnavailableTab name id_ no_docs
    Just v  -> mkTab name id_ (mk_content v) docs

noDocs :: Maybe Html
noDocs = Nothing
