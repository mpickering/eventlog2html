{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Rendering.Bootstrap where

import Eventlog.Rendering.Types

import Control.Monad
import Data.String
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (attribute)

dataToggle :: AttributeValue -> Attribute
dataToggle = attribute "data-toggle" " data-toggle=\""
{-# INLINE dataToggle #-}

dataTarget :: AttributeValue -> Attribute
dataTarget = attribute "data-target" " data-target=\""
{-# INLINE dataTarget #-}

ariaControls :: AttributeValue -> Attribute
ariaControls = attribute "aria-controls" " aria-controls=\""
{-# INLINE ariaControls #-}

ariaExpanded :: AttributeValue -> Attribute
ariaExpanded = attribute "aria-expanded" " aria-expanded=\""
{-# INLINE ariaExpanded #-}

ariaLabel :: AttributeValue -> Attribute
ariaLabel = attribute "aria-label" " aria-label=\""
{-# INLINE ariaLabel #-}

navbar :: [(Int, Tab VizTab)] -> Html
navbar tabs = do
  H.ul ! A.id "vizTabs" ! class_ "nav nav-tabs" $ do
    forM_ tabs $ \(n, tab) -> do
      let status = if n == 1 then "active" else mempty
      H.li ! class_ "nav-item" $
        H.a ! A.id (fromString $ tabId tab <> "-tab")
            ! class_ ("nav-link " <> status)
            ! href ("#" <> fromString (tabId tab))
            ! dataToggle "tab"
            ! dataTarget (toValue $ "#" <> tabId tab)
            $ fromString (tabName tab)
