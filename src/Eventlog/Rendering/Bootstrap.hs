{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Rendering.Bootstrap where

import Eventlog.Rendering.Types

import Control.Monad
import Data.String
import qualified Data.Text as T
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

navbar :: [TabGroup] -> Html
navbar tab_groups = do
  H.ul ! A.id "vizTabs" ! class_ "nav nav-tabs" $ do
    forM_ tab_groups $ \group -> do
      case group of
        SingleTab tab ->
          H.li ! class_ "nav-item" $
            H.a ! A.id (toValue (tabIDToNavItemID (tabId tab)))
                ! class_ (tabClasses tab)
                ! href (toValue (tabIDToHref (tabId tab)))
                ! dataToggle "tab"
                ! dataTarget (toValue (tabIDToHref (tabId tab)))
                $ fromString (tabName tab)
        ManyTabs group_name tabs ->
          H.li ! class_ "nav-item dropdown" $ do
            H.a ! class_ "nav-link dropdown-toggle"
                ! href "#"
                ! dataToggle "dropdown"
                $ fromString group_name
            H.div ! class_ "dropdown-menu" $
              forM_ tabs $ \tab ->
                H.a ! A.id (toValue (tabIDToNavItemID (tabId tab)))
                    ! class_ "dropdown-item"
                    ! href (toValue (tabIDToHref (tabId tab)))
                    ! dataToggle "tab"
                    ! dataTarget (toValue (tabIDToHref (tabId tab)))
                    $ fromString (tabName tab)

tabClasses :: Tab -> AttributeValue
tabClasses tab = toValue $ T.intercalate " " $
  "nav-link" :
  [ "active" | tabActive tab ] ++
  [ "not-available" | tabDisabled tab ]
