{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Rendering.Bootstrap where

import Eventlog.Rendering.Types

import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

navbar :: [(Int, Tab VizTab)] -> Html
navbar tabs = do
  H.nav ! class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
    H.a ! class_ "navbar-brand" ! href "#" $ "Navbar"
    H.button ! class_ "navbar-toggler"
             ! type_ "button"
             ! dataToggle "collapse"
             ! dataTarget "#navbarSupportedContent"
             ! ariaControls "navbarSupportedContent"
             ! ariaExpanded "false"
             ! ariaLabel "Toggle navigation"
{-
  <a class="navbar-brand" href="#">Navbar</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
    <span class="navbar-toggler-icon"></span>
  </button>

  <div class="collapse navbar-collapse" id="navbarSupportedContent">
    <ul class="navbar-nav mr-auto">
      <li class="nav-item active">
        <a class="nav-link" href="#">Home <span class="sr-only">(current)</span></a>
      </li>
      <li class="nav-item">
        <a class="nav-link" href="#">Link</a>
      </li>
      <li class="nav-item dropdown">
        <a class="nav-link dropdown-toggle" href="#" role="button" data-toggle="dropdown" aria-expanded="false">
          Dropdown
        </a>
        <div class="dropdown-menu">
          <a class="dropdown-item" href="#">Action</a>
          <a class="dropdown-item" href="#">Another action</a>
          <div class="dropdown-divider"></div>
          <a class="dropdown-item" href="#">Something else here</a>
        </div>
      </li>
      <li class="nav-item">
        <a class="nav-link disabled">Disabled</a>
      </li>
    </ul>
    <form class="form-inline my-2 my-lg-0">
      <input class="form-control mr-sm-2" type="search" placeholder="Search" aria-label="Search">
      <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Search</button>
    </form>
  </div>
</nav>
-}
