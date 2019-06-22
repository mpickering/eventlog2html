{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad
import Data.Aeson (encodeFile, Value, toJSON)
import System.FilePath
import System.Exit

import Args (args, Args(..), Sort(..))
import Bands (bands)
import qualified Events as E
import qualified HeapProf as H
import HtmlTemplate
import Prune (prune, cmpName, cmpSize, cmpStdDev)
import Total (total)
import Data
import Vega
import VegaTemplate

main :: IO ()
main = do
  a <- args
  when (null (files a)) exitSuccess
  argsToOutput a

argsToOutput :: Args -> IO ()
argsToOutput a =
  if | json a -> doJson a
     | otherwise -> doHtml a

doJson :: Args -> IO ()
doJson a = do
  forM_ (files a) $ \file -> do
    (_, val) <- generateJson file a
    encodeFile (file <.> "json") val

doHtml :: Args -> IO ()
doHtml a = do
  forM_ (files a) $ \file -> do
    (header, data_json) <- generateJson file a
    let html = templateString header data_json a
    let filename2 = file <.> "html"
    writeFile filename2 html
    exitSuccess
