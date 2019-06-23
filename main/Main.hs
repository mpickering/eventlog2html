{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad
import Data.Aeson (encodeFile, Value, toJSON)
import System.FilePath
import System.Exit

import Eventlog.Args (args, Args(..), Sort(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.HtmlTemplate
import Eventlog.Prune (prune, cmpName, cmpSize, cmpStdDev)
import Eventlog.Total (total)
import Eventlog.Data
import Eventlog.Vega
import Eventlog.VegaTemplate

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
