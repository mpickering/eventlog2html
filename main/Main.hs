{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Monad
import Control.Arrow ((&&&))
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

doOneJson :: Args -> FilePath -> FilePath -> IO ()
doOneJson a fin fout = do
  (_, val) <- generateJson fin a
  encodeFile fout val

doJson :: Args -> IO ()
doJson a@((files &&& outputFile) -> (fs, Nothing)) =
  forM_ fs $ \file -> doOneJson a file (file <.> "json")
doJson a@((files &&& outputFile) -> ([fin], Just fout)) =
  doOneJson a fin fout
doJson _ =
  die "When the -o option is specified, exactly one eventlog file has to be passed."

doOneHtml :: Args -> FilePath -> FilePath -> IO ()
doOneHtml a fin fout = do
  (header, data_json) <- generateJson fin a
  let html = templateString header data_json a
  writeFile fout html

doHtml :: Args -> IO ()
doHtml a@((files &&& outputFile) -> (fs, Nothing)) =
  forM_ fs $ \file -> doOneHtml a file (file <.> "html")
doHtml a@((files &&& outputFile) -> ([fin], Just fout)) =
  doOneHtml a fin fout
doHtml _ =
  die "When the -o option is specified, exactly one eventlog file has to be passed."

