{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad
import Data.Aeson (encodeFile, Value, toJSON)
import System.FilePath
import System.Exit

import Eventlog.Args (args, Args(..))
import Eventlog.Bands (bands)
import Eventlog.HtmlTemplate
import Eventlog.Data
import Eventlog.Vega
import Eventlog.VegaTemplate

main :: IO ()
main = do
  a <- args
  when (null (files a)) exitSuccess
  argsToOutput a

argsToOutput :: Args -> IO ()
argsToOutput a@Args{files = files, outputFile = Nothing} =
  if | json a    -> forM_ files $ \file -> doOneJson a file (file <.> "json")
     | otherwise -> forM_ files $ \file -> doOneHtml a file (file <.> "html")
argsToOutput a@Args{files = [fin], outputFile = Just fout} =
  if | json a    -> doOneJson a fin fout
     | otherwise -> doOneHtml a fin fout
argsToOutput _ =
  die "When the -o option is specified, exactly one eventlog file has to be passed."
  
doOneJson :: Args -> FilePath -> FilePath -> IO ()
doOneJson a fin fout = do
  (_, val) <- generateJson fin a
  encodeFile fout val

doOneHtml :: Args -> FilePath -> FilePath -> IO ()
doOneHtml a fin fout = do
  (header, data_json) <- generateJson fin a
  let html = templateString header data_json a
  writeFile fout html

