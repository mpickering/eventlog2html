{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad
import Data.Aeson (encodeFile)
import System.FilePath
import System.Exit
import System.IO

import Eventlog.Args (args, Args(..))
import Eventlog.HtmlTemplate
import Eventlog.Data
import Eventlog.Types

main :: IO ()
main = do
  a <- args
  when (null (files a)) exitSuccess
  argsToOutput a


argsToOutput :: Args -> IO ()
argsToOutput a@Args{files = files', outputFile = Nothing} =
  if | json a    -> forM_ files' $ \file -> doOneJson a file (file <.> "json")
     | otherwise -> forM_ files' $ \file -> doOneHtml a file (file <.> "html")
argsToOutput a@Args{files = [fin], outputFile = Just fout} =
  if | json a    -> doOneJson a fin fout
     | otherwise -> doOneHtml a fin fout
argsToOutput _ =
  die "When the -o option is specified, exactly one eventlog file has to be passed."

doOneJson :: Args -> FilePath -> FilePath -> IO ()
doOneJson a fin fout = do
  (_, val, _) <- generateJson fin a
  encodeFile fout val

doOneHtml :: Args -> FilePath -> FilePath -> IO ()
doOneHtml a fin fout = do
  (header, data_json, descs) <- generateJsonValidate checkTraces fin a
  let html = templateString header data_json descs a
  writeFile fout html
  where
    checkTraces :: ProfData -> IO ()
    checkTraces (ProfData _ _ _ _ ts _) =
      if length ts > 1000
        then hPutStrLn stderr
              "More than 1000 traces, consider reducing using -i or -x"
        else return ()

