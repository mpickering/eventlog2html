{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import Data.Aeson (encodeFile)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding)
import GitHash (tGitInfoCwdTry, giHash, giBranch)
import System.FilePath
import System.Exit
import System.IO

import Eventlog.Args (args, Args(..), Option(..))
import Eventlog.HtmlTemplate
import Eventlog.Data
import Eventlog.Types
import Paths_eventlog2html (version)

main :: IO ()
main = do
  -- This fixes a problem for Windows users: https://serokell.io/blog/haskell-with-utf8
  setLocaleEncoding utf8
  a <- args
  dispatch a


dispatch :: Option -> IO ()
dispatch ShowVersion = printVersion
dispatch (Run a) = do
  when (null (files a)) exitSuccess
  argsToOutput a

printVersion :: IO ()
printVersion = do
  putStrLn $ "eventlog2html Version: " <> showVersion version
  -- We try to get information about the git repository we're building
  -- from. The .git folder might not be available when building from
  -- Hackage.
  let eitherGi = $$tGitInfoCwdTry
  case eitherGi of
    Left _ -> pure ()
    Right gi -> do
      putStrLn $ "Git Commit:            " <> giHash gi
      putStrLn $ "Git Branch:            " <> giBranch gi

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
  Just (HeapProfileData val _ _) <- eventlogHeapProfile <$> generateJson fin a
  encodeFile fout val

doOneHtml :: Args -> FilePath -> FilePath -> IO ()
doOneHtml a fin fout = do
  prof_type <- generateJsonValidate checkTraces fin a
  let html = templateString prof_type a
  writeFile fout html
  where
    checkTraces :: ProfData -> IO ()
    checkTraces dat =
      if length (profTraces dat) > 1000
        then hPutStrLn stderr
              "More than 1000 traces, consider reducing using -i or -x"
        else return ()

