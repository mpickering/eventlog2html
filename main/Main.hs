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
  if | test a -> doTest a
     | json a -> doJson a
     | otherwise -> doHtml a

testMain :: (Show c, Show b) => (FilePath -> IO (a, [b],[c])) -> [FilePath] -> IO ()
testMain c [f] = do
  (_, fs, ts) <- c f
  mapM_ (putStrLn . show) fs
  mapM_ (putStrLn . show) ts
testMain _ _ = error "One file only"

doTest :: Args -> IO ()
doTest a =
  let chunk = if heapProfile a then H.chunk else E.chunk
  in testMain chunk (files a)

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n


doOneJson :: FilePath -> Args -> IO ()
doOneJson file a = do
  (dh, dt) <- generateJson file a
  encodeFile (file <.> "json") dh
  encodeFile (file <.> "json" <.> "traces") dt

doJson :: Args -> IO ()
doJson a = do
  forM_ (files a) $ \file -> do
    doOneJson file a

doHtml :: Args -> IO ()
doHtml a = do
  forM_ (files a) $ \file -> do
    data_json <- generateJson file a
    let html = templateString data_json a vegaJsonText
    let filename2 = file <.> "html"
    writeFile filename2 html
    exitSuccess
