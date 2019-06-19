{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Prelude hiding (print, readFile)
import Control.Monad (forM_, when)
import Data.Aeson (encodeFile, Value, toJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Tuple (swap)
import Graphics.Vega.VegaLite (fromVL)
import System.Exit (exitSuccess)
import System.FilePath
import Text.Blaze.Html.Renderer.String

import Args (args, Args(..), Sort(..))
import Bands (bands)
import qualified Events as E
import qualified HeapProf as H
import HtmlTemplate
import Prune (prune, cmpName, cmpSize, cmpStdDev)
import Total (total)
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
  let chunk = if eventlog a then E.chunk else H.chunk
  in testMain chunk (files a)

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n

generateJson :: FilePath -> Args -> IO (Value, Value)
generateJson file a = do
  let chunk = if eventlog a then E.chunk else H.chunk
      cmp = fst $ reversing' sorting'
      sorting' = case sorting a of
        Name -> cmpName
        Size -> cmpSize
        StdDev -> cmpStdDev
      reversing' = if reversing a then swap else id
  (ph, fs, traces) <- chunk file
  let (h, totals) = total ph fs
  let keeps = prune cmp 0 (bound $ nBands a) totals
  let dataJson = toJSON (bandsToVega keeps (bands h keeps fs))
      dataTraces =  toJSON (tracesToVega traces)
  return (dataJson, dataTraces)

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
    let vegaspec =  toStrict (encodeToLazyText (fromVL vegaResult))
    let html = renderHtml (template data_json a vegaspec)
    let filename2 = file <.> "html"
    writeFile filename2 html
    exitSuccess
