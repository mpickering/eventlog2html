{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Prelude hiding (print, readFile)
import Control.Monad (forM_, when)
import Data.Aeson (encodeFile)
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
     | otherwise -> doTest a
     
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

doJson :: Args -> IO ()
doJson a = do
  let chunk = if eventlog a then E.chunk else H.chunk
      cmp = fst $ reversing' sorting'
      sorting' = case sorting a of
        Name -> cmpName
        Size -> cmpSize
        StdDev -> cmpStdDev
      reversing' = if reversing a then swap else id
  forM_ (files a) $ \file -> do
    (ph, fs, traces) <- chunk file
    let (h, totals) = total ph fs
    let keeps = prune cmp 0 (bound $ nBands a) totals
    encodeFile (file <.> "json") (bandsToVega keeps (bands h keeps fs))
    encodeFile (file <.> "json" <.> "traces") (tracesToVega traces)
    --let mybands = encode (bandsToVega keeps (bands h keeps fs))
    --let mytraces = (tracesToVega traces)
    let vegaspec =  toStrict (encodeToLazyText (fromVL (vegaResult (T.pack (file <.> "json"))(T.pack (file <.> "json" <.> "traces")))))
    let html = renderHtml (template (encloseScript vegaspec))
    let filename2 = file <.> "html"
    writeFile filename2 html
    exitSuccess
