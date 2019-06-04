{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Prelude hiding (print, readFile)
import Data.Text.IO (hPutStrLn)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad (forM, forM_, when)
import Data.List (foldl1', nub)
import Data.Tuple (swap)
import Data.Semigroup ((<>))
import System.Exit (exitSuccess)
import System.FilePath (replaceExtension)
import System.IO (withFile, IOMode(WriteMode), stdout)

import Args (args, Args(..), Uniform(..), Sort(..), KeyPlace(..), TitlePlace(..))
import Total (total)
import Prune (prune, cmpName, cmpSize, cmpStdDev)
import Bands (bands, series, bandsToSeries)
import Vega
import Pretty (pretty)
import Print (print, printKey)
import SVG (svg)
import Types (Header(..))
import qualified Events as E
import qualified HeapProf as H
import Graphics.Svg
import Debug.Trace
import Data.Aeson (encodeFile)
import System.FilePath
import Data.Map(keysSet)


testMain :: (Show c, Show b) => (FilePath -> IO (a, [b],[c])) -> [FilePath] -> IO ()
testMain c [f] = do
  (_, fs, ts) <- c f
  mapM_ (putStrLn . show) fs
  mapM_ (putStrLn . show) ts
testMain _ _ = error "One file only"

data Output = Test | JSON | SVG

argsToOutput a =
  if | test a -> doTest a
     | json a -> doJson a
     | otherwise -> doSvg a

doTest :: Args -> IO ()
doTest a =
  let chunk = if eventlog a then E.chunk else H.chunk
  in testMain chunk (files a)

doSvg :: Args -> IO ()
doSvg a = do
  let chunk = if eventlog a then E.chunk else H.chunk
      uniformTime = uniformity a `elem` [Time, Both]
      uniformMemory = uniformity a `elem` [Memory, Both]
      sorting' = case sorting a of
        Name -> cmpName
        Size -> cmpSize
        StdDev -> cmpStdDev
      reversing' = if reversing a then swap else id
      cmp = fst $ reversing' sorting'
      sepkey = case keyPlace a of
        KeyInline -> False
        KeyFile{} -> True
      noTitle = case titlePlace a of
        TitleInline -> False
        TitleFile{} -> True
  labelss <- if not (uniformTime || uniformMemory)
    then forM (files a) $ \file -> do
      (ph, fs, traces) <- chunk file
      let (header, totals) = total ph fs
          keeps = prune cmp (tracePercent a) (bound $ nBands a) totals
          (times, vals) = bands header keeps fs
          ((sticks, vticks), (labels, coords)) = pretty header vals keeps
          output = renderText $ print svg noTitle sepkey (patterned a) header sticks vticks labels traces times coords
      traceEventIO "Writing"
      withFile (replaceExtension file "svg") WriteMode $ \h -> T.hPutStr h output
      traceEventIO "Finishing"
      return $ (header, reverse labels)
    else do
      inputs <- mapM chunk (files a)
      let hts0 = map (\(ph, b, _) -> total ph b) inputs
          (smima, vmima) = foldl1' (\((!smi, !sma), (!vmi, !vma)) ((!smi', !sma'), (!vmi', !vma')) -> ((smi`min`smi', sma`max`sma'), (vmi`min`vmi', vma`max`vma'))) . map (\(h, _) -> (hSampleRange h, hValueRange h)) $ hts0
          hts1 | uniformTime = map (\(h, t) -> (h{ hSampleRange = smima }, t)) hts0
               | otherwise = hts0
          hts | uniformMemory = map (\(h, t) -> (h{ hValueRange = vmima }, t)) hts1
              | otherwise = hts1
      forM (zip3 (files a) inputs hts) $ \(file, (_ph, fs, traces), (header, totals)) -> do
        let keeps = prune cmp (tracePercent a) (bound $ nBands a) totals
            (times, vals) = bands header keeps fs
            ((sticks, vticks), (labels, coords)) = pretty header vals keeps
            outputs = renderText $ print svg noTitle sepkey (patterned a) header sticks vticks labels traces times coords
        withFile (replaceExtension file "svg") WriteMode $ \h -> T.hPutStr h outputs
        return $ (header, reverse labels)
  case keyPlace a of
    KeyFile keyFile -> (if keyFile == "-" then ($ stdout) else withFile keyFile WriteMode) $ \txt -> do
      forM_ (nub $ concatMap snd labelss) $ \label -> do
        let (filename, content) = printKey svg (patterned a) label
        withFile filename WriteMode $ \h -> T.hPutStr h (renderText content)
        hPutStrLn txt (T.pack filename <> " " <> label)
    _ -> return ()
  case titlePlace a of
    TitleFile titleFile -> (if titleFile == "-" then ($ stdout) else withFile titleFile WriteMode) $ \txt -> do
      forM_ (files a `zip` map fst labelss) $ \(file, header) -> do
        let filename = replaceExtension file "svg"
            title = hJob header <> " (" <> hDate header <> ")"
        hPutStrLn txt (T.pack filename <> " " <> title)
    _ -> return ()



main :: IO ()
main = do
  a <- args
  when (null (files a)) exitSuccess
  argsToOutput a

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
    exitSuccess

