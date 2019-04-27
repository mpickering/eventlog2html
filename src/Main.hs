{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Bands (bands)
import Pretty (pretty)
import Print (print, printKey)
import SVG (svg)
import Types (Header(..))
import qualified Events as E
import qualified HeapProf as H
import Graphics.Svg

testMain :: Show b => (FilePath -> IO (a, [b])) -> [FilePath] -> IO ()
testMain c [f] = do
  (_, fs) <- c f
  mapM_ (putStrLn . show) fs
testMain _ _ = error "One file only"

main :: IO ()
main = do
  a <- args
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
  if test a then testMain chunk (files a)
            else do
  when (null (files a)) exitSuccess
  labelss <- if not (uniformTime || uniformMemory)
    then forM (files a) $ \file -> do
      (ph, fs) <- chunk file
      let (header, totals) = total ph fs
          keeps = prune cmp (tracePercent a) (bound $ nBands a) totals
          (times, vals) = bands header keeps fs
          ((sticks, vticks), (labels, coords)) = pretty header vals keeps
          output = renderText $ print svg noTitle sepkey (patterned a) header sticks vticks labels times coords
      withFile (replaceExtension file "svg") WriteMode $ \h -> T.hPutStr h output
      return $ (header, reverse labels)
    else do
      inputs <- mapM chunk (files a)
      let hts0 = map (uncurry total) inputs
          (smima, vmima) = foldl1' (\((!smi, !sma), (!vmi, !vma)) ((!smi', !sma'), (!vmi', !vma')) -> ((smi`min`smi', sma`max`sma'), (vmi`min`vmi', vma`max`vma'))) . map (\(h, _) -> (hSampleRange h, hValueRange h)) $ hts0
          hts1 | uniformTime = map (\(h, t) -> (h{ hSampleRange = smima }, t)) hts0
               | otherwise = hts0
          hts | uniformMemory = map (\(h, t) -> (h{ hValueRange = vmima }, t)) hts1
              | otherwise = hts1
      forM (zip3 (files a) inputs hts) $ \(file, input, (header, totals)) -> do
        let keeps = prune cmp (tracePercent a) (bound $ nBands a) totals
            (times, vals) = bands header keeps (snd input)
            ((sticks, vticks), (labels, coords)) = pretty header vals keeps
            outputs = renderText $ print svg noTitle sepkey (patterned a) header sticks vticks labels times coords
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

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n
