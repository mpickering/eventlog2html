{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Prelude hiding (print, readFile)
import Data.Text.IO (readFile, hPutStr)
import Control.Monad (forM_, when)
import Data.List (foldl1', isPrefixOf, partition)
import Data.Tuple (swap)
import Data.Monoid (Last(..), mconcat)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (replaceExtension)
import System.IO (withFile, IOMode(WriteMode))

import Args (args, Args(..), Uniform(..), Sort(..))
import Total (total)
import Prune (prune, cmpName, cmpSize, cmpStdDev)
import Bands (bands)
import Pretty (pretty)
import Print (print)
import SVG (svg)
import Types (Header(..))

main :: IO ()
main = do
  a <- args
  let uniformTime = uniformity a `elem` [Time, Both]
      uniformMemory = uniformity a `elem` [Memory, Both]
      sorting' = case sorting a of
        Name -> cmpName
        Size -> cmpSize
        StdDev -> cmpStdDev
      reversing' = if reversing a then swap else id
      cmp = fst $ reversing' sorting'
  when (null (files a)) exitSuccess
  if not (uniformTime || uniformMemory)
    then forM_ (files a) $ \file -> do
      input <- readFile file
      let (header, totals) = total input
          keeps = prune cmp (tracePercent a) (bound $ nBands a) totals
          (times, vals) = bands header keeps input
          ((sticks, vticks), (labels, coords)) = pretty header vals keeps
          outputs = print svg (patterned a) header sticks vticks labels times coords
      withFile (replaceExtension file "svg") WriteMode $ \h -> mapM_ (hPutStr h) outputs
    else do
      inputs <- mapM readFile (files a)
      let hts0 = map total inputs
          (smima, vmima) = foldl1' (\((!smi, !sma), (!vmi, !vma)) ((!smi', !sma'), (!vmi', !vma')) -> ((smi`min`smi', sma`max`sma'), (vmi`min`vmi', vma`max`vma'))) . map (\(h, _) -> (hSampleRange h, hValueRange h)) $ hts0
          hts1 | uniformTime = map (\(h, t) -> (h{ hSampleRange = smima }, t)) hts0
               | otherwise = hts0
          hts | uniformMemory = map (\(h, t) -> (h{ hValueRange = vmima }, t)) hts1
              | otherwise = hts1
      forM_ (zip3 (files a) inputs hts) $ \(file, input, (header, totals)) -> do
        let keeps = prune cmp (tracePercent a) (bound $ nBands a) totals
            (times, vals) = bands header keeps input
            ((sticks, vticks), (labels, coords)) = pretty header vals keeps
            outputs = print svg (patterned a) header sticks vticks labels times coords
        withFile (replaceExtension file "svg") WriteMode $ \h -> mapM_ (hPutStr h) outputs

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n
