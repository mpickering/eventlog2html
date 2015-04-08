{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Prelude hiding (print, readFile)
import Data.Text.IO (readFile, hPutStr)
import Control.Monad (forM_, when)
import Data.List (foldl1', isPrefixOf, partition)
import Data.Monoid (Last(..), mconcat)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (replaceExtension)
import System.IO (withFile, IOMode(WriteMode))

import Total (total)
import Prune (prune)
import Bands (bands)
import Pretty (pretty)
import Print (print)
import SVG (svg)
import Types (Header(..))

main :: IO ()
main = do
  args <- getArgs
  let (flags, files) = partition (isPrefixOf "--") args
      uniformity "--uniform-scale=none"   = Last (Just (False, False))
      uniformity "--uniform-scale=time"   = Last (Just (True, False))
      uniformity "--uniform-scale=memory" = Last (Just (False, True))
      uniformity "--uniform-scale=both"   = Last (Just (True, True))
      uniformity _                        = Last Nothing
      Last (Just (uniformTime, uniformMemory)) = mconcat (Last (Just (False, False)) : map uniformity flags)
  when (null files) exitSuccess
  if not (uniformTime || uniformMemory)
    then forM_ files $ \file -> do
      input <- readFile file
      let (header, totals) = total input
          keeps = prune totals
          (times, vals) = bands header keeps input
          ((sticks, vticks), (labels, coords)) = pretty header vals keeps
          outputs = print svg header sticks vticks labels times coords
      withFile (replaceExtension file "svg") WriteMode $ \h -> mapM_ (hPutStr h) outputs
    else do
      inputs <- mapM readFile files
      let hts0 = map total inputs
          (smima, vmima) = foldl1' (\((!smi, !sma), (!vmi, !vma)) ((!smi', !sma'), (!vmi', !vma')) -> ((smi`min`smi', sma`max`sma'), (vmi`min`vmi', vma`max`vma'))) . map (\(h, _) -> (hSampleRange h, hValueRange h)) $ hts0
          hts1 | uniformTime = map (\(h, t) -> (h{ hSampleRange = smima }, t)) hts0
               | otherwise = hts0
          hts | uniformMemory = map (\(h, t) -> (h{ hValueRange = vmima }, t)) hts1
              | otherwise = hts1
      forM_ (zip3 files inputs hts) $ \(file, input, (header, totals)) -> do
        let keeps = prune totals
            (times, vals) = bands header keeps input
            ((sticks, vticks), (labels, coords)) = pretty header vals keeps
            outputs = print svg header sticks vticks labels times coords
        withFile (replaceExtension file "svg") WriteMode $ \h -> mapM_ (hPutStr h) outputs
