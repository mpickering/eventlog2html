module Process where

import Control.Arrow((&&&))
import Data.List (sortBy)
import Data.Map (unionsWith, toList, fromList, difference, elems, findWithDefault)
import Data.Ord (comparing)

import Types

process :: Run -> Info
process hpr =
  let frames = hprFrames hpr
      samples = map hpfSamples frames
      ccTotals = sortBy (flip $ comparing snd) . toList . unionsWith (+) $ samples
      sizes = map snd ccTotals
      total = sum sizes
      limit = 0.99 * total
      bigs = takeWhile (< limit) . drop 1 . scanl (+) 0 $ sizes
      bands = take (15 `max` length bigs) ccTotals
      ccs = map fst bands
      ccsMap = fromList . map (\c -> (c, ())) $ ccs
      sr = (minimum &&& maximum) . map hpfTime $ frames
      vr = (0, maximum . map (sum . elems) $ samples)
      values = [ (c, [ findWithDefault 0 c m | m <- samples ]) | c <- ccs ]
      traces = map (sum . elems . (`difference` ccsMap)) samples
  in  Info
      { hpiJob        = hprJob hpr
      , hpiDate       = hprDate hpr
      , hpiSampleUnit = "samples" -- hprSampleUnit hpr
      , hpiValueUnit  = hprValueUnit hpr
      , hpiSampleRange= (0, fromIntegral $ length frames - 1) -- sr
      , hpiValueRange = vr
      , hpiSamples    = [0 .. fromIntegral $ length frames - 1] -- map hpfTime frames
      , hpiValues     = values
      , hpiTrace      = traces
      }
