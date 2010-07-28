module Process where

import Control.Arrow ((&&&))
import Data.List (foldl', sortBy)
import Data.Map (toList, fromList, difference, elems, findWithDefault)
import Data.Ord (comparing)

import Types

process :: Run -> Info
process hpr =
  let frames = hprFrames hpr
      samples = map hpfSamples frames
      ccTotals = sortBy (flip $ comparing snd) (toList $ hprTotals hpr)
      sizes = map snd ccTotals
      total = sum' sizes
      limit = 0.99 * total
      bigs = takeWhile (< limit) . drop 1 . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take 15 bigs
      ccs = map fst bands
      ccsMap = fromList . map (\c -> (c, ())) $ ccs
      sr = (minimum &&& maximum) . map hpfTime $ frames
      vr = (0, maximum . map (sum . elems) $ samples)
      values = [ (c, [ findWithDefault 0 c m | m <- samples ]) | c <- ccs ]
      traces = map (sum' . elems . (`difference` ccsMap)) samples
  in  Info
      { hpiJob        = hprJob hpr
      , hpiDate       = hprDate hpr
      , hpiSampleUnit = hprSampleUnit hpr
      , hpiValueUnit  = hprValueUnit hpr
      , hpiSampleRange= sr
      , hpiValueRange = vr
      , hpiSamples    = map hpfTime frames
      , hpiValues     = values
      , hpiTrace      = traces
      }

sum' :: [Double] -> Double
sum' = foldl' (+) 0
