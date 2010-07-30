module Process (process) where

import Control.Arrow ((&&&))
import Data.List (foldl', sortBy)
import Data.Map (toList, fromList, difference, elems, findWithDefault)
import Data.Ord (comparing)

import Types

process :: Run -> Info
process r =
  let frames = rFrames r
      samples = map fSamples frames
      ccTotals = sortBy (flip $ comparing snd) (toList $ rTotals r)
      sizes = map snd ccTotals
      total = sum' sizes
      limit = 0.99 * total
      bigs = takeWhile (< limit) . drop 1 . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take 15 bigs
      ccs = map fst bands
      ccsMap = fromList . map (\c -> (c, ())) $ ccs
      sr = (minimum &&& maximum) . map fTime $ frames
      vr = (0, maximum . map (sum' . elems) $ samples)
      values = [ (c, [ findWithDefault 0 c m | m <- samples ]) | c <- ccs ]
      traces = map (sum' . elems . (`difference` ccsMap)) samples
  in  Info
      { iJob         = rJob r
      , iDate        = rDate r
      , iSampleUnit  = rSampleUnit r
      , iValueUnit   = rValueUnit r
      , iSampleRange = sr
      , iValueRange  = vr
      , iCount       = rCount r
      , iSamples     = map fTime frames
      , iValues      = values
      , iTrace       = traces
      }

sum' :: [Double] -> Double
sum' = foldl' (+) 0
