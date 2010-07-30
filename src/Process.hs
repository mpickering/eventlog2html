module Process (process) where

import Control.Arrow (second)
import Data.List (foldl', sortBy, partition)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S

import Types

process :: Run -> Info
process r =
  let frames = rFrames r
      ccTotals = sortBy (flip $ comparing snd) (M.toList $ rTotals r)
      sizes = map snd ccTotals
      total = sum' sizes
      limit = 0.99 * total
      bigs = takeWhile (< limit) . drop 1 . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take 15 bigs
      ccs = map fst bands
      ccsS = S.fromList ccs
      (times, (keep, rest)) = second unzip . unzip . map (second (partition ((`S.member` ccsS) . fst))) $ frames
      smps = map M.fromList keep
      values = [ (c, [ M.findWithDefault 0 c m | m <- smps ]) | c <- ccs ]
      traces = map (sum' . map snd) rest
  in  Info
      { iJob         = rJob r
      , iDate        = rDate r
      , iSampleUnit  = rSampleUnit r
      , iValueUnit   = rValueUnit r
      , iSampleRange = rSampleRange r
      , iValueRange  = rValueRange r
      , iCount       = rCount r
      , iSamples     = times
      , iValues      = values
      , iTrace       = traces
      }

sum' :: [Double] -> Double
sum' = foldl' (+) 0
