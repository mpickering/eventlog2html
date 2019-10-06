module Eventlog.Prune
  ( prune
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map.Strict (Map, toList, fromList)
import Eventlog.Types

import Eventlog.Args (Args(..), Sort(..))

type Compare a = a -> a -> Ordering

getComparison :: Args -> Compare (Bucket, (Double, Double))
getComparison Args { sorting = Size,   reversing = False }  = cmpSizeDescending
getComparison Args { sorting = Size,   reversing = True } = cmpSizeAscending
getComparison Args { sorting = StdDev, reversing = False }  = cmpStdDevDescending
getComparison Args { sorting = StdDev, reversing = True } = cmpStdDevAscending
getComparison Args { sorting = Name,   reversing = True }  = cmpNameDescending
getComparison Args { sorting = Name,   reversing = False } = cmpNameAscending

cmpNameAscending, cmpNameDescending,
  cmpStdDevAscending, cmpStdDevDescending,
  cmpSizeAscending, cmpSizeDescending :: Compare (Bucket, (Double, Double))
cmpNameAscending = comparing fst
cmpNameDescending = flip cmpNameAscending
cmpStdDevAscending = comparing (snd . snd)
cmpStdDevDescending = flip cmpStdDevAscending
cmpSizeAscending = comparing (fst . snd)
cmpSizeDescending = flip cmpSizeAscending

prune :: Args -> Map Bucket (Double, Double) -> Map Bucket Int
prune args ts =
  let ccTotals = sortBy cmpSizeDescending (toList ts)
      sizes = map (fst . snd) ccTotals
      limit = sum sizes
      bigs = takeWhile (< limit) . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take (bound $ nBands args) bigs
      ccs = map fst (sortBy (getComparison args) bands)
  in  fromList (reverse ccs `zip` [1 ..])

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n

