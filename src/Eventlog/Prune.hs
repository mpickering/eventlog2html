module Eventlog.Prune
  ( prune
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map.Strict (Map, toList, fromList, (!))
import Eventlog.Types

import Eventlog.Args (Args(..), Sort(..))

type Compare a = a -> a -> Ordering

getComparison :: Args -> Compare (Bucket, BucketInfo)
getComparison Args { sorting = Size,   reversing = False }  = cmpSizeDescending
getComparison Args { sorting = Size,   reversing = True } = cmpSizeAscending
getComparison Args { sorting = StdDev, reversing = False }  = cmpStdDevDescending
getComparison Args { sorting = StdDev, reversing = True } = cmpStdDevAscending
getComparison Args { sorting = Name,   reversing = True }  = cmpNameDescending
getComparison Args { sorting = Name,   reversing = False } = cmpNameAscending
getComparison Args { sorting = Gradient,   reversing = True }  = cmpGradientAscending
getComparison Args { sorting = Gradient,   reversing = False } = cmpGradientDescending

cmpNameAscending, cmpNameDescending,
  cmpStdDevAscending, cmpStdDevDescending,
  cmpSizeAscending, cmpSizeDescending :: Compare (Bucket, BucketInfo)
cmpNameAscending = comparing fst
cmpNameDescending = flip cmpNameAscending
cmpStdDevAscending = comparing (bucketStddev . snd)
cmpStdDevDescending = flip cmpStdDevAscending
cmpSizeAscending = comparing (bucketTotal . snd)
cmpSizeDescending = flip cmpSizeAscending
cmpGradientAscending = comparing (bucketGradient . snd)
cmpGradientDescending = flip cmpGradientAscending

prune :: Args -> Map Bucket BucketInfo
              -> Map Bucket (Int, BucketInfo)
prune args ts =
  let ccTotals = sortBy cmpSizeDescending (toList ts)
      sizes = map (bucketTotal . snd) ccTotals
      limit = sum sizes
      bigs = takeWhile (< limit) . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take (bound $ nBands args) bigs
      ccs = map fst (sortBy (getComparison args) bands)
      res :: [(Bucket, (Int, BucketInfo))]
      res = zipWith (\b k -> (b, (k, ts ! b))) (reverse ccs) [1..]
  in  fromList res

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n
