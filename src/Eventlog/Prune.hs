{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Eventlog.Prune
  ( pruneBands, pruneDetailed
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Eventlog.Types
import Data.Map (Map, fromList, (!), toList)

import Eventlog.Args (Args(..), Sort(..))
import Data.Maybe

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
  cmpSizeAscending, cmpSizeDescending,
  cmpGradientAscending, cmpGradientDescending :: Compare (Bucket, BucketInfo)
cmpNameAscending = comparing fst
cmpNameDescending = flip cmpNameAscending
cmpStdDevAscending = comparing (bucketStddev . snd)
cmpStdDevDescending = flip cmpStdDevAscending
cmpSizeAscending = comparing (bucketTotal . snd)
cmpSizeDescending = flip cmpSizeAscending
cmpGradientAscending = comparing (fmap getGradient . bucketGradient . snd)
 where
   getGradient (_a, b, _r2) = b
cmpGradientDescending = flip cmpGradientAscending

prune :: Int
      -> Args
      -> Map Bucket BucketInfo
      -> Map Bucket (Int, BucketInfo)
prune limit args ts =
  let ccTotals = sortBy cmpSizeDescending
                  (toList ts)
      bands = take limit ccTotals
      ccs = map fst (sortBy (getComparison args) bands)
      res :: [(Bucket, (Int, BucketInfo))]
      res = zipWith (\b k -> (b, (k, ts ! b))) (reverse ccs) [1..]
  in  fromList res

pruneBands, pruneDetailed :: Args -> Map Bucket BucketInfo -> Map Bucket (Int, BucketInfo)
pruneBands as = prune (bound $ nBands as) as
pruneDetailed as = prune (fromMaybe maxBound $ detailedLimit as) as


bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n
