module Eventlog.Prune
  ( prune
  ) where

import Data.Text (Text)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Map.Strict (Map, toList, fromList)

import Eventlog.Args (Args(..), Sort(..))

type Compare a = a -> a -> Ordering

getComparison :: Args -> Compare (Text, (Double, Double))
getComparison Args { sorting = Size,   reversing = True }  = cmpSizeDescending
getComparison Args { sorting = Size,   reversing = False } = cmpSizeAscending
getComparison Args { sorting = StdDev, reversing = True }  = cmpStdDevDescending
getComparison Args { sorting = StdDev, reversing = False } = cmpStdDevAscending
getComparison Args { sorting = Name,   reversing = True }  = cmpNameDescending
getComparison Args { sorting = Name,   reversing = False } = cmpNameAscending

cmpNameAscending, cmpNameDescending,
  cmpStdDevAscending, cmpStdDevDescending,
  cmpSizeAscending, cmpSizeDescending :: Compare (Text, (Double, Double))
cmpNameAscending = comparing fst
cmpNameDescending = flip cmpNameAscending
cmpStdDevAscending = comparing (snd . snd)
cmpStdDevDescending = flip cmpStdDevAscending
cmpSizeAscending = comparing (fst . snd)
cmpSizeDescending = flip cmpSizeAscending

prune :: Args -> Double -> Map Text (Double, Double) -> Map Text Int
prune args percent ts =
  let ccTotals = sortBy cmpSizeDescending (toList ts)
      sizes = map (fst . snd) ccTotals
      total = sum' sizes
      limit = if percent == 0 then total
                                   else (1 - percent / 100) * total
      bigs = takeWhile (< limit) . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take (bound $ nBands args) bigs
      ccs = map fst (sortBy (getComparison args) bands)
  in  fromList (reverse ccs `zip` [1 ..])

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n

sum' :: [Double] -> Double
sum' = foldl' (+) 0
