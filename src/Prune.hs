module Prune
  ( prune
  , Compare
  , cmpNameAscending
  , cmpSizeAscending
  , cmpStdDevAscending
  ) where

import Data.Text (Text)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Map.Strict (Map, toList, fromList)

type Compare a = a -> a -> Ordering

cmpNameAscending :: Compare (Text, (Double, Double))
cmpNameAscending = comparing fst

cmpStdDevAscending :: Compare (Text, (Double, Double))
cmpStdDevAscending = comparing (snd . snd)

cmpSizeAscending :: Compare (Text, (Double, Double))
cmpSizeAscending = comparing (fst . snd)

prune :: Compare (Text, (Double, Double)) -> Double -> Int -> Map Text (Double, Double) -> Map Text Int
prune cmp tracePercent nBands ts =
  let ccTotals = sortBy (flip cmpSizeAscending) (toList ts)
      sizes = map (fst . snd) ccTotals
      total = sum sizes
      limit = if tracePercent == 0 then total
                                   else (1 - tracePercent / 100) * total
      bigs = takeWhile (< limit) . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take nBands bigs
      ccs = map fst (sortBy cmp bands)
  in  fromList (reverse ccs `zip` [1 ..])

