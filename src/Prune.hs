module Prune (prune) where

import Data.Text (Text)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Map (Map, toList, fromList)

prune :: Map Text Double -> Map Text Int
prune ts =
  let ccTotals = sortBy (flip $ comparing snd) (toList ts)
      sizes = map snd ccTotals
      total = sum' sizes
      limit = 0.99 * total
      bigs = takeWhile (< limit) . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take 15 bigs
      ccs = map fst bands
  in  fromList (reverse ccs `zip` [1 ..])

sum' :: [Double] -> Double
sum' = foldl' (+) 0
