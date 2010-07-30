module Prune (prune) where

import Data.ByteString.Char8 (ByteString)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Map (Map, toList, fromList)

prune :: Map ByteString Double -> Map ByteString Int
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
