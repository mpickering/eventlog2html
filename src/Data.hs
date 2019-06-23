{-# LANGUAGE OverloadedStrings #-}
module Data (generateJson) where

import Prelude hiding (print, readFile)
import Data.Aeson (Value(..), toJSON, (.=), object)
import Data.Tuple (swap)
import Data.Text (Text)

import Args (Args(..), Sort(..))
import Bands (bands)
import qualified Events as E
import qualified HeapProf as H
import Prune (prune, Compare, cmpNameAscending, cmpSizeAscending, cmpStdDevAscending)
import Total (total)
import Vega
import Types (Header)

-- | Select the correct comparison function from the arguments
-- specified at the command line.
selectComparison :: Args -> Compare (Text, (Double, Double))
selectComparison a =
  let
    sorting' = case sorting a of
                 Name -> cmpNameAscending
                 Size -> cmpSizeAscending
                 StdDev -> cmpStdDevAscending
  in if reversing a then flip sorting' else sorting'

generateJson :: FilePath -> Args -> IO (Header, Value)
generateJson file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk
      cmp = selectComparison a
  (h, fs, traces) <- chunk file
  let totals = total fs
  let keeps = prune cmp 0 (bound $ nBands a) totals
  let combinedJson = object [
          "samples" .= bandsToVega keeps (bands h keeps fs)
        , "traces"  .= tracesToVega traces
        ]
  return (h, combinedJson)

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n

