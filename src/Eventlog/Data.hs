{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventlog.Data (generateJson) where

import Prelude hiding (print, readFile)
import Data.Aeson (Value(..), (.=), object)
import Data.Tuple (swap)

import Eventlog.Args (Args(..), Sort(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.Prune (prune, cmpName, cmpSize, cmpStdDev)
import Eventlog.Vega
import Eventlog.Types (Header)

generateJson :: FilePath -> Args -> IO (Header, Value)
generateJson file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk
      cmp = fst $ reversing' sorting'
      sorting' = case sorting a of
        Name -> cmpName
        Size -> cmpSize
        StdDev -> cmpStdDev
      reversing' = if reversing a then swap else id
  (h,totals, fs, traces) <- chunk file a
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

