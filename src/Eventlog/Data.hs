{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventlog.Data (generateJson) where

import Prelude hiding (print, readFile)
import Data.Aeson (Value(..), (.=), object)
import Data.Tuple (swap)
import Data.Text (isInfixOf, pack)

import Eventlog.Args (Args(..), Sort(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.Prune (prune, cmpName, cmpSize, cmpStdDev)
import Eventlog.Vega
import Eventlog.Types (Header, Trace(..))

filterTraces :: String -> [Trace] -> [Trace]
filterTraces str = filter prop
  where
    prop (Trace _ trc) = pack str `isInfixOf` trc
  
generateJson :: FilePath -> Args -> IO (Header, Value)
generateJson file a@(Args { filterStr }) = do
  let chunk = if heapProfile a then H.chunk else E.chunk
      cmp = fst $ reversing' sorting'
      sorting' = case sorting a of
        Name -> cmpName
        Size -> cmpSize
        StdDev -> cmpStdDev
      reversing' = if reversing a then swap else id
  (h,totals, fs, traces) <- chunk file
  let keeps = prune cmp 0 (bound $ nBands a) totals
  let combinedJson = object [
          "samples" .= bandsToVega keeps (bands h keeps fs)
        , "traces"  .= tracesToVega (maybe id filterTraces filterStr traces)
        ]
  return (h, combinedJson)

bound :: Int -> Int
bound n
  | n <= 0 = maxBound
  | otherwise = n

