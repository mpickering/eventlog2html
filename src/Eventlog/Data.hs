{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Data (generateJson) where

import Prelude hiding (print, readFile)
import Data.Aeson (Value(..), (.=), object)

import Eventlog.Args (Args(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.Prune (prune)
import Eventlog.Vega
import Eventlog.Types (Header)

generateJson :: FilePath -> Args -> IO (Header, Value)
generateJson file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  (h,totals, fs, traces) <- chunk file
  let keeps = prune a 0 totals
  let combinedJson = object [
          "samples" .= bandsToVega keeps (bands h keeps fs)
        , "traces"  .= tracesToVega traces
        ]
  return (h, combinedJson)

