{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Data (generateJson, generateJsonValidate ) where

import Prelude hiding (readFile)
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Map as Map

import Eventlog.Args (Args(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.Prune (prune)
import Eventlog.Vega
import Eventlog.Types (Header(..), ProfData(..), HeapProfBreakdown(..))
import Data.List
import Data.Ord
import Eventlog.Trie

generateJsonValidate :: (ProfData -> IO ()) -> FilePath -> Args -> IO (Header, Value, Maybe Value)
generateJsonValidate validate file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  dat@(ProfData h binfo ccMap fs traces) <- chunk file
  validate dat
  let keeps = prune a binfo
      combinedJson = object [
          "samples" .= bandsToVega keeps (bands h (Map.map fst keeps) fs)
        , "traces"  .= tracesToVega traces
        ]
      mdescs =
        sortBy (flip (comparing (fst . snd))) $ Map.toList keeps
      -- Only supply the cost centre view in cost centre profiling mode.
      descs = case hHeapProfileType h of
                Just HeapProfBreakdownCostCentre -> Just (outputTree ccMap mdescs)
                _ -> Nothing
  return (h, combinedJson, descs)

generateJson :: FilePath -> Args -> IO (Header, Value, Maybe Value)
generateJson = generateJsonValidate (const (return ()))

