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
import Eventlog.Detailed
import Text.Blaze.Html

generateJsonValidate :: (ProfData -> IO ()) -> FilePath -> Args -> IO (Header, Value, Maybe Value, Maybe Html)
generateJsonValidate validate file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  dat@(ProfData h binfo ccMap fs traces ipes) <- chunk file
  validate dat
  let keeps = prune a binfo
      bs = bands h (Map.map fst keeps) fs
      combinedJson = object [
          "samples" .= bandsToVega keeps bs
        , "traces"  .= tracesToVega traces
        ]
      mdescs =
        sortBy (flip (comparing (fst . snd))) $ Map.toList keeps
      -- Only supply the cost centre view in cost centre profiling mode.
      cc_descs = case hHeapProfileType h of
                Just HeapProfBreakdownCostCentre -> Just (outputTree ccMap mdescs)
                _ -> Nothing

  let use_ipes = case hHeapProfileType h of
                   Just HeapProfBreakdownInfoTable -> Just ipes
                   _ -> Nothing
      (_, desc_buckets) = Map.mapAccum (\n b -> (n + 1, (n, b))) 0 binfo
      bs' = bands h (Map.map fst desc_buckets) fs
      closure_table = renderClosureInfo bs' use_ipes desc_buckets
  return (h, combinedJson, cc_descs, Just closure_table)

generateJson :: FilePath -> Args -> IO (Header, Value, Maybe Value, Maybe Html)
generateJson = generateJsonValidate (const (return ()))

