{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Data (generateJson, generateJsonValidate, generateJsonData ) where

import Prelude hiding (readFile)
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Map as Map

import Eventlog.Args (Args(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.Prune
import Eventlog.Vega
import Eventlog.Types (Header(..), ProfData(..), HeapProfBreakdown(..))
import Data.List
import Data.Ord
import Eventlog.Trie
import Eventlog.Detailed
import Text.Blaze.Html

generateJsonData :: Args -> ProfData -> IO (Header, Value, Maybe Value, Maybe Html)
generateJsonData a (ProfData h binfo ccMap fs traces _ipes) = do
  let keeps = pruneBands a binfo
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
                   --Just HeapProfBreakdownInfoTable -> Just ipes
                   _ -> Nothing
      desc_buckets = pruneDetailed a binfo
      bs' = bands h (Map.map fst desc_buckets) fs
      closure_table =
        case detailedLimit a of
          Just 0 ->  Nothing
          _ -> Just (renderClosureInfo bs' use_ipes desc_buckets)
  return (h, combinedJson, cc_descs, closure_table)

generateJson :: FilePath -> Args -> IO (Header, Value, Maybe Value, Maybe Html)
generateJson = generateJsonValidate (const (return ()))

generateJsonValidate :: (ProfData -> IO ()) -> FilePath
                     -> Args -> IO (Header, Value, Maybe Value, Maybe Html)
generateJsonValidate validate file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  dat <- chunk file
  validate dat
  generateJsonData a dat

