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
import Eventlog.ByInfoTable
import Text.Blaze.Html

generateJsonValidate :: (ProfData -> IO ()) -> FilePath -> Args -> IO (Header, Value, Maybe Value, Maybe Html)
generateJsonValidate validate file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  dat@(ProfData h binfo ccMap fs traces ipes) <- chunk file
  validate dat
  let keeps = prune a binfo ipes
      combinedJson = object [
          "samples" .= bandsToVega keeps (bands h (Map.map fst keeps) fs)
        , "traces"  .= tracesToVega traces
        ]
      mdescs =
        sortBy (flip (comparing (fst . snd))) $ Map.toList keeps
      -- Only supply the cost centre view in cost centre profiling mode.
      cc_descs = case hHeapProfileType h of
                Just HeapProfBreakdownCostCentre -> Just (outputTree ccMap mdescs)
                _ -> Nothing
  closure_table <- case (,) <$> hHeapProfileType h <*> hProgPath h of
                     Just (HeapProfBreakdownInfoTable, debug_prog) ->
                      Just . renderClosureInfo <$> mkClosureInfo debug_prog keeps ipes
                     _ -> return Nothing
  return (h, combinedJson, cc_descs, closure_table)

generateJson :: FilePath -> Args -> IO (Header, Value, Maybe Value, Maybe Html)
generateJson = generateJsonValidate (const (return ()))

