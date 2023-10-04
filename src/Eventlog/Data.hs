{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Eventlog.Data
  ( generateJson
  , generateJsonValidate
  , generateJsonData
  , EventlogType(..)
  , HeapProfileData(..)
  , TickyProfileData(..)
  ) where

import Prelude hiding (readFile)
import Data.Aeson ((.=), object)
import qualified Data.Map as Map
import Data.Maybe

import Eventlog.Args (Args(..))
import Eventlog.Bands (bands)
import qualified Eventlog.Events as E
import qualified Eventlog.HeapProf as H
import Eventlog.Prune
import Eventlog.Vega
import Eventlog.Types
import Data.List
import Data.Ord
import Eventlog.Trie
import Eventlog.Detailed
import Eventlog.Ticky

generateJsonData :: Args -> ProfData -> HeapProfileData
generateJsonData a (ProfData h binfo ccMap fs traces heap_info ipes _ticky_counter _ticky_samples _total_allocs) =
  let keeps = pruneBands a binfo
      bs = bands h (Map.map fst keeps) fs
      combinedJson = object [
          "samples" .= bandsToVega bucket_desc keeps bs
        , "traces"  .= tracesToVega traces
        , "heap"    .= heapToVega heap_info
        ]
      mdescs =
        sortBy (flip (comparing (fst . snd))) $ Map.toList keeps
      -- Only supply the cost centre view in cost centre profiling mode.
      cc_descs = case hHeapProfileType h of
                Just HeapProfBreakdownCostCentre -> Just (outputTree ccMap mdescs)
                _ -> Nothing

      use_ipes = case hHeapProfileType h of
                   Just HeapProfBreakdownInfoTable -> Just ipes
                   _ -> Nothing

      -- If we have IPE info, try to translate info table pointers to names
      bucket_desc bucket_info = fromMaybe desc $ do
          ipe_map <- use_ipes
          iptr <- toItblPointer_maybe (Bucket desc)
          itl <- Map.lookup iptr ipe_map
          pure (itlName itl <> " (" <> desc <> ")")
        where
          desc = shortDescription bucket_info

      desc_buckets = pruneDetailed a binfo
      bs' = bands h (Map.map fst desc_buckets) fs
      closure_table =
        case detailedLimit a of
          Just 0 ->  Nothing
          _ | null desc_buckets -> Nothing
          _ -> Just (renderClosureInfo bs' use_ipes desc_buckets)
  in HeapProfileData combinedJson cc_descs closure_table

generateJson :: FilePath -> Args -> IO EventlogType
generateJson = generateJsonValidate (const (return ()))

generateJsonValidate :: (ProfData -> IO ()) -> FilePath
                     -> Args -> IO EventlogType
generateJsonValidate validate file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  dat <- chunk file
  validate dat
  pure $ EventlogType (profHeader dat)
                      (Just (generateJsonData a dat))
                      (if not (null (profTickySamples dat)) then Just (generateTickyData dat) else Nothing)
                      -- If there are any ticky samples then generate a ticky profile

generateTickyData :: ProfData -> TickyProfileData
generateTickyData dat =
      let (percen, html) = renderTicky (profTotalAllocations dat) (profTickyCounters dat) (profItl dat) (profTickySamples dat)
      in TickyProfileData (profTotalAllocations dat) percen html
