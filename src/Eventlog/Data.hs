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
import Eventlog.Types (Header, ProfData(..), BucketInfo(..))
import Data.Text (Text)
import Data.List
import Data.Ord
import Data.Word
import Eventlog.Trie

generateJsonValidate :: (ProfData -> IO ()) -> FilePath -> Args -> IO (Header, Value, Value)
generateJsonValidate validate file a = do
  let chunk = if heapProfile a then H.chunk else E.chunk a
  dat@(ProfData h binfo ccMap fs traces) <- chunk file
  validate dat
  let keeps = prune a binfo
      combinedJson = object [
          "samples" .= bandsToVega keeps (bands h (Map.map fst keeps) fs)
        , "traces"  .= tracesToVega traces
        ]
      mdescs = map (((,) <$> shortDescription <*> longDescription) . snd)
                $ sortBy (flip (comparing fst)) $ map snd $ Map.toList keeps
      descs = outputTree ccMap mdescs
  return (h, combinedJson, descs)

generateJson :: FilePath -> Args -> IO (Header, Value, Value)
generateJson = generateJsonValidate (const (return ()))

