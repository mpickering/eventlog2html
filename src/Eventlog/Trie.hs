{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Trie where

import Prelude hiding (init, lookup)
import Data.Text (Text)

import Eventlog.Types
import Data.Word
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Trie.Map as Trie
import qualified Data.Trie.Map.Internal as TrieI
import Data.Aeson

outputTree :: Map.Map Word32 CostCentre -> [(Bucket, (Int, BucketInfo))]
           -> Value
outputTree ccMap mdescs =
  let t = Trie.fromList [(k, (i, b, v)) | (Bucket b, (i, BucketInfo v (Just k) _ _)) <- mdescs ]
  in toJSON $ outputTrie ccMap t

outputTrie :: Map.Map Word32 CostCentre -> Trie.TMap Word32 (Int, Text, Text) -> [Value]
outputTrie ccMap (TrieI.TMap (TrieI.Node ni m))  =
    mkNode 0 Nothing "MAIN" ni : outputTrieLoop ccMap 0 m


outputTrieLoop :: Map.Map Word32 CostCentre
               -> Word32
               -> Map.Map Word32 (Trie.TMap Word32 (Int, Text, Text))
               -> [Value]
outputTrieLoop ccMap p cs =
  let go p' (TrieI.TMap (TrieI.Node mv cs')) os
        = mkNode p' (Just p) (label $ ccMap ! p') mv :  outputTrieLoop ccMap p' cs' ++ os
  in Map.foldrWithKey go [] cs

mkNode :: Word32 -> Maybe Word32 -> Text -> Maybe (Int, Text, Text) -> Value
mkNode i mparent n mccs = object $ [ "id" .= i, "name" .= n
                             , "ccs" .= maybe "" (\(_, v, _) -> v) mccs
                             , "c" .= maybe "OTHER" (\(_, _, c) -> c) mccs]
                             ++ ["parent" .= p | Just p <- [mparent] ]



