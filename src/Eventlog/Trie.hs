{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Eventlog.Trie where

import Prelude hiding (init, lookup)
import Data.Text (Text, pack)

import Eventlog.Types
import Data.Word
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Trie.Map as Trie
import qualified Data.Trie.Map.Internal as TrieI
import Data.Aeson
import Control.Monad.State

outputTree :: Map.Map Word32 CostCentre -> [(Bucket, (Int, BucketInfo))]
           -> Value
outputTree ccMap mdescs =
  let t = Trie.fromList [(k, (i, b, v)) | (Bucket b, (i, BucketInfo v (Just k) _ _)) <- mdescs ]
  in toJSON $ outputTrie ccMap t

outputTrie :: Map.Map Word32 CostCentre -> Trie.TMap Word32 (Int, Text, Text) -> [Value]
outputTrie ccMap (TrieI.TMap (TrieI.Node ni m))  =
    mkNode "TOP" Nothing "MAIN" ni : flip evalState 0 (outputTrieLoop ccMap "TOP" m)

newLabel :: Word32 -> State Int Text
newLabel n = do
  l <- get
  modify (+1)
  return (pack (show l ++ "-" ++ show n))


outputTrieLoop :: Map.Map Word32 CostCentre
               -> Text
               -> Map.Map Word32 (Trie.TMap Word32 (Int, Text, Text))
               -> State Int [Value]
outputTrieLoop ccMap p cs =
  let go p' (TrieI.TMap (TrieI.Node mv cs')) rest = do
        nid <- newLabel p'
        let CC{..} = ccMap ! p'
        let n = mkNode nid (Just p) (label <> ":" <> loc) mv
        rs <- outputTrieLoop ccMap nid cs'
        os <- rest
        return (n : rs ++ os)
  in Map.foldrWithKey go (return []) cs

mkNode :: Text -> Maybe Text -> Text -> Maybe (Int, Text, Text) -> Value
mkNode id_string mparent n mccs = object $ [ "id" .= id_string, "name" .= n
                             , "ccs" .= maybe "" (\(_, v, _) -> v) mccs
                             , "c" .= maybe "OTHER" (\(_, _, c) -> c) mccs]
                             ++ ["parent" .= p | Just p <- [mparent] ]



