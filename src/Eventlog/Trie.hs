{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Trie where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)
import qualified Data.Text as T
import Data.Text (Text)

import Eventlog.Types
import Eventlog.Total
import Eventlog.Args (Args(..))
import Data.List
import Data.Function
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Vector.Unboxed (Vector, (!?), toList)
import Data.Maybe
import Data.Version
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char
import System.IO
import qualified Data.Trie.Map as Trie
import qualified Data.Trie.Map.Internal as TrieI
import Data.Map.Merge.Lazy
import Data.Functor.Identity
import Data.Aeson

outputTree :: Map.Map Word32 CostCentre -> [(Bucket, (Int, BucketInfo))]
           -> Value
outputTree ccMap mdescs =
  let t = Trie.fromList [(k, (t, b, v)) | (Bucket b, (t, BucketInfo v (Just k) _ _)) <- mdescs ]
  in toJSON $ outputTrie ccMap t

outputTrie :: Map.Map Word32 CostCentre -> Trie.TMap Word32 (Int, Text, Text) -> [Value]
outputTrie ccMap (TrieI.TMap (TrieI.Node _ m))  =
  object [ "id" .= (0 :: Int), "name" .= ("MAIN" :: String), "ccs" .= ("0" :: String)
         ]
    : outputTrieLoop ccMap 0 m


outputTrieLoop :: Map.Map Word32 CostCentre
               -> Word32
               -> Map.Map Word32 (Trie.TMap Word32 (Int, Text, Text))
               -> [Value]
outputTrieLoop ccMap p cs =
  let go p' (TrieI.TMap (TrieI.Node mv cs')) os
        = mkNode p' p (label $ ccMap ! p') mv :  outputTrieLoop ccMap p' cs' ++ os
  in Map.foldrWithKey go [] cs

mkNode :: Word32 -> Word32 -> Text -> Maybe (Int, Text, Text) -> Value
mkNode i p n mccs = object $ [ "id" .= i, "name" .= n, "parent" .= p
                             , "ccs" .= maybe "" (\(_, v, _) -> v) mccs
                             ] ++ [ "c" .= c | Just (_, _, c) <- [mccs] ]



