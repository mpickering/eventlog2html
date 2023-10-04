{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Eventlog.Vega (bandsToVega, tracesToVega, heapToVega) where

import Data.Array.Base ((!), bounds)
import Data.Array.Unboxed (UArray)
import Data.Map (Map,  foldr)
import Prelude hiding (lookup, lines, words, length)
import Data.Text (Text)
import Eventlog.Types
import Data.Aeson hiding (Series)
import GHC.Generics
import Data.Word

data VegaEntry = VegaEntry { x :: Double, y :: Double, k :: Int, c :: Text }
  deriving (Show, ToJSON, Generic)

bandsToVega :: (BucketInfo -> Text)
            -> Map Bucket (Int, BucketInfo)
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [VegaEntry]
bandsToVega bucket_name ks (ts, vs) =
  let (t1, tn) = bounds ts
      go (i, binfo) rs = go_1 ++ rs
        where
          txt = bucket_name binfo

          go_1 :: [VegaEntry]
          go_1 = flip map [t1 .. tn] $ \t -> VegaEntry (ts ! t) (vs ! (i, t)) i txt

      other_binfo = BucketInfo "OTHER" Nothing
                               -- Last two fields currently unused
                               0 0 Nothing
  in Data.Map.foldr go (go (0, other_binfo) []) ks

data VegaTrace = VegaTrace { tx :: Double, desc :: Text }
  deriving (Show, ToJSON, Generic)

tracesToVega :: [Trace] -> [VegaTrace]
tracesToVega = map (\(Trace t d) -> VegaTrace t d)

data VegaHeap = VegaHeap { x :: Double, y :: Word64, c :: Text, k :: Int}
  deriving (Show, ToJSON, Generic)

heapToVega :: HeapInfo -> [VegaHeap]
heapToVega HeapInfo{..} = mk 2 "Heap Size" heapSizeSamples ++ mk 0 "Live Bytes" liveBytesSamples ++ mk 1 "Blocks Size" blocksSizeSamples

  where
    mk k l xs = [VegaHeap t v l k | HeapSample t v <- xs]

