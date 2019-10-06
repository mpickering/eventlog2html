{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Vega (bandsToVega, tracesToVega) where

import Data.Array.Base ((!), bounds)
import Data.Array.Unboxed (UArray)
import Data.Map (Map,  foldrWithKey)
import Prelude hiding (lookup, lines, words, length)
import Data.Text (Text)
import Eventlog.Types
import Data.Aeson hiding (Series)
import GHC.Generics

data VegaEntry = VegaEntry { x :: Double, y :: Double, k :: Int, c :: Text }
  deriving (Show, ToJSON, Generic)

bandsToVega :: Map Bucket Int
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [VegaEntry]
bandsToVega ks (ts, vs) =
  let (t1, tn) = bounds ts
      go (Bucket key) v rs = go_1 ++ rs
        where
          go_1 :: [VegaEntry]
          go_1 = flip map [t1 .. tn] $ \t -> VegaEntry (ts ! t) (vs ! (v, t)) v key
  in foldrWithKey go (go (Bucket "OTHER") 0 []) ks

data VegaTrace = VegaTrace { tx :: Double, desc :: Text }
  deriving (Show, ToJSON, Generic)

tracesToVega :: [Trace] -> [VegaTrace]
tracesToVega = map (\(Trace t d) -> VegaTrace t d)

