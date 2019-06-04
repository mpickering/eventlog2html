{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Vega (bandsToVega, tracesToVega) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray, (!), bounds)
import Data.Array.ST (writeArray, readArray, newArray)
import Data.Array.Unboxed (UArray)
import Data.Map (Map, lookup, size, foldrWithKey)
import Prelude hiding (lookup, lines, words, length)
import Data.Text (Text)
import Types
import Data.HashTable.ST.Basic hiding (lookup)
import Data.Aeson hiding (Series)
import GHC.Generics
import Data.Set (Set, notMember)

data VegaEntry = VegaEntry { x :: Double, y :: Double, k :: Int, c :: Text }
  deriving (Show, ToJSON, Generic)

bandsToVega :: Map Text Int
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [VegaEntry]
bandsToVega ks (ts, vs) =
  let (t1, tn) = bounds ts
      go k v rs = go_1 ++ rs
        where
          go_1 :: [VegaEntry]
          go_1 = flip map [t1 .. tn] $ \t -> VegaEntry (ts ! t) (vs ! (v, t)) v k
  in foldrWithKey go (go "OTHER" 0 []) ks

data VegaTrace = VegaTrace { tx :: Double, desc :: Text }
  deriving (Show, ToJSON, Generic)

tracesToVega :: [Trace] -> [VegaTrace]
tracesToVega = map (\(Trace t d) -> VegaTrace t d)

