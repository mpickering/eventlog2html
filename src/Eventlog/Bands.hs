{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Bands (bands, series, bandsToSeries) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray, (!), bounds)
import Data.Array.ST (writeArray, readArray, newArray)
import Data.Array.Unboxed (UArray)
import Data.Map (Map, lookup, size, foldrWithKey)
import Prelude hiding (lookup, lines, words, length)
import Eventlog.Types
import Data.HashTable.ST.Basic hiding (lookup)
import Data.Aeson hiding (Series)
import GHC.Generics
import Data.Set (Set, notMember)

bands :: Header -> Map Bucket Int -> [Frame] -> (UArray Int Double, UArray (Int, Int) Double)
bands h bs frames = runST $ do
  times <- newArray (1, hCount h) 0
  vals  <- newArray ((-1,1), (size bs, hCount h)) 0
  forM_ (zip [1 ..] frames) $ \(i, (Frame t ss)) -> do
    writeArray times i t
    forM_ ss $ \(Sample k v) -> do
      case k `lookup` bs of
        Nothing -> writeArray vals (0, i) . (+ v) =<< readArray vals (0, i)
        Just b  -> writeArray vals (b, i) v
  times' <- unsafeFreezeSTUArray times
  vals'  <- unsafeFreezeSTUArray vals
  return (times', vals')

bandsToSeries :: Map Bucket Int -> (UArray Int Double, UArray (Int, Int) Double)
              -> [Series]
bandsToSeries ks (ts, vs) =
  let (t1, tn) = bounds ts
      go k v rs = Series k go_1 : rs
        where
          go_1 :: [(Double, Double)]
          go_1 = flip map [t1 .. tn] $ \t -> (ts ! t, vs ! (v, t))
  in foldrWithKey go (go (Bucket "OTHER") 0 []) ks

data Series = Series { key :: Bucket, values :: [(Double, Double)] }
  deriving (Show, ToJSON, Generic)

series :: Set Bucket -> [Frame] ->  [Series]
series ks fs = runST $ do
  m <- new
  forM_ (reverse fs) $ \(Frame t s) ->
    forM_ s $ \(Sample k v) -> do
      let ins _ | notMember k ks = (Nothing, ())
          ins Nothing = (Just [(t, v)]  , ())
          ins (Just ss) = (Just ((t,v) : ss), ())
      mutate m k ins
  foldM (\r (k,v) -> return (Series k v : r)) [] m
