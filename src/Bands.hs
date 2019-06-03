{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Bands (bands, series) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.ST (writeArray, readArray, newArray)
import Data.Array.Unboxed (UArray)
import Data.Map (Map, lookup, size)
import Prelude hiding (lookup, lines, words, length)
import Data.Text (Text)
import Types
import Data.HashTable.ST.Basic hiding (lookup)
import Data.Aeson hiding (Series)
import GHC.Generics

bands :: Header -> Map Text Int -> [Frame] -> (UArray Int Double, UArray (Int, Int) Double)
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

data Series = Series { seriesKey :: Text, seriesValues :: [(Double, Double)] }
  deriving (Show, ToJSON, Generic)

series :: [Frame] ->  [Series]
series fs = runST $ do
  m <- new
  forM_ (reverse fs) $ \(Frame t s) ->
    forM_ s $ \(Sample k v) -> do
      let ins Nothing = (Just [(t, v)]  , ())
          ins (Just ss) = (Just ((t,v) : ss), ())
      mutate m k ins
  foldM (\r (k,v) -> return (Series k v : r)) [] m
