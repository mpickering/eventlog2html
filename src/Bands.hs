{-# LANGUAGE BangPatterns #-}
module Bands (bands) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.ST (writeArray, readArray, newArray)
import Data.Array.Unboxed (UArray)
import Data.Map (Map, lookup, size)
import Prelude hiding (lookup, lines, words, length)
import Data.Text (Text)
import Types

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
