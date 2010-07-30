{-# LANGUAGE BangPatterns #-}
module Bands (bands) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.ST (writeArray, readArray, newArray)
import Data.Array.Unboxed (UArray)
import Data.Map (Map, lookup, size)
import Numeric (readSigned, readFloat)
import Prelude hiding (lookup, lines, words, length)
import Data.ByteString.Char8 (ByteString, pack, unpack, lines, words, isPrefixOf, length)
import qualified Data.ByteString.Char8 as BS

import Types

bands :: Header -> Map ByteString Int -> ByteString -> (UArray Int Double, UArray (Int, Int) Double)
bands h bs input = runST $ do
  times <- newArray (1, hCount h) 0
  vals  <- newArray ((-1,1), (size bs, hCount h)) 0
  forM_ (zip [1 ..] . chunkSamples . drop 4 . lines $ input) $ \(i, (t:ss)) -> do
    writeArray times i (sampleTime sBEGIN_SAMPLE t)
    forM_ ss $ \s -> do
      let [k,vs] = words s
          !v = readDouble vs
      case k `lookup` bs of
        Nothing -> writeArray vals (0, i) . (+ v) =<< readArray vals (0, i)
        Just b  -> writeArray vals (b, i) v
  times' <- unsafeFreezeSTUArray times
  vals'  <- unsafeFreezeSTUArray vals
  return (times', vals')

chunkSamples :: [ByteString] -> [[ByteString]]
chunkSamples [] = []
chunkSamples (x:xs)
  | sBEGIN_SAMPLE `isPrefixOf` x =
      let (ys, zs) = break (sEND_SAMPLE `isPrefixOf`) xs
      in  case zs of
            [] -> [] -- discard incomplete sample
            (_:ws) -> (x:ys) : chunkSamples ws
  | otherwise = [] -- expected BEGIN_SAMPLE or EOF...

sampleTime :: ByteString -> ByteString -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then readDouble .  BS.drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ unpack name ++ " but got " ++ unpack h

readDouble :: ByteString -> Double
readDouble s = case readSigned readFloat (unpack s) of
  ((x,_):_) -> x
  _ -> error $ "Parse.readDouble: no parse " ++ unpack s

sBEGIN_SAMPLE, sEND_SAMPLE :: ByteString
sBEGIN_SAMPLE = pack "BEGIN_SAMPLE"
sEND_SAMPLE = pack "END_SAMPLE"
