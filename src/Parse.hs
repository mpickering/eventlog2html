{-# LANGUAGE BangPatterns #-}
module Parse (parse) where

import Control.Monad.State.Strict (State(), runState, get, put)
import Data.List (foldl')
import Data.Map (Map, empty, lookup, insert, alter)
import Numeric (readSigned, readFloat)
import Prelude hiding (lookup, lines, words, drop, length)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack, lines, words, isPrefixOf, drop, length)

import Types

data Parse =
  Parse
  { symbols :: !(Map ByteString ByteString) -- intern symbols to save RAM
  , totals  :: !(Map ByteString Double    ) -- compute running totals
  , sampleR :: !(Double, Double)            -- sample range
  , valueR  :: !(Double, Double)
  , count   :: !Int                         -- number of frames
  }
  deriving (Read, Show, Eq, Ord)

parse0 :: Parse
parse0 = Parse{ symbols = empty, totals = empty, sampleR = (0,0), valueR = (0,0), count = 0 }

parse :: ByteString -> Run
parse s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] =
        zipWith header [sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT] hs
      (frames, parse1) = flip runState parse0 . mapM parseFrame . chunkSamples $ ss
  in  Run
      { rJob        = job
      , rDate       = date
      , rSampleUnit = smpU
      , rValueUnit  = valU
      , rSampleRange= sampleR parse1
      , rValueRange = valueR parse1
      , rCount      = count parse1
      , rFrames     = frames
      , rTotals     = totals parse1
      }

header :: ByteString -> ByteString -> ByteString
header name h =
  if name `isPrefixOf` h
  then pack . read . unpack . drop (length name + 1) $ h
  else error $ "Parse.header: expected " ++ unpack name

chunkSamples :: [ByteString] -> [[ByteString]]
chunkSamples [] = []
chunkSamples (x:xs)
  | sBEGIN_SAMPLE `isPrefixOf` x =
      let (ys, zs) = break (sEND_SAMPLE `isPrefixOf`) xs
      in  case zs of
            [] -> [] -- discard incomplete sample
            (_:ws) -> (x:ys) : chunkSamples ws
  | otherwise = [] -- expected BEGIN_SAMPLE or EOF...

parseFrame :: [ByteString] -> State Parse Frame
parseFrame [] = error "Parse.parseFrame: empty"
parseFrame (l:ls) = do
  let !time = sampleTime sBEGIN_SAMPLE l
  samples <- mapM inserter ls
  p <- get
  let v = foldl' (+) 0 . map snd $ samples
      sr = if count p == 0
           then (time, time)
           else let (!t1,!t2) = sampleR p
                in (time `min` t1, time `max` t2)
      vr = let (!v1,!v2) = valueR p
           in (v `min` v1, v `max` v2)
  put $! p{ count = count p + 1, sampleR = sr, valueR = vr }
  return (time, samples)

inserter :: ByteString -> State Parse (ByteString, Double)
inserter s = do
  let [k,vs] = words s
      !v = readDouble vs
  p <- get
  k' <- case lookup k (symbols p) of
    Nothing -> do
      put $! p{ symbols = insert k k (symbols p) }
      return k
    Just kk -> return kk
  p' <- get
  put $! p'{ totals = alter (accum  v) k' (totals p') }
  return $! (k', v)

accum :: Double -> Maybe Double -> Maybe Double
accum x Nothing  = Just x
accum x (Just y) = Just $! x + y

sampleTime :: ByteString -> ByteString -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then readDouble .  drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ unpack name ++ " but got " ++ unpack h

readDouble :: ByteString -> Double
readDouble s = case readSigned readFloat (unpack s) of
  ((x,_):_) -> x
  _ -> error $ "Parse.readDouble: no parse " ++ unpack s

sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT, sBEGIN_SAMPLE, sEND_SAMPLE :: ByteString
sJOB = pack "JOB"
sDATE = pack "DATE"
sSAMPLE_UNIT = pack "SAMPLE_UNIT"
sVALUE_UNIT = pack "VALUE_UNIT"
sBEGIN_SAMPLE = pack "BEGIN_SAMPLE"
sEND_SAMPLE = pack "END_SAMPLE"
