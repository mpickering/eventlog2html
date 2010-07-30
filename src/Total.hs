{-# LANGUAGE BangPatterns #-}
module Total (total) where

import Control.Monad.State.Strict (State(), execState, get, put)
import Data.List (foldl')
import Data.Map (Map, empty, lookup, insert, alter)
import Numeric (readSigned, readFloat)
import Prelude hiding (lookup, lines, words, drop, length)
import Data.ByteString.Char8 (ByteString, pack, unpack, lines, words, isPrefixOf, drop, length)

import Types

data Parse =
  Parse
  { symbols   :: !(Map ByteString ByteString) -- intern symbols to save RAM
  , totals    :: !(Map ByteString Double    ) -- compute running totals
  , sampleMin :: !Double
  , sampleMax :: !Double
  , valueMin  :: !Double
  , valueMax  :: !Double
  , count     :: !Int                         -- number of frames
  }

parse0 :: Parse
parse0 = Parse{ symbols = empty, totals = empty, sampleMin = 0, sampleMax = 0, valueMin = 0, valueMax = 0, count = 0 }

total :: ByteString -> (Header, Map ByteString Double)
total s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] =
        zipWith header [sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT] hs
      parse1 = flip execState parse0 . mapM_ parseFrame . chunkSamples $ ss
  in  ( Header
        { hJob        = job
        , hDate       = date
        , hSampleUnit = smpU
        , hValueUnit  = valU
        , hSampleRange= (sampleMin parse1, sampleMax parse1)
        , hValueRange = (valueMin parse1, valueMax parse1)
        , hCount      = count parse1
        }
      , totals parse1
      )

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

parseFrame :: [ByteString] -> State Parse ()
parseFrame [] = error "Parse.parseFrame: empty"
parseFrame (l:ls) = do
  let !time = sampleTime sBEGIN_SAMPLE l
  samples <- mapM inserter ls
  p <- get
  let v = foldl' (+) 0 samples
      sMin = if count p == 0 then time else time `min` sampleMin p
      sMax = if count p == 0 then time else time `max` sampleMax p
      vMin = v `min` valueMin p
      vMax = v `max` valueMax p
  put $! p{ count = count p + 1, sampleMin = sMin, sampleMax = sMax, valueMin = vMin, valueMax = vMax }

inserter :: ByteString -> State Parse Double
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
  return $! v

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
