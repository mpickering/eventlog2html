{-# LANGUAGE BangPatterns #-}
module Parse where

import Control.Monad.State.Strict(State(), evalState, get, put)
import Control.Monad(foldM)
import Data.Map(Map, empty, insertWith', alter, (!))
import Numeric (readSigned, readFloat)
import Prelude hiding (lines, words, drop, length)
import Data.ByteString.Lazy.Char8(ByteString, pack, unpack, lines, words, isPrefixOf, drop, length)

import Types

sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT, sBEGIN_SAMPLE, sEND_SAMPLE :: ByteString
sJOB = pack "JOB"
sDATE = pack "DATE"
sSAMPLE_UNIT = pack "SAMPLE_UNIT"
sVALUE_UNIT = pack "VALUE_UNIT"
sBEGIN_SAMPLE = pack "BEGIN_SAMPLE"
sEND_SAMPLE = pack "END_SAMPLE"

parse :: ByteString -> Run
parse s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] =
        zipWith header [sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT] hs
      frames = flip evalState empty . mapM parseFrame . chunkSamples $ ss
  in  Run
      { hprJob        = job
      , hprDate       = date
      , hprSampleUnit = smpU
      , hprValueUnit  = valU
      , hprFrames     = frames
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

parseFrame :: [ByteString] -> State (Map ByteString ByteString) Frame
parseFrame [] = error "Parse.parseFrame: empty"
parseFrame (l:ls) = do
  let time = sampleTime sBEGIN_SAMPLE l
  samples <- foldM inserter empty ls
  return  Frame
          { hpfTime    = time
          , hpfSamples = samples
          }

inserter :: Map ByteString Double -> ByteString -> State (Map ByteString ByteString) (Map ByteString Double)
inserter !m s = do
  let [k,vs] = words s
      !v = readDouble vs
  get >>= \n -> put $! alter (alterer k) k n
  names <- get
  return $! insertWith' (+) (names ! k) v m

alterer :: ByteString -> Maybe ByteString -> Maybe ByteString
alterer s Nothing = Just s
alterer _ js      = js

sampleTime :: ByteString -> ByteString -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then readDouble .  drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ unpack name ++ " but got " ++ unpack h

readDouble :: ByteString -> Double
readDouble s = case readSigned readFloat (unpack s) of
  ((x,_):_) -> x
  _ -> error $ "Parse.readDouble: no parse " ++ unpack s
