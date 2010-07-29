{-# LANGUAGE BangPatterns #-}
module Parse (parse) where

import Control.Monad.State.Strict (State(), runState, get, put)
import Control.Monad (foldM)
import Data.Map (Map, empty, insertWith', alter, (!))
import Numeric (readSigned, readFloat)
import Prelude hiding (lines, words, drop, length)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack, lines, words, isPrefixOf, drop, length)

import Types

data Parse =
  Parse
  { symbols :: !(Map ByteString ByteString) -- intern symbols to save RAM
  , totals  :: !(Map ByteString Double    ) -- compute running totals
  }
  deriving (Read, Show, Eq, Ord)

parse0 :: Parse
parse0 = Parse{ symbols = empty, totals = empty }

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
  let time = sampleTime sBEGIN_SAMPLE l
  samples <- foldM inserter empty ls
  return  Frame
          { fTime    = time
          , fSamples = samples
          }

inserter :: Map ByteString Double -> ByteString -> State Parse (Map ByteString Double)
inserter !m s = do
  let [k,vs] = words s
      !v = readDouble vs
  p <- get
  let symbols' = alter (intern k) k (symbols p)
      totals'  = alter (accum  v) k (totals  p)
  put $! p{ symbols = symbols', totals = totals' }
  p' <- get
  return $! insertWith' (+) (symbols p' ! k) v m

intern :: ByteString -> Maybe ByteString -> Maybe ByteString
intern s Nothing = Just s
intern _ js      = js

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
