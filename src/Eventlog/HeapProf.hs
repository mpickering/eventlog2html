{-# LANGUAGE BangPatterns #-}
module Eventlog.HeapProf (chunk) where

import Prelude hiding (init, lookup, lines, words, drop, length, readFile)
import Data.Text (Text, lines, init, drop, length, isPrefixOf, unpack, words, pack)
import Data.Text.IO (readFile)
import Data.Attoparsec.Text (parseOnly, double)

import Eventlog.Total
import Eventlog.Types

chunk :: FilePath -> IO ProfData
chunk f = do
  (ph, fs) <- chunkT <$> readFile f
  let (counts, totals) = total fs
  -- Heap profiles do not support traces
  return (ProfData (ph counts) totals fs [])

chunkT :: Text -> (Int -> Header, [Frame])
chunkT s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] =
        zipWith header [sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT] hs
      fs = chunkSamples ss
  in  (
        Header job date (pack "") (pack "") smpU valU
      ,  fs
      )

header :: Text -> Text -> Text
header name h =
  if name `isPrefixOf` h
  then init . drop (length name + 2) $ h -- drop the name and the quotes
  else error $ "Parse.header: expected " ++ unpack name

chunkSamples :: [Text] -> [Frame]
chunkSamples [] = []
chunkSamples (x:xs)
  | sBEGIN_SAMPLE `isPrefixOf` x =
      let (ys, zs) = break (sEND_SAMPLE `isPrefixOf`) xs
      in  case zs of
            [] -> [] -- discard incomplete sample
            (_:ws) -> parseFrame x ys : chunkSamples ws
  | otherwise = [] -- expected BEGIN_SAMPLE or EOF...

parseFrame :: Text -> [Text] -> Frame
parseFrame l ls =
  let !time = sampleTime sBEGIN_SAMPLE l
      ss = map parseSample ls
  in Frame time ss

parseSample :: Text -> Sample
parseSample s =
  let [k,vs] = words s
      !v = readDouble vs
  in (Sample k v)


sampleTime :: Text -> Text -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then readDouble .  drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ unpack name ++ " but got " ++ unpack h

readDouble :: Text -> Double
readDouble s = case parseOnly double s of
  Right x -> x
  _ -> error $ "Parse.readDouble: no parse " ++ unpack s

sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT, sBEGIN_SAMPLE, sEND_SAMPLE :: Text
sJOB = pack "JOB"
sDATE = pack "DATE"
sSAMPLE_UNIT = pack "SAMPLE_UNIT"
sVALUE_UNIT = pack "VALUE_UNIT"
sBEGIN_SAMPLE = pack "BEGIN_SAMPLE"
sEND_SAMPLE = pack "END_SAMPLE"
