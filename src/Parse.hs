module Parse where

import Data.List(isPrefixOf)
import Data.Map(Map, empty, insertWith')

import Types

parse :: String -> Run
parse s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] =
        zipWith header ["JOB", "DATE", "SAMPLE_UNIT", "VALUE_UNIT"] hs
      frames = map parseFrame . chunkSamples1 $ ss
  in  Run
      { hprJob        = job
      , hprDate       = date
      , hprSampleUnit = smpU
      , hprValueUnit  = valU
      , hprFrames     = frames
      }

header :: String -> String -> String
header name h =
  if name `isPrefixOf` h
  then read . drop (length name + 1) $ h
  else error $ "Parse.header: expected " ++ name

chunkSamples1 :: [String] -> [[String]]
chunkSamples1 [] = []
chunkSamples1 (x:xs) =
  if "BEGIN_SAMPLE" `isPrefixOf` x
  then let (ys,zs) = chunkSamples2 xs in (x:ys) : chunkSamples1 zs
  else [] -- expected BEGIN_SAMPLE or EOF...

chunkSamples2 :: [String] -> ([String], [String])
chunkSamples2 [] = ([], []) -- expected END_SAMPLE or EOF...
chunkSamples2 (x:xs) =
  if "END_SAMPLE" `isPrefixOf` x
  then ([x],xs)
  else let (ys,zs) = chunkSamples2 xs in (x:ys, zs)

parseFrame :: [String] -> Frame
parseFrame [] = error "Parse.parseFrame: empty"
parseFrame (l:ls) =
  let begin = sampleTime "BEGIN_SAMPLE" l
      end   = sampleTime "END_SAMPLE" $ last ls
      smps  = init ls
      time  =
        if begin == end
        then begin
        else error "Parse.parseFrame: begin /= end"
      samples = foldr inserter empty smps
  in  Frame
      { hpfTime    = time
      , hpfSamples = samples
      }

inserter :: String -> Map String Double -> Map String Double
inserter s m =
  let [k,vs] = words s
      v = read vs
  in  insertWith' (+) k v m

sampleTime :: String -> String -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then read . drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ name
