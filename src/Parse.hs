module Parse where

import Control.Monad.State.Strict(State(), evalState, get, put)
import Control.Monad(foldM)
import Data.List(isPrefixOf)
import Data.Map(Map, empty, insertWith', alter, (!))

import Types

parse :: String -> Run
parse s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] =
        zipWith header ["JOB", "DATE", "SAMPLE_UNIT", "VALUE_UNIT"] hs
      frames = flip evalState empty . mapM parseFrame . chunkSamples1 $ ss
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

parseFrame :: [String] -> State (Map String String) Frame
parseFrame [] = error "Parse.parseFrame: empty"
parseFrame (l:ls) = do
  let begin = sampleTime "BEGIN_SAMPLE" l
      end   = sampleTime "END_SAMPLE" $ last ls
      smps  = init ls
      time  =
        if begin == end
        then begin
        else error "Parse.parseFrame: begin /= end"
  samples <- foldM inserter empty smps
  return  Frame
          { hpfTime    = time
          , hpfSamples = samples
          }

inserter :: Map String Double -> String -> State (Map String String) (Map String Double)
inserter m s = do
  let [k,vs] = words s
      v = read vs
  get >>= put . alter (alterer k) k
  names <- get
  return $ insertWith' (+) (names ! k) v m

alterer :: String -> Maybe String -> Maybe String
alterer s Nothing = Just s
alterer _ js      = js

sampleTime :: String -> String -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then read . drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ name
