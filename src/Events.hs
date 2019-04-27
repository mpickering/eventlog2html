{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Events(chunk) where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)

import Types
import Data.Maybe

chunk :: FilePath -> IO (PartialHeader, [Frame])
chunk f = eventlogToHP . either error id =<< readEventLogFromFile f

eventlogToHP :: EventLog -> IO (PartialHeader, [Frame])
eventlogToHP (EventLog _h e) = do
  eventsToHP e

eventsToHP :: Data -> IO (PartialHeader, [Frame])
eventsToHP (Data es) = do
  let es' =  filter filterEvent es
  return $ (partialHeader, chunkSamples es')

filterEvent :: Event -> Bool
filterEvent e = p (evSpec e)

p :: EventInfo -> Bool
p e = case e of
        HeapProfBegin {} -> True
        HeapProfCostCentre {} -> False
        HeapProfSampleBegin {} -> True
        HeapProfSampleCostCentre {} -> True
        HeapProfSampleString {} -> True
        _ -> False

isStartEvent :: Event -> Maybe Timestamp
isStartEvent e = case evSpec e of
                   HeapProfSampleBegin {} -> Just (evTime e)
                   _ -> Nothing

chunkSamples :: [Event] -> [Frame]
chunkSamples [] = []
chunkSamples (x:xs)
  | Just t <- isStartEvent x =
      let (ys, zs) = break (isJust . isStartEvent) xs
      in  (Frame (fromIntegral t) (mapMaybe mkSample ys)) : chunkSamples zs
  | otherwise = chunkSamples xs -- expected BEGIN_SAMPLE or EOF...

mkSample :: Event -> Maybe Sample
mkSample (Event _t s _) =
  case s of
    HeapProfSampleString _hid res k -> Just $ Sample k (fromIntegral res)
    _ -> Nothing

partialHeader :: PartialHeader
partialHeader = Header "" "" "" ""


