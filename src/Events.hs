{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Events(chunk) where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)
import qualified Data.Text as T

import Types
import Data.Maybe

chunk :: FilePath -> IO (PartialHeader, [Frame], [Trace])
chunk f = eventlogToHP . either error id =<< readEventLogFromFile f

eventlogToHP :: EventLog -> IO (PartialHeader, [Frame], [Trace])
eventlogToHP (EventLog _h e) = do
  eventsToHP e

eventsToHP :: Data -> IO (PartialHeader, [Frame], [Trace])
eventsToHP (Data es) = do
  let fir = Frame (fromIntegral (evTime (head es))) []
      las = Frame (fromIntegral (evTime (last es))) []
  return $ (partialHeader, fir : chunkSamples es ++ [las], getTraces es)

getTraces :: [Event] -> [Trace]
getTraces = mapMaybe isTrace
  where
    isTrace (Event t e _) =
      case e of
        Message s -> Just (Trace (fromIntegral t) (T.pack s))
        UserMessage s -> Just (Trace (fromIntegral t) (T.pack s))
        _ -> Nothing

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

isNonSampleEvent :: Event -> Maybe Timestamp
isNonSampleEvent e = case evSpec e of
                       Startup {} -> Just (evTime e)
                       Shutdown   -> Just (evTime e)
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


