{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Events where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)
import qualified Data.Text as T

import Types
import Data.Maybe
import Data.List
import Data.Function
import Data.Word
import Data.Time
import Data.Time
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)

fromNano :: Word64 -> Double
fromNano e = fromIntegral e * 1e-9

chunk :: FilePath -> IO (PartialHeader, [Frame], [Trace])
chunk f = eventlogToHP . either error id =<< readEventLogFromFile f

eventlogToHP :: EventLog -> IO (PartialHeader, [Frame], [Trace])
eventlogToHP (EventLog _h e) = do
  eventsToHP e

eventsToHP :: Data -> IO (PartialHeader, [Frame], [Trace])
eventsToHP (Data es) = do
  let
      el@EL{..} = foldEvents es
      duration = end - start
      fir = Frame (fromNano start) []
      las = Frame (fromNano end) []
  return $ (elHeader el, fir : reverse (las: normalise duration frames) , traces)

normalise dur fs = map
                    (\(t, ss) -> Frame (fromNano t) ss) fs

data EL = EL
  { pargs :: Maybe [String]
  , samples :: Maybe (Word64, [Sample])
  , frames :: [(Word64, [Sample])]
  , traces :: [Trace]
  , start :: Word64
  , end :: Word64 } deriving Show

initEL :: Word64 -> EL
initEL t = EL Nothing Nothing [] [] t 0

foldEvents :: [Event] -> EL
foldEvents (e:es) =
  let res = foldl' folder  (initEL (evTime e)) (e:es)
  in addFrame 0 res
foldEvents [] = error "Empty event log"

folder :: EL -> Event -> EL
folder el (Event t e _) = el &
  updateLast t .
    case e of
      -- Traces
      Message s -> addTrace (Trace (fromNano t) (T.pack s))
      UserMessage s -> addTrace (Trace (fromNano t) (T.pack s))
      HeapProfBegin {} -> addFrame t
      --HeapProfCostCentre {} -> False
      HeapProfSampleBegin {} -> addFrame t
      --HeapProfSampleCostCentre {} -> True
      HeapProfSampleString _hid res k -> addSample (Sample k (fromIntegral res))
      ProgramArgs _ as -> addArgs as
      _ -> id




addArgs as el = el { pargs = Just as }
getTime = fromNano . evTime

addTrace t el = el { traces = t : traces el }

addFrame t el =
  el { samples = Just (t, [])
     , frames = sampleToFrames (samples el) (frames el) }

sampleToFrames :: Maybe (Word64, [Sample]) -> [(Word64, [Sample])]
                                           -> [(Word64, [Sample])]
sampleToFrames (Just (t, ss)) fs = (t, (reverse ss)) : fs
sampleToFrames Nothing fs = fs

addSample s el = el { samples = go <$> (samples el) }
  where
    go (t, ss) = (t, (s:ss))

updateLast :: Word64 -> EL -> EL
updateLast t el = el { end = t }


getTraces :: [Event] -> [Trace]
getTraces = mapMaybe isTrace
  where
    isTrace (Event t e _) =
      case e of
        _ -> Nothing

filterEvent :: Event -> Bool
filterEvent e = p (evSpec e)

p :: EventInfo -> Bool
p e = case e of
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
      in  (Frame (fromNano t) (mapMaybe mkSample ys)) : chunkSamples zs
  | otherwise = chunkSamples xs -- expected BEGIN_SAMPLE or EOF...

mkSample :: Event -> Maybe Sample
mkSample (Event _t s _) =
  case s of
    HeapProfSampleString _hid res k -> Just $ Sample k (fromNano res)
    _ -> Nothing

elHeader :: EL -> PartialHeader
elHeader EL{..} =
  let title = maybe "" (T.unwords . map T.pack) pargs
      dl = formatTime defaultTimeLocale "%c"
         . posixSecondsToUTCTime $ realToFrac start

  in Header title "aaa" "" ""


