{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Events(chunk) where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)
import qualified Data.Text as T

import Types
import Data.List
import Data.Function
import Data.Word
--import Data.Time
--import Data.Time.Clock.POSIX(posixSecondsToUTCTime)

fromNano :: Word64 -> Double
fromNano e = fromIntegral e * 1e-9

chunk :: FilePath -> IO (Header, [Frame], [Trace])
chunk f = eventlogToHP . either error id =<< readEventLogFromFile f

eventlogToHP :: EventLog -> IO (Header, [Frame], [Trace])
eventlogToHP (EventLog _h e) = do
  eventsToHP e

eventsToHP :: Data -> IO (Header, [Frame], [Trace])
eventsToHP (Data es) = do
  let
      el@EL{..} = foldEvents es
      fir = Frame (fromNano start) []
      las = Frame (fromNano end) []
  return $ (elHeader el, fir : reverse (las: normalise frames) , traces)

normalise :: [(Word64, [Sample])] -> [Frame]
normalise fs = map (\(t, ss) -> Frame (fromNano t) ss) fs

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



addArgs :: [String] -> EL -> EL
addArgs as el = el { pargs = Just as }

addTrace :: Trace -> EL -> EL
addTrace t el = el { traces = t : traces el }

addFrame :: Word64 -> EL -> EL
addFrame t el =
  el { samples = Just (t, [])
     , frames = sampleToFrames (samples el) (frames el) }

sampleToFrames :: Maybe (Word64, [Sample]) -> [(Word64, [Sample])]
                                           -> [(Word64, [Sample])]
sampleToFrames (Just (t, ss)) fs = (t, (reverse ss)) : fs
sampleToFrames Nothing fs = fs

addSample :: Sample -> EL -> EL
addSample s el = el { samples = go <$> (samples el) }
  where
    go (t, ss) = (t, (s:ss))

updateLast :: Word64 -> EL -> EL
updateLast t el = el { end = t }


elHeader :: EL -> Header
elHeader EL{..} =
  let title = maybe "" (T.unwords . map T.pack) pargs
   --   dl = formatTime defaultTimeLocale "%c"
   --      . posixSecondsToUTCTime $ realToFrac start

 in Header title "aaa" "" "" (length frames + 2)
 -- The 2 Corresponds to the additional 2 frames inserted in eventsToHP


