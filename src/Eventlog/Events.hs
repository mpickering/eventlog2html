{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
module Eventlog.Events(chunk) where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)
import qualified Data.Text as T
import Data.Text (Text)

import Eventlog.Types
import Eventlog.Total
import Eventlog.Args (Args(..))
import Data.List
import Data.Function
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector, (!?), toList)
import Data.Maybe
import Data.Version
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char
import System.IO
import qualified Data.Trie.Map as Trie

type PartialHeader = Int -> Header

fromNano :: Word64 -> Double
fromNano e = fromIntegral e * 1e-9

chunk :: Args -> FilePath -> IO ProfData
chunk a f = do
  (EventLog _ e) <- either error id <$> readEventLogFromFile f
  (ph, frames, traces) <- eventsToHP a e
  let (counts, totals) = total frames
  return $ (ProfData (ph counts) totals frames traces)

checkGHCVersion :: EL -> Maybe String
checkGHCVersion EL { ident = Just (version,_)}
  | version <= makeVersion [8,4,4]  =
      Just $ "Warning: The eventlog has been generated with ghc-"
           ++ showVersion version
           ++ ", which does not support profiling events in the eventlog."
checkGHCVersion EL { pargs = Just args, ident = Just (version,_)}
  | version > makeVersion [8,4,4] &&
    version <= makeVersion [8,9,0] &&
    ("-hr" `elem` args || "-hb" `elem` args) =
     Just $ "Warning: The eventlog has been generated with ghc-"
            ++ showVersion version
            ++ ", which does not support biographical or retainer profiling."
checkGHCVersion _ = Nothing


eventsToHP :: Args -> Data -> IO (PartialHeader, [Frame], [Trace])
eventsToHP a (Data es) = do
  let
      el@EL{..} = foldEvents a es
      fir = Frame (fromNano start) []
      las = Frame (fromNano end) []
  mapM_ (hPutStrLn stderr) (checkGHCVersion el)
  return $ (elHeader el, fir : reverse (las: normalise frames) , traces)

normalise :: [FrameEL] -> [Frame]
normalise fs = map (\(FrameEL t ss) -> Frame (fromNano t) ss) fs


data EL = EL
  { pargs :: !(Maybe [String])
  , ident :: Maybe (Version, String)
  , samplingRate :: !(Maybe Word64)
  , heapProfileType :: !(Maybe HeapProfBreakdown)
  , ccMap :: !(Map.Map Word32 CostCentre)
  , ccsMap :: CCSMap
  , clocktimeSec :: !Word64
  , samples :: !(Maybe FrameEL)
  , frames :: ![FrameEL]
  , traces :: ![Trace]
  , start :: !Word64
  , end :: !Word64 } deriving Show

data FrameEL = FrameEL Word64 [Sample] deriving Show

data CCSMap = CCSMap (Trie.TMap Word32 CCStack) Int deriving Show

data CCStack = CCStack { ccsId :: Int, ccsName :: Text } deriving Show

getCCSId :: EL -> Vector Word32 -> (CCStack, EL)
getCCSId el@EL { ccsMap = (CCSMap trie uniq), ccMap = ccMap } k  =
  let kl = toList k
  in case Trie.lookup kl trie of
        Just n -> (n, el)
        Nothing ->
          let new_stack = CCStack uniq name
          in (new_stack, el { ccsMap = CCSMap (Trie.insert kl new_stack trie) (uniq + 1) })
  where
    name = fromMaybe "MAIN" $ do
             cid <- (k !? 0)
             CC{label, modul} <- Map.lookup cid ccMap
             return $ modul <> "." <> label

data CostCentre = CC { cid :: Word32
                     , label :: Text
                     , modul :: Text
                     , loc :: Text } deriving Show

initEL :: Word64 -> EL
initEL t = EL
  { pargs = Nothing
  , ident = Nothing
  , samplingRate = Nothing
  , heapProfileType = Nothing
  , clocktimeSec = 0
  , samples = Nothing
  , frames = []
  , traces = []
  , start = t
  , end = 0
  , ccMap = Map.empty
  , ccsMap =  CCSMap Trie.empty 0
  }

foldEvents :: Args -> [Event] -> EL
foldEvents a (e:es) =
  let res = foldl' (folder a)  (initEL (evTime e)) (e:es)
  in addFrame 0 res
foldEvents _ [] = error "Empty event log"

folder :: Args -> EL -> Event -> EL
folder a el (Event t e _) = el &
  updateLast t .
    case e of
      -- Traces
      -- Messages and UserMessages correspond to high-frequency "traceEvent" or "traceEventIO" events from Debug.Trace and
      -- are only included if "--include-trace-events" has been specified.
      -- For low-frequency events "traceMarker" or "traceMarkerIO" should be used, which generate "UserMarker" events.
      Message s -> if traceEvents a then addTrace a (Trace (fromNano t) (T.pack s)) else id
      UserMessage s -> if traceEvents a then addTrace a (Trace (fromNano t) (T.pack s)) else id
      UserMarker s -> addTrace a (Trace (fromNano t) (T.pack s))
      -- Information about the program
      RtsIdentifier _ ident -> addIdent ident
      ProgramArgs _ as -> addArgs as
      WallClockTime _ s _ -> addClocktime s
      -- Profiling Events
      HeapProfBegin { heapProfSamplingPeriod, heapProfBreakdown } -> addHeapProfBegin heapProfSamplingPeriod heapProfBreakdown
      HeapProfCostCentre cid l m loc _  -> addCostCentre cid (CC cid l m loc)
      HeapProfSampleBegin {} -> addFrame t
      HeapBioProfSampleBegin { heapProfSampleTime = t' } -> addFrame t'
      HeapProfSampleCostCentre _hid r d s -> addCCSample r d s
      HeapProfSampleString _hid res k -> addSample (Sample k (fromIntegral res))
      _ -> id


addHeapProfBegin :: Word64 -> HeapProfBreakdown -> EL -> EL
addHeapProfBegin sr hptype el = el { samplingRate = Just sr, heapProfileType = Just hptype }

addIdent :: String -> EL -> EL
addIdent s el = el { ident = parseIdent s }

parseIdent :: String -> Maybe (Version, String)
parseIdent s = listToMaybe $ flip readP_to_S s $ do
  void $ string "GHC-"
  [v1, v2, v3] <- replicateM 3 (intP <* optional (char '.'))
  skipSpaces
  return (makeVersion [v1,v2,v3])
  where
    intP = do
      x <- munch1 isDigit
      return $ read x


addCostCentre :: Word32 -> CostCentre -> EL -> EL
addCostCentre s cc el = el { ccMap = Map.insert s cc (ccMap el) }

addCCSample :: Word64 -> Word8 -> Vector Word32 -> EL -> EL
addCCSample res _sd st el =
  let (CCStack stack_id tid, el') = getCCSId el st
      -- TODO: Can do better than this by differentiating normal samples form stack samples
      sample_string = (T.pack $ "(" ++ show stack_id ++ ") ") <> tid
  in addSample (Sample sample_string (fromIntegral res)) el'


addClocktime :: Word64 -> EL -> EL
addClocktime s el = el { clocktimeSec = s }

addArgs :: [String] -> EL -> EL
addArgs as el = el { pargs = Just as }


-- | Decide whether to include a trace based on the "includes" and
-- "excludes" options.
--
-- If a trace satisfies an `-i` flag then it is certainly included.
--
-- For example for a trace called "eventlog2html" then `-i eventlog -x
-- html` will still include the trace because the `-i` option matches.
--
-- If a trace doesn't match an `-i` flag then it is excluded if it matches
-- a `-x` flag.
--
filterTrace :: [Text] -> [Text] -> Trace -> Bool
filterTrace []       []       _             = True
filterTrace []       excludes (Trace _ trc) =
  not (any (flip T.isInfixOf trc) excludes)
filterTrace includes []       (Trace _ trc) =
  any (flip T.isInfixOf trc) includes
filterTrace includes excludes (Trace _ trc) =
  any (flip T.isInfixOf trc) includes
    || not (any (flip T.isInfixOf trc) excludes)

addTrace :: Args -> Trace -> EL -> EL
addTrace a t el | noTraces a = el
                | prop t     = el { traces = t : traces el }
                | otherwise  = el
  where
    prop = filterTrace (includeStr a) (excludeStr a)

addFrame :: Word64 -> EL -> EL
addFrame t el =
  el { samples = Just (FrameEL t [])
     , frames = sampleToFrames (samples el) (frames el) }

sampleToFrames :: Maybe FrameEL -> [FrameEL]
                                -> [FrameEL]
sampleToFrames (Just (FrameEL t ss)) fs = FrameEL t (reverse ss) : fs
sampleToFrames Nothing fs = fs

addSample :: Sample -> EL -> EL
addSample s el = el { samples = go <$> (samples el) }
  where
    go (FrameEL t ss) = FrameEL t (s:ss)

updateLast :: Word64 -> EL -> EL
updateLast t el = el { end = t }

formatDate :: Word64 -> T.Text
formatDate sec =
  let posixTime :: POSIXTime
      posixTime = realToFrac sec
  in
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%d, %H:%M %Z" (posixSecondsToUTCTime posixTime)

elHeader :: EL -> PartialHeader
elHeader EL{..} =
  let title = maybe "" (T.unwords . map T.pack) pargs
      date = formatDate clocktimeSec
      profileType = ppHeapProfileType heapProfileType
  in Header title date profileType "" ""

ppHeapProfileType :: Maybe HeapProfBreakdown -> Text
ppHeapProfileType (Just HeapProfBreakdownCostCentre) = "Cost centre profiling (implied by -hc)"
ppHeapProfileType (Just HeapProfBreakdownModule) = "Profiling by module (implied by -hm)"
ppHeapProfileType (Just HeapProfBreakdownClosureDescr) = "Profiling by closure description (implied by -hd)"
ppHeapProfileType (Just HeapProfBreakdownTypeDescr) = "Profiling by type (implied by -hy)"
ppHeapProfileType (Just HeapProfBreakdownRetainer) = "Retainer profiling (implied by -hr)"
ppHeapProfileType (Just HeapProfBreakdownBiography) = "Biographical profiling (implied by -hb)"
ppHeapProfileType (Just HeapProfBreakdownClosureType) = "Basic heap profile (implied by -hT)"
ppHeapProfileType Nothing = "<Not available>"
