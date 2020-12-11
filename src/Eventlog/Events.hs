{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Eventlog.Events(chunk) where

import GHC.RTS.Events hiding (Header, header)
import Prelude hiding (init, lookup)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Data.Map.Merge.Lazy
import Data.Functor.Identity
import GHC.Exts.Heap.ClosureTypes

type PartialHeader = Int -> Header

fromNano :: Word64 -> Double
fromNano e = fromIntegral e * 1e-9


chunk :: Args -> FilePath -> IO ProfData
chunk a f = do
  (EventLog _ e) <- either error id <$> readEventLogFromFile f
  (ph, bucket_map, ccMap, frames, traces, ipes) <- eventsToHP a e
  let (counts, totals) = total frames
      -- If both keys are present, combine
      combine = zipWithAMatched (\_ (t, mt) (tot, sd, g) -> Identity $ BucketInfo t mt tot sd g)
      -- If total is missing, something bad has happened
      combineMissingTotal :: Bucket -> (Text, Maybe [Word32]) -> Identity BucketInfo
      combineMissingTotal k = error ("Missing total for: " ++ show k)

      -- This case happens when we are not in CC mode
      combineMissingDesc :: Bucket -> (Double, Double, Maybe (Double, Double, Double)) -> Identity BucketInfo
      combineMissingDesc (Bucket t) (tot, sd, g) = Identity (BucketInfo t Nothing tot sd g)

      binfo = merge (traverseMissing combineMissingTotal) (traverseMissing combineMissingDesc) combine bucket_map totals
  return $ (ProfData (ph counts) binfo ccMap frames traces ipes)

checkGHCVersion :: EL -> Maybe Text
checkGHCVersion EL { ident = Just (version,_)}
  | version <= makeVersion [8,4,4]  =
      Just $ "Warning: The eventlog has been generated with ghc-"
           <> T.pack (showVersion version)
           <> ", which does not support profiling events in the eventlog."
checkGHCVersion EL { pargs = Just args, ident = Just (version,_)}
  | version > makeVersion [8,4,4] &&
    version <= makeVersion [8,9,0] &&
    ("-hr" `elem` args || "-hb" `elem` args) =
     Just $ "Warning: The eventlog has been generated with ghc-"
            <> T.pack (showVersion version)
            <> ", which does not support biographical or retainer profiling."
checkGHCVersion _ = Nothing

eventsToHP :: Args -> Data -> IO (PartialHeader, BucketMap, Map.Map Word32 CostCentre, [Frame], [Trace], Map.Map InfoTablePtr InfoTableLoc)
eventsToHP a (Data es) = do
  let
      el@EL{..} = foldEvents a es
      fir = Frame (fromNano start) []
      las = Frame (fromNano end) []
  mapM_ (T.hPutStrLn stderr) (checkGHCVersion el)
  return $ (elHeader el, elBucketMap el, ccMap, fir : reverse (las: normalise frames) , traces, Map.fromList ipes)

normalise :: [FrameEL] -> [Frame]
normalise = map (\(FrameEL t ss) -> Frame (fromNano t) ss)

type BucketMap = Map.Map Bucket (Text, Maybe [Word32])

data EL = EL
  { pargs :: !(Maybe [Text])
  , programInvocation :: !(Maybe FilePath)
  , ident :: Maybe (Version, Text)
  , samplingRate :: !(Maybe Word64)
  , heapProfileType :: !(Maybe HeapProfBreakdown)
  , ccMap :: !(Map.Map Word32 CostCentre)
  -- At the moment bucketMap and CCS map are quite similar, cost centre profiling
  -- is the only mode to populate the bucket map
  , bucketMap :: BucketMap
  , ccsMap :: CCSMap
  , clocktimeSec :: !Word64
  , samples :: !(Maybe FrameEL)
  , frames :: ![FrameEL]
  , traces :: ![Trace]
  , ipes :: [(InfoTablePtr, InfoTableLoc)]
  , start :: !Word64
  , end :: !Word64 } deriving Show


data FrameEL = FrameEL Word64 [Sample] deriving Show

data CCSMap = CCSMap (Trie.TMap Word32 CCStack) Int deriving Show


data CCStack = CCStack { ccsId :: Int, ccsName :: Text } deriving Show

getCCSId :: EL -> Vector Word32 -> (CCStack, EL)
getCCSId el@EL { ccsMap = (CCSMap trie uniq), ccMap = ccMap } k  =
  let kl = reverse $ toList k
  in case Trie.lookup kl trie of
        Just n -> (n, el)
        Nothing ->
          let new_stack = CCStack uniq name

              sid = T.pack $ "(" ++ show uniq ++ ") "
              short_bucket_info = sid <> name
              bucket_info = (short_bucket_info, Just kl)
              bucket_key = Bucket (T.pack (show uniq))
          in (new_stack, el { ccsMap = CCSMap (Trie.insert kl new_stack trie) (uniq + 1)
                            , bucketMap = Map.insert bucket_key bucket_info (bucketMap el) })
  where
    name = fromMaybe "MAIN" $ do
             cid <- (k !? 0)
             CC{label} <- Map.lookup cid ccMap
             return $ label


initEL :: EL
initEL = EL
  { pargs = Nothing
  , ident = Nothing
  , samplingRate = Nothing
  , heapProfileType = Nothing
  , clocktimeSec = 0
  , samples = Nothing
  , frames = []
  , traces = []
  , ipes = []
  , start = 0
  , end = 0
  , ccMap = Map.empty
  , ccsMap =  CCSMap Trie.empty 0
  , bucketMap = Map.empty
  , programInvocation = Nothing
  }

foldEvents :: Args -> [Event] -> EL
foldEvents a es =
  let res = foldl' (folder a) initEL es
  in addFrame 0 res

folder :: Args -> EL -> Event -> EL
folder a el (Event t e _) = el &
  updateLast t .
    case e of
      -- Traces
      -- Messages and UserMessages correspond to high-frequency "traceEvent" or "traceEventIO" events from Debug.Trace and
      -- are only included if "--include-trace-events" has been specified.
      -- For low-frequency events "traceMarker" or "traceMarkerIO" should be used, which generate "UserMarker" events.
      Message s -> if traceEvents a then addTrace a (Trace (fromNano t) s) else id
      UserMessage s -> if traceEvents a then addTrace a (Trace (fromNano t) s) else id
      UserMarker s -> addTrace a (Trace (fromNano t) s)
      -- Information about the program
      RtsIdentifier _ ident -> addIdent ident
      ProgramArgs _ as -> addArgs as
      ProgramInvocation inv -> addInvocation inv
      WallClockTime _ s _ -> addClocktime s
      -- Profiling Events
      HeapProfBegin { heapProfSamplingPeriod, heapProfBreakdown } -> addHeapProfBegin heapProfSamplingPeriod heapProfBreakdown
      HeapProfCostCentre cid l m loc _  -> addCostCentre cid (CC cid l m loc)
      HeapProfSampleBegin {}
        | t >= 1 -> addFrame t
      HeapBioProfSampleBegin { heapProfSampleTime = t' } -> addFrame t'
      HeapProfSampleCostCentre _hid r d s -> addCCSample r d s
      HeapProfSampleString _hid res k -> addSample (Sample (Bucket k) (fromIntegral res))
      IPE ptr name desc ty lbl smod sloc -> addInfoTableLoc (InfoTablePtr ptr,
                                              InfoTableLoc name (parseClosureType desc) ty lbl smod sloc)
      _ -> id

parseClosureType :: Text -> ClosureType
-- IPEs do not distinguish different CONSTR types, yet
parseClosureType "0" = CONSTR
parseClosureType ct = toEnum . read @Int . T.unpack $ ct

addInfoTableLoc :: (InfoTablePtr, InfoTableLoc) -> EL -> EL
addInfoTableLoc itl el = el { ipes = itl : ipes el }

addHeapProfBegin :: Word64 -> HeapProfBreakdown -> EL -> EL
addHeapProfBegin sr hptype el = el { samplingRate = Just sr, heapProfileType = Just hptype }

addIdent :: Text -> EL -> EL
addIdent s el = el { ident = fmap T.pack <$> (parseIdent (T.unpack s)) }

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
  let (CCStack stack_id _tid, el') = getCCSId el st
      -- TODO: Can do better than this by differentiating normal samples form stack samples
      sample_string = T.pack (show stack_id)
  in addSample (Sample (Bucket sample_string) (fromIntegral res)) el'


addClocktime :: Word64 -> EL -> EL
addClocktime s el = el { clocktimeSec = s }

addArgs :: [Text] -> EL -> EL
addArgs as el = el { pargs = Just as }

addInvocation :: String -> EL -> EL
addInvocation inv el = el { programInvocation = Just inv }


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
  let title = maybe "" T.unwords pargs
      date = formatDate clocktimeSec
      ppSamplingRate = T.pack . maybe "<Not available>" (show . fromNano) $ samplingRate
  in \v -> Header title date heapProfileType ppSamplingRate "" "" v (T.unpack . head <$> pargs)


elBucketMap :: EL -> BucketMap
elBucketMap = bucketMap


