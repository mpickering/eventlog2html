{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eventlog.Types(module Eventlog.Types, HeapProfBreakdown(..)) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson
import Data.Hashable
import Data.Word
import GHC.RTS.Events (HeapProfBreakdown(..))

data Header =
  Header
  { hJob         :: Text
  , hDate        :: Text
  , hHeapProfileType :: Maybe HeapProfBreakdown
  , hSamplingRate :: Text
  , hSampleUnit  :: Text
  , hValueUnit   :: Text
  , hCount       :: Int
  , hProgPath    :: Maybe FilePath
  } deriving Show


-- The bucket is a key to uniquely identify a band
newtype Bucket = Bucket Text
                  deriving (Show, Ord, Eq)
                  deriving newtype (ToJSON, Hashable)


data BucketInfo = BucketInfo { shortDescription :: Text -- For the legend and hover
                             , longDescription :: Maybe [Word32]
                             , bucketTotal :: Double
                             , bucketStddev :: Double
                             , bucketGradient :: !Double
                             } deriving Show

data CostCentre = CC { cid :: Word32
                     , label :: Text
                     , modul :: Text
                     , loc :: Text } deriving Show

data Sample = Sample Bucket Double deriving Show

data Frame = Frame Double [Sample] deriving Show

-- | A trace we also want to show on the graph
data Trace = Trace Double Text deriving Show

data ProfData = ProfData { profHeader :: Header
                         , profTotals :: (Map Bucket BucketInfo)
                         , profCCMap  :: Map Word32 CostCentre
                         , profFrames :: [Frame]
                         , profTraces :: [Trace] } deriving Show
