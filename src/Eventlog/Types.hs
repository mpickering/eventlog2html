{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eventlog.Types where

import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson
import Data.Hashable

data Header =
  Header
  { hJob         :: Text
  , hDate        :: Text
  , hHeapProfileType :: Text
  , hSamplingRate :: Text
  , hSampleUnit  :: Text
  , hValueUnit   :: Text
  , hCount       :: Int
  } deriving Show


-- The bucket is a key to uniquely identify a band
newtype Bucket = Bucket Text
                  deriving (Show, Ord, Eq)
                  deriving newtype (ToJSON, Hashable)


data BucketInfo = BucketInfo { shortDescription :: Text -- For the legend and hover
                             , longDescription :: Maybe Text -- Displayed beneath the graph
                             , bucketTotal :: Double
                             , bucketStddev :: Double
                             } deriving Show

data Sample = Sample Bucket Double deriving Show

data Frame = Frame Double [Sample] deriving Show

-- | A trace we also want to show on the graph
data Trace = Trace Double Text deriving Show

data ProfData = ProfData { profHeader :: Header
                         , profTotals ::  (Map Bucket BucketInfo)
                         , profFrames :: [Frame]
                         , profTraces :: [Trace] } deriving Show
