module Types where

import Data.Map (Map)

data Run =
  Run
  { hprJob        :: String
  , hprDate       :: String
  , hprSampleUnit :: String
  , hprValueUnit  :: String
  , hprFrames     :: [Frame]
  }
  deriving (Read, Show, Eq, Ord)

data Frame =
  Frame
  { hpfTime       :: Double
  , hpfSamples    :: Map String Double
  }
  deriving (Read, Show, Eq, Ord)

data Info =
  Info
  { hpiJob        :: String
  , hpiDate       :: String
  , hpiSampleUnit :: String
  , hpiValueUnit  :: String
  , hpiSampleRange:: (Double, Double)
  , hpiValueRange :: (Double, Double)
  , hpiSamples    :: [Double]
  , hpiValues     :: [(String, [Double])]
  , hpiTrace      :: [Double]
  }
  deriving (Read, Show, Eq, Ord)

data Graph =
  Graph
  { hpgJob        :: String
  , hpgDate       :: String
  , hpgSampleUnit :: String
  , hpgValueUnit  :: String
  , hpgSampleRange:: (Double, Double)
  , hpgValueRange :: (Double, Double)
  , hpgSampleTicks:: [Double]
  , hpgValueTicks :: [Double]
  , hpgLabels     :: [String]
  , hpgBands      :: [[Double]]
  , hpgSamples    :: [Double]
  }
  deriving (Read, Show, Eq, Ord)
