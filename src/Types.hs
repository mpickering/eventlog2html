module Types where

import Data.Array.Unboxed (UArray)
import Data.ByteString.Lazy.Char8(ByteString)
import Data.Map (Map)

data Run =
  Run
  { hprJob        :: ByteString
  , hprDate       :: ByteString
  , hprSampleUnit :: ByteString
  , hprValueUnit  :: ByteString
  , hprFrames     :: [Frame]
  }
  deriving (Read, Show, Eq, Ord)

data Frame =
  Frame
  { hpfTime       :: Double
  , hpfSamples    :: Map ByteString Double
  }
  deriving (Read, Show, Eq, Ord)

data Info =
  Info
  { hpiJob        :: ByteString
  , hpiDate       :: ByteString
  , hpiSampleUnit :: ByteString
  , hpiValueUnit  :: ByteString
  , hpiSampleRange:: (Double, Double)
  , hpiValueRange :: (Double, Double)
  , hpiSamples    :: [Double]
  , hpiValues     :: [(ByteString, [Double])]
  , hpiTrace      :: [Double]
  }
  deriving (Read, Show, Eq, Ord)

data Graph =
  Graph
  { hpgJob        :: ByteString
  , hpgDate       :: ByteString
  , hpgSampleUnit :: ByteString
  , hpgValueUnit  :: ByteString
  , hpgSampleRange:: (Double, Double)
  , hpgValueRange :: (Double, Double)
  , hpgSampleTicks:: [Double]
  , hpgValueTicks :: [Double]
  , hpgLabels     :: [ByteString]
  , hpgBands      :: UArray (Int, Int) Double
  , hpgSamples    :: [Double]
  }
  deriving (Show, Eq, Ord)
