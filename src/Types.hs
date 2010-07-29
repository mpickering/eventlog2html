module Types where

import Data.Array.Unboxed (UArray)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)

data Run =
  Run
  { rJob         :: ByteString
  , rDate        :: ByteString
  , rSampleUnit  :: ByteString
  , rValueUnit   :: ByteString
  , rFrames      :: [Frame]
  , rTotals      :: Map ByteString Double
  }

data Frame =
  Frame
  { fTime        :: Double
  , fSamples     :: Map ByteString Double
  }

data Info =
  Info
  { iJob         :: ByteString
  , iDate        :: ByteString
  , iSampleUnit  :: ByteString
  , iValueUnit   :: ByteString
  , iSampleRange :: (Double, Double)
  , iValueRange  :: (Double, Double)
  , iSamples     :: [Double]
  , iValues      :: [(ByteString, [Double])]
  , iTrace       :: [Double]
  }

data Graph =
  Graph
  { gJob         :: ByteString
  , gDate        :: ByteString
  , gSampleUnit  :: ByteString
  , gValueUnit   :: ByteString
  , gSampleRange :: (Double, Double)
  , gValueRange  :: (Double, Double)
  , gSampleTicks :: [Double]
  , gValueTicks  :: [Double]
  , gLabels      :: [ByteString]
  , gBands       :: UArray (Int, Int) Double
  , gSamples     :: [Double]
  }
