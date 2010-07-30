module Types where

import Data.ByteString.Char8 (ByteString)

data Header =
  Header
  { hJob         :: ByteString
  , hDate        :: ByteString
  , hSampleUnit  :: ByteString
  , hValueUnit   :: ByteString
  , hSampleRange :: (Double, Double)
  , hValueRange  :: (Double, Double)
  , hCount       :: Int
  }
