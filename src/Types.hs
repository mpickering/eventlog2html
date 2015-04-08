module Types where

import Data.Text (Text)

data Header =
  Header
  { hJob         :: Text
  , hDate        :: Text
  , hSampleUnit  :: Text
  , hValueUnit   :: Text
  , hSampleRange :: (Double, Double)
  , hValueRange  :: (Double, Double)
  , hCount       :: Int
  }
