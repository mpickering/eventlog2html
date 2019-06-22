module Types where

import Data.Text (Text)

data Header =
  Header
  { hJob         :: Text
  , hDate        :: Text
  , hSampleUnit  :: Text
  , hValueUnit   :: Text
  , hCount       :: Int
  }

type PartialHeader = Int -> Header

data Sample = Sample Text Double deriving Show

data Frame = Frame Double [Sample] deriving Show

-- | A trace we also want to show on the graph
data Trace = Trace Double Text deriving Show
