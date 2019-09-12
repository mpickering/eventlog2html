{-# LANGUAGE BangPatterns #-}
module Eventlog.Total (total) where

import Control.Monad.State.Strict (State(), execState, get, put, modify)
import Data.Map (Map, empty, alter)
import Prelude hiding (init, lookup, lines, words, drop, length, readFile)
import Data.Text (Text)

import Eventlog.Types


data Parse =
  Parse
  { totals    :: !(Map Text (Double, Double)) -- compute running totass and total of squares
  , count     :: !Int                         -- number of frames
  }

parse0 :: Parse
parse0 = Parse{ totals = empty, count = 0 }

total :: [Frame] -> (Int, Map Text (Double, Double))
total fs =
  let parse1 = flip execState parse0 . mapM_ parseFrame $ fs
  in  (
       count parse1
      , fmap (stddev $ fromIntegral (count parse1)) (totals parse1)
      )

stddev :: Double -> (Double, Double) -> (Double, Double)
stddev s0 (s1, s2) = (s1, sqrt (s0 * s2 - s1 * s1) / s0)


parseFrame :: Frame -> State Parse ()
parseFrame (Frame _time ls) = do
  _ <- mapM inserter ls
  modify $ \p -> p{ count = count p + 1 }

inserter :: Sample -> State Parse Double
inserter (Sample k v) = do
  p <- get
  put $! p { totals = alter (accum  v) k (totals p) }
  return $! v

accum :: Double -> Maybe (Double, Double) -> Maybe (Double, Double)
accum x Nothing  = Just $! (((,) $! x) $! (x * x))
accum x (Just (y, yy)) = Just $! (((,) $! (x + y)) $! (x * x + yy))
