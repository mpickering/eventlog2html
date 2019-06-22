{-# LANGUAGE BangPatterns #-}
module Total (total) where

import Control.Monad.State.Strict (State(), execState, get, put, modify)
import Data.List (foldl')
import Data.Map (Map, empty, lookup, insert, alter)
import Prelude hiding (init, lookup, lines, words, drop, length, readFile)
import Data.Text (Text)

import Types


data Parse =
  Parse
  { symbols   :: !(Map Text Text) -- intern symbols to save RAM
  , totals    :: !(Map Text (Double, Double)) -- compute running totass and total of squares
  , count     :: !Int                         -- number of frames
  }

parse0 :: Parse
parse0 = Parse{ symbols = empty, totals = empty, count = 0 }

total :: PartialHeader -> [Frame] -> (Header, Map Text (Double, Double))
total ph fs =
  let parse1 = flip execState parse0 . mapM_ parseFrame $ fs
  in  (
       ph (count parse1)
      , fmap (stddev $ fromIntegral (count parse1)) (totals parse1)
      )

stddev :: Double -> (Double, Double) -> (Double, Double)
stddev s0 (s1, s2) = (s1, sqrt (s0 * s2 - s1 * s1) / s0)


parseFrame :: Frame -> State Parse ()
parseFrame (Frame _time ls) = do
  samples <- mapM inserter ls
  modify $ \p -> p{ count = count p + 1 }

inserter :: Sample -> State Parse Double
inserter (Sample k v) = do
  p <- get
  k' <- case lookup k (symbols p) of
    Nothing -> do
      put $! p{ symbols = insert k k (symbols p) }
      return k
    Just kk -> return kk
  p' <- get
  put $! p'{ totals = alter (accum  v) k' (totals p') }
  return $! v

accum :: Double -> Maybe (Double, Double) -> Maybe (Double, Double)
accum x Nothing  = Just $! (((,) $! x) $! (x * x))
accum x (Just (y, yy)) = Just $! (((,) $! (x + y)) $! (x * x + yy))
