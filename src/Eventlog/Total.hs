module Eventlog.Total(total) where

import Control.Monad.State.Strict (State(), execState, get, put, modify)
import Data.Map (Map, empty, alter)
import Prelude hiding (init, lookup, lines, words, drop, length, readFile)

import Eventlog.Types
import qualified Data.Vector as V
import Statistics.LinearRegression
import Debug.Trace


data Parse =
  Parse
  { totals    :: !(Map Bucket (Double, Double, [(Double, Double)])) -- compute running totals and total of squares
  , count     :: !Int                         -- number of frames
  , times     :: [Double]
  }

parse0 :: Parse
parse0 = Parse{ totals = empty, count = 0, times = [] }

total :: [Frame] -> (Int, Map Bucket (Double, Double, Maybe (Double, Double, Double)))
total fs =
  let parse1 = flip execState parse0 . mapM_ parseFrame $ fs
  in  (
       count parse1
      , fmap (stddev $ fromIntegral (count parse1)) (totals parse1)
      )


stddev :: Double -> (Double, Double, [(Double, Double)]) -> (Double, Double, Maybe (Double, Double, Double))
stddev s0 (s1, s2, samples) = traceShow (slope, xvect, yvect) (s1, sqrt (s0 * s2 - s1 * s1) / s0, slope)
  where
    m = maximum values
    mt = maximum timesv
    (timesv, values) = unzip (reverse samples)
    yvect = V.fromList (map (/ m) values)
    xvect = V.fromList (map (/ mt) timesv)
    slope = -- TODO: Distinguish these cases
            case samples of
              [] -> Nothing
              -- Linear regression is meaningless with 1 sample
              [_] -> Nothing
              _
                -- All values are the same leads to NaN r2
                | V.all (1 ==) yvect -> Nothing
                | otherwise -> Just $! linearRegressionRSqr xvect yvect




parseFrame :: Frame -> State Parse ()
parseFrame (Frame time ls) = do
  mapM_ (inserter time) ls
  modify $ \p -> p{ count = count p + 1 }

inserter :: Double -> Sample -> State Parse Double
inserter t (Sample k v) = do
  p <- get
  put $! p { totals = alter (accum t v) k (totals p) }
  return $! v

accum :: Double -> Double -> Maybe (Double, Double, [(Double, Double)]) -> Maybe (Double, Double, [(Double, Double)])
accum t x Nothing  = Just $! ((((,,) $! x) $! (x * x)) $! [(t, x)])
accum t x (Just (y, yy, ys)) = Just $! ((((,,) $! (x + y)) $! (x * x + yy)) $! (t, x):ys)
