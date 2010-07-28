module Pretty where

import Control.Monad (forM_, liftM2)
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.ByteString.Lazy.Char8 (pack)

import Types

pretty :: Info -> Graph
pretty hpi =
  let sticks = uncurry (ticks 20) (hpiSampleRange hpi)
      vticks = uncurry (ticks 20) (hpiValueRange hpi)
      labels = pack "(trace elements)" : (reverse . map fst . hpiValues) hpi
      values = hpiTrace hpi : (reverse . map snd . hpiValues) hpi
      bands  = accumulate values
  in  Graph
      { hpgJob        = hpiJob hpi
      , hpgDate       = hpiDate hpi
      , hpgSampleUnit = hpiSampleUnit hpi
      , hpgValueUnit  = hpiValueUnit hpi
      , hpgSampleRange= hpiSampleRange hpi
      , hpgValueRange = hpiValueRange hpi
      , hpgSampleTicks= sticks
      , hpgValueTicks = vticks
      , hpgLabels     = labels
      , hpgBands      = bands
      , hpgSamples    = hpiSamples hpi
      }

accumulate :: [[Double]] -> UArray (Int, Int) Double
accumulate [] = error $ "accumulate': empty"
accumulate xss@(x:_) = runSTUArray $ do
  let bands   = length xss
      samples = length x
  a <- newListArray ((0,1),(bands,samples)) $ replicate samples 0 ++ concat xss
  forM_ [1 .. samples] $ \s ->
    forM_ [1 .. bands] $ \b ->
      writeArray a (b, s) =<< liftM2 (+) (readArray a (b - 1, s)) (readArray a (b, s))
  return a

ticks :: Int -> Double -> Double -> [Double]
ticks n mi ma =
  let k = nearestNice $ (ma - mi) / fromIntegral n
      m0 = fromIntegral (ceiling (mi / k) :: Integer) * k
      m1 = fromIntegral (floor   (ma / k) :: Integer) * k
  in  [m0, m0 + k .. m1 ]

nearestNice :: Double -> Double
nearestNice k0 = head . dropWhile (< k0) $ nices

nices :: [Double]
nices = [ f * k | f <- map (10**) [-6 ..], k <- [1,2,5] ]
