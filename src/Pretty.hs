module Pretty where

import Data.List (transpose)

import Types

pretty :: Info -> Graph
pretty hpi =
  let sticks = uncurry (ticks 20) (hpiSampleRange hpi)
      vticks = uncurry (ticks 20) (hpiValueRange hpi)
      labels = "(trace elements)" : (reverse . map fst . hpiValues) hpi
      values = hpiTrace hpi : (reverse . map snd . hpiValues) hpi
      bands  = transpose . map (scanl (+) 0) . transpose $ values
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

ticks :: Int -> Double -> Double -> [Double]
ticks n mi ma =
  let k = nearestNice $ (ma - mi) / fromIntegral n
      m0 = fromIntegral (ceiling (mi / k)) * k
      m1 = fromIntegral (floor   (ma / k)) * k
  in  [m0, m0 + k .. m1 ]

nearestNice :: Double -> Double
nearestNice k0 = head . dropWhile (< k0) $ nices

nices :: [Double]
nices = [ f * k | f <- map (10**) [-6 ..], k <- [1,2,5] ]
