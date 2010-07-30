module Main (main) where

import Prelude hiding (print, getContents, putStr)
import Data.ByteString.Char8 (getContents, putStr)

import Total (total)
import Prune (prune)
import Bands (bands)
import Pretty (pretty)
import Print (print)
import SVG (svg)

main :: IO ()
main = do
  input <- getContents
  let (header, totals) = total input
      keeps = prune totals
      (times, vals) = bands header keeps input
      ((sticks, vticks), (labels, coords)) = pretty header vals keeps
      outputs = print svg header sticks vticks labels times coords
  mapM_ putStr outputs
