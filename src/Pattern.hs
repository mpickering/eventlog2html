module Pattern (patterns, Pattern) where

import Data.Tuple (swap)

type Point = (Double, Double)

type Pattern = [(Point, Point)]

loop :: [Point] -> [(Point, Point)]
loop ps = zip ps (tail (cycle ps))

patterns :: [Pattern]
patterns =
  -- short and long dashes in 4 directions
  [ [ ((5, 8), (11, 8)) ]
  , [ ((3, 8), (13, 8)) ]
  , [ ((8, 5), (8, 11)) ]
  , [ ((8, 3), (8, 13)) ]
  , [ ((5, 5), (11, 11)) ]
  , [ ((3, 3), (13, 13)) ]
  , [ ((5, 11), (11, 5)) ]
  , [ ((3, 13), (13, 3)) ]
  -- small and large crosses in 2 directions
  , [ ((5, 8), (11, 8)), ((8, 5), (8, 11)) ]
  , [ ((3, 8), (13, 8)), ((8, 3), (8, 13)) ]
  , [ ((5, 5), (11, 11)), ((5, 11), (11, 5)) ]
  , [ ((3, 3), (13, 13)), ((3, 13), (13, 3)) ]
  , [ ((3, 8), (13, 8)), ((8, 3), (8, 13)), ((3, 3), (13, 13)), ((3, 13), (13, 3)) ]
  , [ ((5, 8), (11, 8)), ((8, 5), (8, 11)), ((5, 5), (11, 11)), ((5, 11), (11, 5)) ]
  -- small and large triangles in 4 directions
  , loop [ (5, 5), (11, 5), (8, 11) ]
  , loop [ (3, 3), (13, 3), (8, 13) ]
  , loop [ (5, 11), (11, 11), (8, 5) ]
  , loop [ (3, 13), (13, 13), (8, 3) ]
  , loop (map swap [ (5, 5), (11, 5), (8, 11) ])
  , loop (map swap [ (3, 3), (13, 3), (8, 13) ])
  , loop (map swap [ (5, 11), (11, 11), (8, 5) ])
  , loop (map swap [ (3, 13), (13, 13), (8, 3) ])
  -- small and large squares in 2 directions
  , loop [ (5, 5), (11, 5), (11, 11), (5, 11) ]
  , loop [ (3, 3), (13, 3), (13, 13), (3, 13) ]
  , loop [ (5, 8), (8, 5), (11, 8), (8, 11) ]
  , loop [ (3, 8), (8, 3), (13, 8), (8, 13) ]
  ]
