module Main where

a <<< b = {-# SCC "ThisSCCHasAnExtremelyLongNameThatMightBeChoppedOffPrematurely" #-} reverse a ++ reverse b
a &&& b = zipWith (\x y -> {-# SCC "ThisSCCHasAnotherExtremelyLongNameThatMightBeChoppedOffPrematurely" #-} [x, y]) a b
foo = id

main = interact main'
main' s = foo $ (s <<< concat (s &&& s)) <<< concat (s &&& s) <<< s
