module Main where

a <<< b = reverse a ++ reverse b
a &&& b = zipWith (\x y -> [x, y]) a b
foo = {-# SCC "&<>!'\"/" #-} id

main = interact main'
main' s = foo $ (s <<< concat (s &&& s)) <<< concat (s &&& s) <<< s
