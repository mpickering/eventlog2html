module Main where

import Prelude hiding (print)

import Parse (parse)
import Process (process)
import Pretty (pretty)
import Print (print)

main :: IO ()
main = interact $ print . pretty . process . parse
