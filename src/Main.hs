module Main (main) where

import Prelude hiding (print, interact)
import Data.ByteString.Lazy.Char8 (interact)

import Parse (parse)
import Process (process)
import Pretty (pretty)
import Print (print)

main :: IO ()
main = interact $ print . pretty . process . parse
