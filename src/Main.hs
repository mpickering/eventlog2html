module Main (main) where

import Prelude hiding (print, getContents, putStr, interact)
import Data.ByteString.Lazy.Char8 (ByteString, getContents, putStr)

import Parse (parse)
import Process (process)
import Pretty (pretty)
import Print (print)
import SVG (svg)

main :: IO ()
main = interact $ print svg . pretty . process . parse

interact :: (ByteString -> [ByteString]) -> IO ()
interact f = getContents >>= mapM_ putStr . f
