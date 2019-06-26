{-# LANGUAGE OverloadedStrings #-}
module Eventlog.Args
  (
    args
  , argsInfo
  , Args(..)
  , Sort(..)
  ) where

import Options.Applicative
import Data.Text (Text)
import Data.Semigroup ((<>))
import Control.Applicative (optional)

data Sort = Size | StdDev | Name

data Args = Args
  {
    sorting      :: Sort
  , reversing    :: Bool
  , tracePercent :: Double
  , nBands       :: Int
  , heapProfile  :: Bool
  , noIncludejs    :: Bool
  , json         :: Bool
  , noTraces     :: Bool
  , userColourScheme :: Text
  , outputFile :: Maybe String
  , files        :: [String]
  }

argParser :: Parser Args
argParser = Args
      <$> option parseSort
          ( long "sort"
         <> help "How to sort the bands.  One of: size (default), stddev, name."
         <> value Size
         <> metavar "FIELD" )
      <*> switch
          ( long "reverse"
         <> help "Reverse the order of bands." )
      <*> option auto
          ( long "trace"
         <> help "Percentage of trace elements to combine."
         <> value 1
         <> showDefault
         <> metavar "PERCENT" )
      <*> option auto
          ( long "bands"
         <> help "Maximum number of bands to draw (0 for unlimited)."
         <> value 15
         <> showDefault
         <> metavar "COUNT" )
      <*> switch
          ( long "heap-profile"
          <> short 'p'
          <> help "Input files are .hp heap profiles.")
      <*> switch
          (long "no-include-js"
          <> help "Fetch the javascript from a CDN rather than bundling it into the file.")
      <*> switch
          ( long "json"
          <> short 'j'
          <> help "Output JSON")
      <*> switch
          ( long "no-traces"
          <> help "Don't display traces on chart")
      <*> option str
          ( long "colour-scheme"
          <> value "category20b"
          <> help "The name of the colour scheme. See the vega documentation (https://vega.github.io/vega/docs/schemes/#reference) for a complete list. Examples include \"category10\" \"dark2\" \"tableau10\". ")
      <*> (optional $ option str
          (short 'o'
          <> help "Write the output to the given filename."
          <> metavar "OUTFILE"))
      <*> some (argument str
          ( help "Eventlogs (FILE.eventlog will be converted to FILE.html)."
         <> metavar "FILES..." ))

parseSort :: ReadM Sort
parseSort = eitherReader $ \s -> case s of
  "size" -> Right Size
  "stddev" -> Right StdDev
  "name" -> Right Name
  _ -> Left "expected one of: size, stddev, name"

args :: IO Args
args = execParser argsInfo

argsInfo :: ParserInfo Args
argsInfo = opts
  where
    opts = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Convert eventlogs FILES.eventlog to interactive FILES.html"
     <> header "eventlog2html - generate interactive html from eventlogs" )
