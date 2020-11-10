{-# LANGUAGE OverloadedStrings #-}
-- Warning turned off for CI
{-# OPTIONS_GHC -Wwarn=unused-imports #-}
module Eventlog.Args
  (
    args
  , argsInfo
  , Args(..)
  , Sort(..)
  ) where

import Options.Applicative
import Data.Text (Text)
-- Used for GHC 8.6.5
import Data.Semigroup ((<>))
import Control.Applicative (optional)

data Sort = Size | StdDev | Name | Gradient

data Args = Args
  {
    sorting      :: Sort
  , reversing    :: Bool
  , nBands       :: Int
  , heapProfile  :: Bool
  , noIncludejs    :: Bool
  , json         :: Bool
  , noTraces     :: Bool
  , traceEvents  :: Bool -- ^ By default, only traceMarkers are included.
                         -- This option enables the inclusion of traceEvents.
  , userColourScheme :: Text
  , fixedYAxis :: Maybe Int
  , includeStr :: [Text]
  , excludeStr :: [Text]
  , outputFile :: Maybe String
  , files        :: [String]
  }

argParser :: Parser Args
argParser = Args
      <$> option parseSort
          ( long "sort"
         <> help "How to sort the bands.  One of: size (default), stddev, name, gradient."
         <> value Size
         <> metavar "FIELD" )
      <*> switch
          ( long "reverse"
         <> help "Reverse the order of bands." )
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
      <*> switch
          ( long "include-trace-events"
          <> help ("Enables the inclusion of traces emitted using `traceEvent`"
                   ++ ", which should only be used for high-frequency events. "
                   ++ "For low frequency events, use `traceMarker` instead.")
          <> showDefault)
      <*> option str
          ( long "colour-scheme"
          <> value "category20b"
          <> help "The name of the colour scheme. See the vega documentation (https://vega.github.io/vega/docs/schemes/#reference) for a complete list. Examples include \"category10\" \"dark2\" \"tableau10\". ")
      <*> option (Just <$> auto)
          ( long "y-axis"
          <> value Nothing
          <> help "Fixed value for the maximum extent of the y-axis in bytes. This option is useful for comparing profiles together.")
      <*> many (option str
          (short 'i'
          <> long "include"
          <> help ("Specify the traces which should be included in the output. Only traces which contain SUBSTRING "
                    ++ "in their name will be included. Multiple different traces can be included "
                    ++ "with \"-i foo -i bar\".")
          <> metavar "SUBSTRING"))
      <*> many (option str
          (short 'x'
          <> long "exclude"
          <> help ("Specify the traces which should be excluded in the output. All traces which contain SUBSTRING "
                    ++ "in their name will be excluded. Multiple different traces can be excluded "
                    ++ "with \"-x foo -x bar\".")
          <> metavar "SUBSTRING"))
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
  "gradient" -> Right Gradient
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
