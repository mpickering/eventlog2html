module Args
  (
    args
  , argsInfo
  , Args(..)
  , Uniform(..)
  , Sort(..)
  , KeyPlace(..)
  , TitlePlace(..)
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Uniform = None | Time | Memory | Both deriving (Eq)
data Sort = Size | StdDev | Name
data KeyPlace = KeyInline | KeyFile FilePath
data TitlePlace = TitleInline | TitleFile FilePath

data Args = Args
  { uniformity   :: Uniform
  , sorting      :: Sort
  , keyPlace     :: KeyPlace
  , titlePlace   :: TitlePlace
  , reversing    :: Bool
  , tracePercent :: Double
  , nBands       :: Int
  , patterned    :: Bool
  , heapProfile  :: Bool
  , test         :: Bool
  , includejs    :: Bool
  , json         :: Bool
  , files        :: [String]
  }

argParser :: Parser Args
argParser = Args
      <$> option parseUniform
          ( long "uniform-scale"
         <> help "Whether to use a uniform scale for all outputs.  One of: none (default), time, memory, both."
         <> value None
         <> metavar "AXES" )
      <*> option parseSort
          ( long "sort"
         <> help "How to sort the bands.  One of: size (default), stddev, name."
         <> value Size
         <> metavar "FIELD" )
      <*> option parseKey
          ( long "key"
         <> help "Whether to embed the key in the image output.  One of: inline (default), FILE.txt.  Use - for standard output and ./inline for a file named literally \"inline\"."
         <> value KeyInline
         <> metavar "KEY" )
      <*> option parseTitle
          ( long "title"
         <> help "Whether to embed the title in the image output.  One of: inline (default), FILE.txt.  Use - for standard output and ./inline for a file named literally \"inline\"."
         <> value TitleInline
         <> metavar "TITLE" )
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
          ( long "pattern"
         <> help "Use patterns instead of solid colours to fill bands." )
      <*> switch
          ( long "heap-profile"
          <> short 'p'
          <> help "Input files are .hp heap profiles.")
      <*> switch
          (short 't')
      <*> switch
          (long "include-js"
          <> help "Include the javascript into the generated HTML instead of fetching it from a CDN.")
      <*> switch
          ( long "json"
          <> short 'j'
          <> help "Output JSON")
      <*> some (argument str
          ( help "Eventlogs (FILE.eventlog will be converted to FILE.html)."
         <> metavar "FILES..." ))


parseUniform :: ReadM Uniform
parseUniform = eitherReader $ \s -> case s of
  "none" -> Right None
  "time" -> Right Time
  "memory" -> Right Memory
  "both" -> Right Both
  _ -> Left "expected one of: none, time, memory, both"

parseSort :: ReadM Sort
parseSort = eitherReader $ \s -> case s of
  "size" -> Right Size
  "stddev" -> Right StdDev
  "name" -> Right Name
  _ -> Left "expected one of: size, stddev, name"

parseKey :: ReadM KeyPlace
parseKey = eitherReader $ \s -> case s of
  "inline" -> Right KeyInline
  "" -> Left "expected one of: inline, FILE.txt"
  f -> Right (KeyFile f)

parseTitle :: ReadM TitlePlace
parseTitle = eitherReader $ \s -> case s of
  "inline" -> Right TitleInline
  "" -> Left "expected one of: inline, FILE.txt"
  f -> Right (TitleFile f)

args :: IO Args
args = execParser argsInfo

argsInfo :: ParserInfo Args
argsInfo = opts
  where
    opts = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Convert eventlogs FILES.eventlog to interactive FILES.html"
     <> header "eventlog2html - generate interactive html from eventlogs" )
