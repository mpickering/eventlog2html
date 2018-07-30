module Args (args, Args(..), Uniform(..), Sort(..), KeyPlace(..)) where

import Options.Applicative
import Data.Semigroup ((<>))

data Uniform = None | Time | Memory | Both deriving (Eq)
data Sort = Size | StdDev | Name
data KeyPlace = Inline | File FilePath

data Args = Args
  { uniformity   :: Uniform
  , sorting      :: Sort
  , keyPlace     :: KeyPlace
  , reversing    :: Bool
  , tracePercent :: Double
  , nBands       :: Int
  , patterned    :: Bool
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
         <> help "Whether to embed the key in the image output.  One of: inline (default), FILE.html.  Use - for standard output and ./inline for a file named literally \"inline\"."
         <> value Inline
         <> metavar "KEY" )
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
      <*> some (argument str
          ( help "Heap profiles (FILE.hp will be converted to FILE.svg)."
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
  "inline" -> Right Inline
  "" -> Left "expected one of: inline, FILE.html"
  f -> Right (File f)


args :: IO Args
args = execParser opts
  where
    opts = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Convert heap profile FILES.hp to pretty graphs FILES.svg"
     <> header "hp2pretty - generate pretty graphs from heap profiles" )
