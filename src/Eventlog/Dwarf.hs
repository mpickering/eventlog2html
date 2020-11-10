module Eventlog.Dwarf
  ( getDwarfInfo
  , lookupDwarf
  , lookupSourceInfo
  , SourceInfo(..)
  , printFileSnippet
  , InfoTablePtr(..)
  ) where

import Control.Monad
--import qualified Data.HashMap.Strict as HM
import System.IO
import Data.Word
import System.Endian
import Data.Foldable
import Eventlog.Types


import qualified Data.Dwarf as Dwarf
--import qualified Data.Dwarf.ADT.Pretty as DwarfPretty
import qualified Data.Dwarf.Elf as Dwarf.Elf

import Data.Dwarf
import Data.Dwarf.ADT
import qualified Data.Text  as T
import System.FilePath
import System.Directory
import Text.Printf
import Numeric
import Debug.Trace
import qualified System.IO.Strict as S


getDwarfInfo :: FilePath -> IO Dwarf
getDwarfInfo fn = do
 (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT Dwarf.LittleEndian fn
-- (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT Dwarf.LittleEndian "/home/matt/ghc/inplace/lib/bin/ghc-stage2"
-- (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT Dwarf.LittleEndian ("/home/matt/eventlog2html/dist-newstyle/build/x86_64-linux/ghc-8.8.1/eventlog2html-0.7.0/x/eventlog2html/build/eventlog2html/eventlog2html")
-- mapM_ print warnings
-- print $ DwarfPretty.dwarf dwarf
 return dwarf


data SourceInfo = SourceInfo FilePath (Int, Int) !([(Int, String)])

lookupSourceInfo :: FilePath -> Dwarf -> InfoTablePtr -> IO (Maybe SourceInfo)
lookupSourceInfo prog_fp d itp = do
  let res = lookupDwarf d itp
  case res of
    Nothing -> return Nothing
    Just pinfo -> getFileSnippet prog_fp pinfo

lookupDwarf :: Dwarf -> InfoTablePtr -> Maybe ([FilePath], Int, Int)
lookupDwarf d (InfoTablePtr w) = do
  let (Dwarf units) = d
  traceShowM (length units)
  asum (map (lookupDwarfUnit (fromBE64 w)) units)

lookupDwarfUnit :: Word64 -> Boxed CompilationUnit -> Maybe ([FilePath], Int, Int)
lookupDwarfUnit w (Boxed _ cu) = do
--  traceShowM (cuName cu, cuLowPc cu, cuHighPc cu, w)
  low <- cuLowPc cu
  high <- cuHighPc cu
  guard (low <= w && w <= high)
  (LNE ds fs ls) <- cuLineNumInfo cu
  (fp, l, c) <- foldl' (lookupDwarfLine w) Nothing (zip ls (tail ls))
  let res_fps = if null ds then [T.unpack (cuCompDir cu) </> fp]
                           else map (\d -> T.unpack (cuCompDir cu) </> T.unpack d </> fp) ds
  return ( res_fps
         , l , c)

lookupDwarfSubprogram :: Word64 -> Boxed Def -> Maybe Subprogram
lookupDwarfSubprogram w (Boxed _ (DefSubprogram s)) = do
  low <- subprogLowPC s
  high <- subprogHighPC s
  guard (low <= w && w <= high)
  return s
lookupDwarfSubprogram _ _ = Nothing

lookupDwarfLine :: Word64
                -> Maybe (FilePath, Int, Int)
                -> (Dwarf.DW_LNE, Dwarf.DW_LNE)
                -> Maybe (FilePath, Int, Int)
lookupDwarfLine w Nothing (d, nd) =
  if lnmAddress d <= w && w <= lnmAddress nd
    then do
      let (LNEFile file _ _ _) = lnmFiles nd !! (fromIntegral (lnmFile nd) - 1)
      Just
        (T.unpack file, fromIntegral (lnmLine nd), fromIntegral (lnmColumn nd))
    else Nothing
lookupDwarfLine _ (Just r) _ =  Just r

getFileSnippet :: FilePath -> ([FilePath], Int, Int) -> IO (Maybe SourceInfo)
getFileSnippet d_fp (fps, l, c) = go fps
  where
    go [] = return Nothing
    go (fp: fps) = do
      exists <- doesFileExist fp
      -- get file modtime
      if not exists
        then go fps
        else do
          fp `warnIfNewer` d_fp
          src <- zip [1..] . lines <$> S.readFile fp
          let ctx = take 10 (drop (max (l - 5) 0) src)
          return (Just $ SourceInfo fp (l, c) ctx)


-- For debugging
printFileSnippet :: SourceInfo -> IO ()
printFileSnippet (SourceInfo file (l, c) ctx) = do
  putStrLn (file <> ":" <> show l <> ":" <> show c)
  mapM_ (\(n, l) ->
    let sn = show n
    in putStrLn (sn <> replicate (5 - length sn) ' ' <> l)) ctx

-- | Print a warning if source file (first argument) is newer than the binary (second argument)
warnIfNewer :: FilePath -> FilePath -> IO ()
warnIfNewer fpSrc fpBin = do
    modTimeSource <- getModificationTime fpSrc
    modTimeBinary <- getModificationTime fpBin

    when (modTimeSource > modTimeBinary) $
      hPutStrLn stderr $
        printf "Warning: %s is newer than %s. Code snippets might be wrong!"
        fpSrc
        fpBin
