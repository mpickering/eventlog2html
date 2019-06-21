--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import           Control.Applicative
import           Control.Monad
import           Data.List           (delete)
import           Data.List.Split
import           Data.Maybe
import           System.Directory    (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import           System.Exit         (ExitCode (..))
import           System.FilePath     ((<.>), (</>), pathSeparator)
import           System.IO           (hPutStrLn, stderr)
import           System.Process
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON
import           Data
import           Args
import           VegaTemplate
import           HtmlTemplate
import           Text.Blaze.Html.Renderer.String
import           Options.Applicative
import           Options.Applicative.Help.Types
import           Options.Applicative.Help.Core
import           Data.IORef

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "examples/*.eventlog" $ do
      route $ customRoute $ (++ ".html") . toFilePath
      compile $ do
        path <- getResourceFilePath
        res <- unsafeCompiler $ fullEventLogPage path
        makeItem res

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions eventlogTransformer
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

eventlogTransformer :: Pandoc -> Compiler Pandoc
eventlogTransformer pandoc =
  unsafeCompiler $ renderEventlog pandoc


data Echo = Above | Below
   deriving (Read, Show)

renderEventlog :: Pandoc -> IO Pandoc
renderEventlog p = do
  counter <- newIORef 0
  bottomUpM (insertEventlogs counter) p

insertEventlogs :: IORef Int -> Block -> IO Block
insertEventlogs c block@(CodeBlock (ident, classes, attrs) code) | "eventlog" `elem` classes = do
   n <- readIORef c
   modifyIORef c (+1)
   let cid = "viz" ++ show n
   d <- drawEventlog (words code) n attrs
   return (RawBlock (Format "html") d)
insertEventlogs _ (CodeBlock (_, ["help"], _) _) = insertHelp
insertEventlogs _ (c@(CodeBlock {})) = return $ Div ("", ["bg-light"],[]) [c]
insertEventlogs _ block = return block


render c = Div ("", ["bg-light"], []) [CodeBlock nullAttr ("> eventlog2html " ++ c)]

drawEventlog :: [String] -> Int -> [(String, String)] -> IO String
drawEventlog args vid attrs = do
  as <- handleParseResult (execParserPure defaultPrefs argsInfo args)
  dat <- generateJson (head $ files as) as
  return $ renderHtml $ renderChartWithJson vid dat (vegaJsonText (chartConfig attrs))

chartConfig :: [(String, String)] -> ChartConfig
chartConfig as = foldr go def as
  where
    def = ChartConfig 600 500 True (AreaChart Stacked)

    go :: (String, String) -> ChartConfig -> ChartConfig
    go (k,v) c = case k of
                   "w"      -> c { cwidth  = read v }
                   "h"      -> c { cheight = read v }
                   "traces" -> c { traces = read v }
                   "type"   -> c { chartType = readChart v }

    readChart "stack"  = AreaChart Stacked
    readChart "stream" = AreaChart StreamGraph
    readChart "normal" = AreaChart Normalized
    readChart "line"   = LineChart
    readChart e = error $ "Unknown chart type: "  ++ e

insertHelp :: IO Block
insertHelp =
  return $ Div ("", ["bg-light"], []) [CodeBlock nullAttr (base_help argsInfo)]
  where
    base_help :: ParserInfo a -> String
    base_help i =
      renderHelp 80 (mconcat [h, f, parserHelp defaultPrefs (infoParser i)])
      where
        h = headerHelp (infoHeader i)
        f = footerHelp (infoFooter i)

fullEventLogPage :: FilePath -> IO String
fullEventLogPage file = do
  as <- handleParseResult (execParserPure defaultPrefs argsInfo [file])
  data_json <- generateJson file as
  return $ templateString data_json as

