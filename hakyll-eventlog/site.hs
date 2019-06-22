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
import qualified Data.Text as T

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Each chart needs a unique ID
  globalCounter <- newIORef 0
  hakyll $ do
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
        saveSnapshot "snippet" =<< makeItem
                               =<< (unsafeCompiler $ eventlogSnippet globalCounter [path] exampleConf)
        makeItem res

    create ["examples.html"] $ do
        route idRoute
        compile $ do
            examples <- loadAllSnapshots "examples/*" "snippet"
            let widthCtx =
                    constField "width" (show $ (cwidth exampleConf + 50)) `mappend`
                    defaultContext
            let examplesCtx =
                    listField "examples" widthCtx (return examples) `mappend`
                    constField "title" "Examples"            `mappend`
                    defaultContext


            makeItem ""
                >>= loadAndApplyTemplate "templates/examples.html" examplesCtx
                >>= loadAndApplyTemplate "templates/default.html" examplesCtx
                >>= relativizeUrls


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
   d <- eventlogSnippet c (words code) (chartConfig attrs)
   return $ wrap (RawBlock (Format "html") d)
insertEventlogs _ (CodeBlock (_, ["help"], _) _) = insertHelp
insertEventlogs _ (c@(CodeBlock {})) = return $ Div ("", ["bg-light"],[]) [c]
insertEventlogs _ block = return block

wrap :: Block -> Block
wrap b = Div ("", ["card", "mx-auto"],[("style", "max-width: 600px")]) [Div ("", ["card-body"],[]) [b]]

render c = Div ("", ["bg-light"], []) [CodeBlock nullAttr ("> eventlog2html " ++ c)]


eventlogSnippet :: IORef Int -> [String] -> ChartConfig -> IO String
eventlogSnippet c as conf = do
   n <- readIORef c
   modifyIORef c (+1)
   drawEventlog as n conf

drawEventlog :: [String] -> Int -> ChartConfig -> IO String
drawEventlog args vid conf  = do
  as <- handleParseResult (execParserPure defaultPrefs argsInfo args)
  (_, dat) <- generateJson (head $ files as) as
  return $ renderHtml $ renderChartWithJson vid dat (vegaJsonText conf)

def :: ChartConfig
def = ChartConfig 600 500 True "category20" (AreaChart Stacked)

exampleConf :: ChartConfig
exampleConf = ChartConfig 500 300 False "category20" (AreaChart Stacked)

chartConfig :: [(String, String)] -> ChartConfig
chartConfig as = foldr go def as
  where

    go :: (String, String) -> ChartConfig -> ChartConfig
    go (k,v) c = case k of
                   "w"      -> c { cwidth  = read v }
                   "h"      -> c { cheight = read v }
                   "traces" -> c { traces = read v }
                   "type"   -> c { chartType = readChart v }
                   "scheme" -> c { colourScheme = T.pack v }

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
  (header, data_json) <- generateJson file as
  return $ templateString header data_json as

