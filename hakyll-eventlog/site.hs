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
import           Eventlog.Data
import           Eventlog.Args
import           Eventlog.VegaTemplate
import           Eventlog.HtmlTemplate
import           Eventlog.Rendering.Types
import           Eventlog.Ticky
import           Text.Blaze.Html.Renderer.Text
import           Options.Applicative
import           Options.Applicative.Help.Types
import           Options.Applicative.Help.Core
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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
        saveSnapshot "snippet" =<< makeItem . T.unpack
                               =<< (unsafeCompiler $ eventlogSnippet globalCounter [T.pack path] exampleConf)
        makeItem res

    create ["examples.html"] $ do
        route idRoute
        compile $ do
            examples <- loadAllSnapshots "examples/*" "snippet"
            let widthCtx =
                    constField "width" (show $ (cwidth exampleConf + 60)) `mappend`
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
   d <- eventlogSnippet c (T.words code) (chartConfig attrs)
   return $ wrap (RawBlock (Format "html") d)
insertEventlogs _ (CodeBlock (_, ["help"], _) _) = insertHelp
insertEventlogs _ (c@(CodeBlock {})) = return $ Div ("", ["bg-light"],[]) [c]
insertEventlogs _ block = return block

wrap :: Block -> Block
wrap b = Div ("", ["card", "mx-auto"],[("style", "max-width: 600px")]) [Div ("", ["card-body"],[]) [b]]

render c = Div ("", ["bg-light"], []) [CodeBlock nullAttr ("> eventlog2html " <> c)]


eventlogSnippet :: IORef Int -> [T.Text] -> ChartConfig -> IO T.Text
eventlogSnippet c as conf = do
   n <- readIORef c
   modifyIORef c (+1)
   drawEventlog as (mkTabID (show n)) conf

drawEventlog :: [T.Text] -> TabID -> ChartConfig -> IO T.Text
drawEventlog args vid conf  = do
  let final_args = ["--no-include-js"] ++ args
  Run as <- handleParseResult (execParserPure defaultPrefs argsInfo (map T.unpack final_args))
  ty <- generateJson (head $ files as) as
  return $ case eventlogHeapProfile ty of
    Just (HeapProfileData dat _ _) ->
      let itd = if traces conf then TraceData else NoTraceData
      in TL.toStrict $ renderHtml $ renderChartWithJson itd (chartType conf) vid dat (vegaJsonText conf)
    Nothing -> mempty

def :: ChartConfig
def = ChartConfig 600 500 True "category20" "set1" (AreaChart Stacked) Nothing

exampleConf :: ChartConfig
exampleConf = ChartConfig 500 300 False "category20" "set1" (AreaChart Stacked) Nothing

chartConfig :: [(T.Text, T.Text)] -> ChartConfig
chartConfig as = foldr go def as
  where
    tread :: Read a => T.Text -> a
    tread = read . T.unpack
    go :: (T.Text, T.Text) -> ChartConfig -> ChartConfig
    go (k,v) c = case k of
                   "w"      -> c { cwidth  = tread v }
                   "h"      -> c { cheight = tread v }
                   "traces" -> c { traces = tread v }
                   "type"   -> c { chartType = readChart v }
                   "scheme" -> c { colourScheme = v }

    readChart "stack"  = AreaChart Stacked
    readChart "stream" = AreaChart StreamGraph
    readChart "normal" = AreaChart Normalized
    readChart "line"   = LineChart
    readChart e = error $ "Unknown chart type: "  ++ (T.unpack e)

insertHelp :: IO Block
insertHelp =
  return $ Div ("", ["bg-light"], []) [CodeBlock nullAttr (base_help argsInfo)]
  where
    base_help :: ParserInfo a -> T.Text
    base_help i =
      T.pack $ renderHelp 80 (mconcat [h, f, parserHelp defaultPrefs (infoParser i)])
      where
        h = headerHelp (infoHeader i)
        f = footerHelp (infoFooter i)

fullEventLogPage :: FilePath -> IO String
fullEventLogPage file = do
  -- The examples have been generated with "traceEvent" instead of "traceMarker",
  -- so we have to explicitly include these traces.
  -- In the future we can replace these examples but for now this is more
  -- convenient.
  Run as <- handleParseResult (execParserPure defaultPrefs argsInfo
          [file, "--no-include-js", "--include-trace-events", "--limit-detailed=100"])
  ty <- generateJson file as
  return $ templateString ty as
