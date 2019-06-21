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

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions eventlogTransformer
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

eventlogTransformer :: Pandoc -> Compiler Pandoc
eventlogTransformer pandoc = unsafeCompiler $ renderEventlog pandoc


data Echo = Above | Below
   deriving (Read, Show)

renderEventlog :: Pandoc -> IO Pandoc
renderEventlog p = bottomUpM insertEventlogs p

insertEventlogs :: Block -> IO Block
insertEventlogs block@(CodeBlock (ident, classes, attrs) code) | "eventlog" `elem` classes = do
   d <- drawEventlog (words code)
   return (Div nullAttr [render code, RawBlock (Format "html") d])
insertEventlogs block = print block >> return block

render c = CodeBlock nullAttr ("> eventlog2html " ++ c)

drawEventlog :: [String] -> IO String
drawEventlog args = do
  as <- handleParseResult (execParserPure defaultPrefs argsInfo args)
  dat <- generateJson (head $ files as) as
  return $ renderHtml $ renderChartWithJson dat (vegaJsonText (AreaChart Stacked))

