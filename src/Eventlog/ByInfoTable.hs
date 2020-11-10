{-# LANGUAGE OverloadedStrings #-}
module Eventlog.ByInfoTable where

import Eventlog.Dwarf
import qualified Data.Map as Map
import Eventlog.Types
import Numeric
import qualified Data.Text as T
import System.Endian
import Data.Maybe

import Data.Aeson
import Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Word


mkClosureInfo :: FilePath
              -> Map.Map Bucket a
              -> Map.Map InfoTablePtr InfoTableLoc
              -> IO (Map.Map Bucket InfoTableLoc)
mkClosureInfo fp b ipes = do
--  d <- getDwarfInfo fp
  let itps = map toItblPointer (Map.keys b)
  print (take 5 (zip (Map.keys b) itps))
  print $ length (Map.keys b)
  print $ (Map.size ipes)
  print $ (take 100 $ Map.keys ipes)
  --res <- Map.traverseMaybeWithKey (\k _ -> lookupSourceInfo fp d (toItblPointer k)) b
  res <- Map.traverseMaybeWithKey (\k _ -> return $ Map.lookup (toItblPointer k) ipes) b
  print (Map.size res)
--  print $ length (catMaybes ress)
--  print (head ress)
  return res


{-
renderClosureInfo :: Map.Map Bucket SourceInfo -> Html
renderClosureInfo cs = do
  H.dl $ Map.foldrWithKey (\k a res -> renderEntry k a >> res) mempty cs
  where
    renderEntry (Bucket k) (SourceInfo f (l, c) ctx) = do
        H.dt (H.b (toHtml k) <> toHtml (":" <> T.pack f <> ":" <> T.pack (show l) <> ":" <> T.pack (show c)))
        H.dd (H.pre (H.code ((preEscapedToHtml (unlines (map renderLine ctx))))))
    renderLine (n, l) =
      let sn = show n
      in sn <> replicate (5 - length sn) ' ' <> l
      -}

renderClosureInfo :: Map.Map Bucket InfoTableLoc -> Html
renderClosureInfo cs = do
  H.dl $ Map.foldrWithKey (\k a res -> renderEntry k a >> res) mempty cs
  where
    renderEntry (Bucket k) (InfoTableLoc table_name cd tydesc lbl m loc) = do
        H.dt (H.b (toHtml k) <> ":" <> toHtml cd  <> ":" <> toHtml table_name <> (toHtml (":" <> tydesc)) <> toHtml (":" <> toHtml loc))
        H.dd mempty
        --H.dd (H.pre (H.code ((preEscapedToHtml (unlines (map renderLine ctx))))))
    renderLine (n, l) =
      let sn = show n
      in sn <> replicate (5 - length sn) ' ' <> l



