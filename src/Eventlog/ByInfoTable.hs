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


mkClosureInfo :: FilePath -> Map.Map Bucket a -> IO (Map.Map Bucket SourceInfo)
mkClosureInfo fp b = do
  d <- getDwarfInfo fp
  let itps = map toItblPointer (Map.keys b)
  print (take 5 (zip (Map.keys b) itps))
  print $ length (Map.keys b)
  res <- Map.traverseMaybeWithKey (\k _ -> lookupSourceInfo fp d (toItblPointer k)) b
  print (Map.size res)
--  print $ length (catMaybes ress)
--  print (head ress)
  return res

  where
    toItblPointer (Bucket t) =
      let s = drop 2 (T.unpack t)
          w64 = case readHex s of
                  ((n, ""):_) -> toBE64 n
                  _ -> error (show t)
      in InfoTablePtr w64

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



