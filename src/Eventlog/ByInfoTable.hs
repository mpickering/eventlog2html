{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Eventlog.ByInfoTable where

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
import Data.Array.Unboxed (UArray, bounds)
import qualified Data.Array.Unboxed as A
import Data.Fixed


mkClosureInfo :: FilePath
              -> Map.Map Bucket a
              -> Map.Map InfoTablePtr InfoTableLoc
              -> IO (Map.Map Bucket (InfoTableLoc, a))
mkClosureInfo fp b ipes = do
--  d <- getDwarfInfo fp
  let itps = map toItblPointer (Map.keys b)
  print (take 5 (zip (Map.keys b) itps))
  print $ length (Map.keys b)
  print $ (Map.size ipes)
  print $ (take 100 $ Map.keys ipes)
  --res <- Map.traverseMaybeWithKey (\k _ -> lookupSourceInfo fp d (toItblPointer k)) b
  res <- Map.traverseMaybeWithKey (\k v -> return $ (,) <$> Map.lookup (toItblPointer k) ipes <*> pure v) b
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

renderClosureInfo :: (UArray Int Double, UArray (Int, Int) Double)
                  -> Map.Map Bucket (InfoTableLoc, (Int, BucketInfo))
                  -> Html
renderClosureInfo (ts, bs) cs = do
  H.table ! A.id "closure_table" ! A.class_ "table table-striped closureTable" $ do
    H.thead $ H.tr $ do
      H.th "Profile"
      numTh "n"
      H.th "Address"
      H.th "Description"
      H.th "CTy"
      H.th "Type"
      H.th "Module"
      H.th "Loc"
      numTh "Total Size (M)"
      numTh "Stddev"
      numTh "Intercept"
      numTh "Slope"
      numTh "Fit (r2)"
    Map.foldrWithKey (\k a res -> renderEntry k a >> res) (mempty :: Html) cs
  H.script $ preEscapedToHtml initTable
  where
    numTh label = H.th ! H.dataAttribute "sortas" "numeric" $ label
    truncate :: Double -> Fixed E2
    truncate = realToFrac
    render = showFixed True
    renderEntry (Bucket k)
      (InfoTableLoc table_name cd tydesc lbl m loc
        , (n, BucketInfo _ _ tot std mg)) = do
          let (a, b, r2) =
                case mg of
                  Nothing -> ("", "", "")
                  Just (ad, bd, r2d) -> (render $ truncate ad
                                       , render $ truncate bd
                                       , render $ truncate r2d)
          H.tr $ do
            H.td (renderSpark (getBandValues n (ts, bs)))
            H.td (toHtml n)
            H.td (toHtml k)
            H.td (toHtml table_name)
            H.td (toHtml (show @ClosureType cd))
            H.td (toHtml tydesc)
            H.td (toHtml m)
            H.td (toHtml loc)
            H.td (toHtml (render $ truncate (tot / 1e6)))
            H.td (toHtml (render $ truncate std))
            H.td (toHtml a)
            H.td (toHtml b)
            H.td (toHtml r2)

renderSpark :: [(Double, Double)] -> Html
renderSpark vs = H.span ! A.class_ "linechart" $ toHtml (T.intercalate "," (map renderLine vs))
  where
    rdouble = T.pack . showFixed True . realToFrac @Double @(Fixed E2)
    renderLine (x,y) = rdouble x <> ":" <> rdouble y

initTable :: T.Text
initTable = "$(document).ready(function() {\
        \$(\".closureTable\").fancyTable({\
        \    sortColumn:8,\
        \    pagination: true,\
        \    perPage:10,\
        \    globalSearch:false,\
        \    globalSearchExcludes: [7,8,9,10,11,12],\
        \    sortOrder: 'descending',\
        \    onUpdate: function(){$.sparkline_display_visible()}\
        \});\
        \$('.linechart').sparkline()\
\});"

getBandValues :: Int
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [(Double, Double)]
getBandValues k (ts, vs) =
  let (t1, tn) = bounds ts
      go i = flip map [t1 .. tn] $ \t -> ((ts A.! t), (vs A.! (i, t)))

  in go k

