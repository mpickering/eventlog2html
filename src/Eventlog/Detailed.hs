{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Eventlog.Detailed where

import qualified Data.Map as Map
import Eventlog.Types
import qualified Data.Text as T

import Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Array.Unboxed (UArray, bounds)
import qualified Data.Array.Unboxed as A
import Data.Fixed
import Control.Monad
import Data.Maybe

data InfoTableLocStatus = None -- None of the entries have InfoTableLoc
                        | Missing -- This one is just missing
                        | Here InfoTableLoc -- Here is is

mkMissing :: Maybe InfoTableLoc -> InfoTableLocStatus
mkMissing = maybe Missing Here


mkClosureInfo :: Map.Map Bucket a
              -> Map.Map InfoTablePtr InfoTableLoc
              -> Map.Map Bucket (InfoTableLocStatus, a)
mkClosureInfo b ipes =
  Map.mapWithKey (\k v -> (mkMissing $ Map.lookup (toItblPointer k) ipes, v)) b


renderClosureInfo :: (UArray Int Double, UArray (Int, Int) Double)
                  -- Raw Data
                  -> Maybe (Map.Map InfoTablePtr InfoTableLoc)
                  -- Do we have IPE information?
                  -> Map.Map Bucket (Int, BucketInfo)
                  -- Buckets
                  -> Html
renderClosureInfo (ts, bs) mipes raw_bs = do
  let cs = case mipes of
             Just ipes -> mkClosureInfo raw_bs ipes
             Nothing   -> Map.map (\v -> (None, v)) raw_bs

  H.table ! A.id "closure_table" ! A.class_ "table table-striped closureTable" $ do
    H.thead $ H.tr $ do
      H.th "Profile"
      numTh "n"
      H.th "Label"
      when (isJust mipes) $ do
        H.th "Description"
        H.th "CTy"
        H.th "Type"
        H.th "Module"
        H.th "Loc"
      numTh ("Integrated Size" <> H.br <> "(MiB s)")
      numTh "Stddev (MiB) "
      numTh "Intercept"
      numTh "Slope"
      numTh "Fit (R²)"
    Map.foldrWithKey (\k a res -> renderEntry k a >> res) (mempty :: Html) cs
  H.script $ preEscapedToHtml (initTable (isJust mipes))
  where
    numTh lbl = H.th ! H.dataAttribute "sortas" "numeric" $ lbl
    trunc :: Double -> Fixed E2
    trunc = realToFrac
    render = showFixed True

    renderInfoTableLoc :: InfoTableLoc -> Html
    renderInfoTableLoc (InfoTableLoc table_name cd tydesc _lbl m sloc) = do
      H.td (toHtml table_name)
      H.td (toHtml (show @ClosureType cd))
      H.td (toHtml tydesc)
      H.td (toHtml m)
      H.td (toHtml sloc)


    renderInfoTableLocStatus :: InfoTableLocStatus -> Html
    renderInfoTableLocStatus itls =
      case itls of
        Here itl -> renderInfoTableLoc itl
        Missing  -> emptyItlColumns
        None     -> mempty

    emptyItlColumns = do
      H.td ""
      H.td ""
      H.td ""
      H.td ""
      H.td ""

    renderEntry (Bucket k) (mitl, (n, BucketInfo _ _ tot std mg)) = do
          let (a, b, r2) =
                case mg of
                  Nothing -> ("", "", "")
                  Just (ad, bd, r2d) -> (render $ trunc ad
                                       , render $ trunc bd
                                       , render $ trunc r2d)
          H.tr $ do
            H.td (renderSpark (getBandValues n (ts, bs)))
            H.td (toHtml n)
            H.td (toHtml k)
            renderInfoTableLocStatus mitl
            H.td (toHtml (render $ trunc (tot / 1e6)))
            H.td (toHtml (render $ trunc (std / 1e6)))
            H.td (toHtml a)
            H.td (toHtml b)
            H.td (toHtml r2)

renderSpark :: [(Double, Double)] -> Html
renderSpark vs = H.span ! A.class_ "linechart" $ toHtml (T.intercalate "," (map renderLine vs))
  where
    rdouble = T.pack . showFixed True . realToFrac @Double @(Fixed E2)
    renderLine (x,y) = rdouble x <> ":" <> rdouble y

initTable :: Bool -> T.Text
initTable b = "$(document).ready(function() {\
        \$(\".closureTable\").fancyTable({\
        \    sortColumn: 1,\
        \    pagination: true,\
        \    perPage:10,\
        \    globalSearch:false,\
        \    globalSearchExcludes: [7,8,9,10,11,12],\
        \    sortOrder: 'descending',\
        \    onUpdate: function(){$.sparkline_display_visible()}\
        \});\
        \$.fn.sparkline.defaults.common.chartRangeMin = 0;\
        \$.fn.sparkline.defaults.common.width = 200;\
        \$('.linechart').sparkline()\
\});"

getBandValues :: Int
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [(Double, Double)]
getBandValues k (ts, vs) =
  let (t1, tn) = bounds ts
      go i = flip map [t1 .. tn] $ \t -> ((ts A.! t), (vs A.! (i, t)))

  in go k

