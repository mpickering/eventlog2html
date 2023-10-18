{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- Functions for rendering ticky sample information
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Eventlog.Ticky (tickyTab, renderTicky) where

import qualified Data.Map as Map
import Data.Word

import qualified Data.Text as T
--import Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import Text.Blaze.Html5 as H
    ( preEscapedToHtml,
      toHtml,
      dataAttribute,
      preEscapedStringValue,
      stringComment,
      Html,
      (!),
      AttributeValue,
      body,
      button,
      code,
      div,
      docTypeHtml,
      h1,
      head,
      link,
      meta,
      script,
      style,
      table,
      td,
      th,
      thead,
      title,
      tr )
import Text.Blaze.Html5.Attributes as A
    ( charset, class_, hidden, href, id, onclick, rel, src)
import Text.Blaze (customAttribute)

import Eventlog.Types
import Text.RawString.QQ
import Data.Fixed
import Control.Monad

import Data.List (foldl', sortBy)
import Data.Ord

renderTicky :: Word64 -> Map.Map TickyCounterId TickyCounter
                      -> Map.Map InfoTablePtr InfoTableLoc
                      -> [TickySample] -> (Double, Html)
renderTicky total_allocs counters ipes samples = (percentage_ticked, renderTickyInfo (not (Map.null ipes)) joined_with_ipe)
  where
    percentage_ticked = realToFrac (sum (Map.map allocs accum_samples)) / realToFrac total_allocs
    joined_with_ipe   = mkClosureInfo (\_ (v, _, _) -> tickyCtrInfo v) joined_data ipes

    joined_data   = Map.mergeWithKey (\_ b c -> Just (b, c, realToFrac (allocs c) / realToFrac total_allocs)) (const mempty) (const mempty) counters accum_samples
    accum_samples = accumulateSamples samples


data AccumStats = AccumStats { entries :: !Word64, allocs :: !Word64, allocd :: !Word64, series :: ![(Double, Word64 {- allocd -},   Word64 {- entries -})] } deriving Show

emptyAccumStats :: AccumStats
emptyAccumStats = AccumStats 0 0 0 []

insertSample :: TickySample -> AccumStats -> AccumStats
insertSample (TickySample _ids entries allocs allocd time) (AccumStats aentries aalloc aallocd aseries ) =
  (AccumStats (aentries + entries) (aalloc + allocs) (allocd + aallocd) ((time, allocd + aallocd, aentries + entries ) : aseries))

initStats :: TickySample -> AccumStats
initStats = flip insertSample emptyAccumStats

accumulateSamples ::  [TickySample] -> Map.Map TickyCounterId AccumStats
accumulateSamples samples =
  foldl' (\smap ts -> Map.insertWith (\_new old -> insertSample ts old) (TickyCounterId $ tickyCtrSampleId ts) (initStats ts) smap) Map.empty
  (sortBy (comparing tickySampleTime) samples)


tickyTab :: TickyProfileData -> Html
tickyTab (TickyProfileData total ticked_percen v) = do
    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Total Allocations: "
        code $ toHtml $ toHtml total
      H.div ! class_ "column cheader" $ do
        "Allocations Ticked (%): "
        code $ toHtml $ toHtml (render  $ trunc (ticked_percen * 100))
    H.div ! class_ "row" $ do
          H.div ! A.id "table" ! class_ "tabviz" $ v

-- Table rendering
trunc :: Double -> Fixed E2
trunc = realToFrac
render :: Fixed E2 -> String
render = showFixed True


renderTickyInfo :: Bool
                  -> Map.Map TickyCounterId (InfoTableLocStatus, (TickyCounter, AccumStats, Double))
                  -> Html
renderTickyInfo with_ipe ticky_samples = do
  H.table ! A.id "closure_table" ! A.class_ "table table-striped closureTable" $ do
    H.thead $ H.tr $ headFoot
--      H.th "Profile"
--      numTh "n"
    Map.foldr (\a res -> renderEntry a >> res) (mempty :: Html) ticky_samples
    H.tfoot $ H.tr $ headFoot
  H.script $ preEscapedToHtml (initTable with_ipe)
  where
    headFoot = do
      H.th "Label"
      H.th "FVs"
      H.th "Args"
      when (with_ipe) $ do
        H.th "Description"
        H.th "CTy"
        H.th "Type"
        H.th "Module"
        H.th "Loc"
      numTh "Allocs"
      numTh "Allocs (%)"
      numTh "Allocd"
      numTh "Allocd #"
      numTh "Entries"
      numTh "Allocs/Entries"
      numTh "Allocd #/Entries"
--      numTh "Chart"
    numTh lbl = H.th ! H.dataAttribute "sortas" "numeric" $ lbl

    renderInfoTableLoc :: InfoTableLoc -> Html
    renderInfoTableLoc (InfoTableLoc table_name cd tydesc _lbl m sloc) = do
      H.td (toHtml table_name)
      H.td (toHtml (show @ClosureType cd))
      H.td (preEscapedToHtml tydesc) -- Don't escape this as the ellipsis plugin does it.
      H.td (toHtml m)
      H.td (toHtml sloc)


    renderInfoTableLocStatus :: InfoTableLocStatus -> Html
    renderInfoTableLocStatus _ | not with_ipe = mempty
    renderInfoTableLocStatus itls =
      case itls of
        Here itl -> renderInfoTableLoc itl
        Missing  -> emptyItlColumns
        None -> mempty

    emptyItlColumns = do
      H.td ""
      H.td ""
      H.td ""
      H.td ""
      H.td ""


    renderEntry :: (InfoTableLocStatus, (TickyCounter, AccumStats, Double)) -> Html
    renderEntry (loc, ((TickyCounter _id _arity kinds label _), AccumStats {..}, percent)) = do
      let fvs = tickyCounterFVs kinds
          ticky_args = tickyCounterArgs kinds
          size = closureSize (length fvs) (length ticky_args)
          alloc_no = fromIntegral allocd `Prelude.div` size
      H.tr $ do
--            H.td (renderSpark (getBandValues n (ts, bs)))
            H.td (toHtml label)
            H.td (toHtml fvs)
            H.td (toHtml ticky_args)
            renderInfoTableLocStatus loc
            H.td (toHtml allocs)
            H.td (toHtml $ render $ trunc (percent * 100))
            H.td (toHtml allocd)
            H.td (toHtml alloc_no)
            H.td (toHtml entries)
            H.td (toHtml (case entries of
                            0 -> 0
                            _ -> allocs `Prelude.div` entries))
            H.td (case entries of
                            0 -> "NaN"
                            _ ->
                              case allocd of
                                0 -> "None"
                                _ -> toHtml (render (trunc (realToFrac entries / realToFrac alloc_no))))
--            H.td (toHtml (renderSpark size series))
--            H.td mempty

closureSize :: Int -> Int -> Int
closureSize fvs cl_args
  -- THUNK, HEADER = 2
  | cl_args == 0 = (2 + fvs) * 8
  | otherwise  = (1 + fvs) * 8



renderSpark :: Int -> [(Double, Word64, Word64)] -> Html
renderSpark size vs = H.span ! A.class_ "linechart"
  ! customAttribute "data-allocd" (H.preEscapedTextValue $ T.intercalate "," (map renderLine vs))
  ! customAttribute "data-entries" (H.preEscapedTextValue $ T.intercalate "," (map renderLineEntries vs))
  ! customAttribute "sparkChartRangeMax" (H.toValue max_alloc_n)
  $ mempty
  where
    rdouble = T.pack . showFixed True . realToFrac @Double @(Fixed E2)
    renderLine (x,w, _) = rdouble x <> ":" <> T.pack (show (w `Prelude.div` fromIntegral size))
    renderLineEntries (x,_, e) = rdouble x <> ":" <> T.pack (show e)

    max_alloc_n = last_allocd `Prelude.div` (fromIntegral size)
    (_, last_allocd, _) = Prelude.head vs

initTable :: Bool -> T.Text
initTable ipe =

  "var ipe = " <> (if ipe then "true" else "false") <> ";\n" <>
  [r|// Setup - add a text input to each footer cell
    $(document).ready(function(){
    $('.closureTable tfoot th').each( function () {
        var title = $(this).text();
//       if (! ($(this).data("sortas") == "numeric")){
          $(this).html( '<input type="text" style="width:100%"; placeholder="Search"/>' );
//        }
//        else {
//          $(this).html('')
//        }
    } );

    function init_spark(){

      $('.linechart').sparkline('html', { enableTagOptions: true, tagOptionPrefix: 'allocd-',  tagValuesAttribute: 'data-allocd' });
      $('.linechart').sparkline('html', { composite: true, lineColor: 'red', enableTagOptions: true, tagOptionPrefix: 'entries-', tagValuesAttribute: 'data-entries' });
      $.sparkline_display_visible();
    }

    // DataTable
    var table = $('.closureTable').DataTable({
        "order": [[ ipe ? 8 : 3, "desc" ]],
        "autoWidth": true,
        "dom": 'Bfrtip',
        "buttons": [
            {
                text: 'TSV',
                extend: 'csvHtml5',
                fieldSeparator: '\t',
                extension: '.tsv'
            }
        ],
        "columnDefs": [
          { "orderSequence": ["desc", "asc"],  "targets": (ipe ? [8,9,10,11,12,13,14] : [ 3,4,5,6,7,8,9])}
          , {"render": $.fn.dataTable.render.ellipsis( 30, true, false ), "targets": (ipe ? [5] : []) }
          ],

        "deferRender" : true,
        initComplete: function () {
            // Apply the search
            $(".closureTable").removeAttr("hidden");
            this.api().columns().every( function () {
                var that = this;
                $( 'input', this.footer() ).on( 'blur', function () {
                    if ( that.search() !== this.value ) {
                        that
                            .search( this.value )
                            .draw();
                    }
                } );
        $.fn.sparkline.defaults.common.chartRangeMin = 0;
        $.fn.sparkline.defaults.common.width = 200;
        init_spark();
            } );
        }
    });
    table.on( 'draw', function () {
        init_spark();
    } );
    })
    |]


{-
getBandValues :: Int
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [(Double, Double)]
getBandValues k (ts, vs) =
  let (t1, tn) = bounds ts
      go i = flip map [t1 .. tn] $ \t -> ((ts A.! t), (vs A.! (i, t)))

  in go k
  -}


