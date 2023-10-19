$(document).ready(function () {
    $("#closures-tab").on("shown.bs.tab", function (e) {
        $.sparkline_display_visible();
    });

    $("#profiteur-tab").on("shown.bs.tab", function (e) {
        var iFrame = document.getElementById( "profiteur-iframe" );
        resizeIFrameToFitContent( iFrame );
    });
});

var resizeIFrameToFitContent = function( iFrame ) {

    iFrame.height = $(".container-fluid").height();
};
