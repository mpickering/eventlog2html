$(document).ready(function () {
    $("#closures-tab").on("shown.bs.tab", function (e) {
        $.sparkline_display_visible();
    });
});
