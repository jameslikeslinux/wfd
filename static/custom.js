$(document).on("mobileinit", function() {
//    $.mobile.page.prototype.options.addBackBtn = true;
});

$(document).ready(function() {
    $('div').live('pageshow', function(event, ui) {
        ui.prevPage.remove();
    });
});
