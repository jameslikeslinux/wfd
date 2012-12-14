$(document).ready(function() {
    $('div').live('pageshow', function(event, ui) {
        ui.prevPage.remove();
    });
});
