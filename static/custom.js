$(document).on("mobileinit", function() {
//    $.mobile.page.prototype.options.addBackBtn = true;
});

$(document).ready(function() {
    $('div').live('pageshow', function(event, ui) {
        ui.prevPage.remove();
    });
});

/*
 * Enable page loading notifications for Nitrogen calls
 * (ajax actoins performed outside of jQuery Mobile)
 *
 * See: http://jquerymobile.com/demos/1.2.0/docs/pages/loader.html
 *      http://stackoverflow.com/questions/7208609/show-page-loading-spinner-on-ajax-call-in-jquery-mobile
 */
var timer;

$(document).ajaxStart(function() {
    timer = setTimeout(function() {
        $.mobile.loading('show');
    }, 50);
});

$(document).ajaxStop(function() {
    clearTimeout(timer);
    $.mobile.loading('hide');
});
