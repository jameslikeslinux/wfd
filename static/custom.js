$(document).on('mobileinit', function() {
//    $.mobile.page.prototype.options.addBackBtn = true;
//    $.mobile.ignoreContentEnabled = true;    
    $.mobile.defaultDialogTransition = 'none';
    $.mobile.defaultPageTransition = 'none';
});

$(document).on('pageinit', function() {
    $('div').on('pageshow', function(event, ui) {
        ui.prevPage.remove();
    });

    $('.ui-slider-input').each(function() {
        $(this).data('lastvalue', $(this).val());

        $(this).on('slidestart', function() {
            $(this).data('sliding', true);
        });
    
        $(this).on('slidestop', function() {
           $(this).data('sliding', false);
           $(this).trigger('change');
        });
    
        $(this).on('change', function(event) {
            if ($(this).val() == $(this).data('lastvalue') || $(this).data('sliding')) {
                event.stopImmediatePropagation();
            } else {
                $(this).data('lastvalue', $(this).val());
            }
        });
    });

    // I want to create labels for the listview filter input
    $('input[data-type="search"]').attr('id', 'search-input');
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
