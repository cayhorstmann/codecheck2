/* For ajax/jsonp codecheck submission report within single page */
$(function () {
    $('form').on('submit', function (e) {
        e.preventDefault();
        if ($('.codecheck-submit-response').length == 0)
            $('form').after('<div class="codecheck-submit-response"></div>');
        $('.codecheck-submit-response').text('Submitting...');
        var values = $(this).serializeArray();
        values.push({name: 'type', value: 'jsonp'});
        $.ajax({
            url: '/checkJsonp',
            dataType: 'jsonp',
            contentType: 'application/json',
            jsonp: "callback",
            data: values,
            success: function (data) {
                $('.codecheck-submit-response').text('');
                $('.codecheck-submit-response').append(data['report']);
            }
        });
    });
});