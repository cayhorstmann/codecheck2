/* For ajax/jsonp codecheck submission report within single page */
$(function () {
    $('form').on('submit', function(e) {
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

                // ace editor
                // clear any existing annotations
                $(document).find('textarea')
                    .filter(function(i,e) {
                        // ignore textareas made by ace
                        return $(e).attr('class') === undefined || $(e).attr('class').indexOf("ace_") === -1;
                    })
                    .each(function(idx, textarea) {
                        var aceEditor = $(textarea).data('ace').editor.ace;
                        aceEditor.getSession().clearAnnotations();
                    });
                // show error annotations
                if ('errors' in data) {
                    var error = data['errors'][0]; // only display first error for sanity
                    highlightLine(error['file'], error['line'], error['message'])
                }
            }
        });
    });
});

var highlightLine = function(file, line, message) {
    var editor = document.getElementById(file);
    var aceEditor = $(editor).data('ace').editor.ace;
    aceEditor.getSession().setAnnotations([{
        row: line - 1, // ace editor lines are 0-indexed
        text: message,
        type: "error"
    }]);
};