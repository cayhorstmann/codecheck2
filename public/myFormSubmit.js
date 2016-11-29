/* For json/jsonp codecheck submission report within single page */
$(function () {
    $('form').on('submit', function(e) {
        e.preventDefault();
        clearErrorAnnotations();

        if ($('.codecheck-submit-response').length == 0)
            $('form').after('<div class="codecheck-submit-response"></div>');
        $('.codecheck-submit-response').text('Submitting...');
        var values = $(this).serializeArray();
        values.push({name: 'type', value: 'jsonp'});
        $.ajax({
            url: '/checkJsonp',
            dataType: ajaxSubmissionType, // 'jsonp' or 'json' 
            contentType: 'application/json',
            jsonp: "callback",
            data: values,
            timeout: 300000, // 5 minutes            
            success: function (data) {
                $('.codecheck-submit-response').text('')
                $('.codecheck-submit-response').append(data['report'])
                $('.codecheck-submit-response').append(
                		'<div class="download">'
                		+ '<button onclick="download(\'data:application/octet-stream;base64,' + data.zip + '\', \'homework.zip\', \'application/octet-stream\')">Download Test</button></div>')
                $('.codecheck-submit-response').data('score', data['score'])

                // ace editor
                // clear any existing annotations
                clearErrorAnnotations();                
                // show error annotations
                if ('errors' in data) {
                    var error = data['errors'][0]; // only display first error for sanity
                    highlightLine(error['file'], error['line'], error['message']);

                    var editor = document.getElementById(error['file']);
                    var aceEditor = $(editor).data('ace').editor.ace;
                    aceEditor.getSession().on('change', function() {
                        clearErrorAnnotations();
                    });
                }
            },
            error: function(jqXHR, textStatus, errorThrown) {
            	$('.codecheck-submit-response').text('')
            	
                $('.codecheck-submit-response').append($("<div>").text('Error Status: ' + textStatus))
                $('.codecheck-submit-response').append($("<div>").text('Error Thrown: ' + errorThrown))
                $('.codecheck-submit-response').append($("<div>").text('Error Response: ' + jQuery.parseJSON(jqXHR.responseText)))
            }
        });
    });
});

var downloadZip = function(data) {
	download(data.metadata.ID + '.signed.zip', 'report.zip', 'application/octet-stream');
}

var highlightLine = function(file, line, message) {
    var editor = document.getElementById(file);
    var aceEditor = $(editor).data('ace').editor.ace;
    aceEditor.getSession().setAnnotations([{
        row: row - 1, // ace editor lines are 0-indexed
        text: message,
        type: "error"
    }]);
};

var clearErrorAnnotations = function() {
    $(document).find('textarea')
        .filter(function(i,e) {
            // ignore textareas made by ace
            return $(e).attr('class') === undefined || $(e).attr('class').indexOf("ace_") === -1;
        })
        .each(function(idx, textarea) {
            var aceEditor = $(textarea).data('ace').editor.ace;
            aceEditor.getSession().clearAnnotations();
        });
};