$(document).ready(function() {
  $("textarea").each(function(index) {
    var textarea_id = $(this).attr('id');
    var editor = CodeMirror.fromTextArea(document.getElementById(textarea_id), {
      extraKeys: {"Ctrl-Space": "autocomplete"},
      lineNumbers: true,
      viewportMargin: Infinity,
      matchBrackets: true,
      styleActiveLine: true,
      autoCloseBrackets: true,
      showTrailingSpace: true,
      theme: 'neo',
      tabSize: 2
    });
  });
});
