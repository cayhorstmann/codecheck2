$('.java').ace({ theme: 'eclipse', lang: 'java' }).each(function(idx, editor) {
  var ace = $(editor).data('ace').editor.ace;
  ace.setOption("autoScrollEditorIntoView", "true");
  ace.setOption("displayIndentGuides", "true");
  ace.setOption("enableBasicAutocompletion", "true");
  ace.setOption("enableLiveAutocompletion", "false");
  ace.setOption("enableSnippets", "true");
  ace.setOption("minLines", 10);
  ace.setOption("maxLines", 30);
  ace.setOption("showInvisibles", false);
  ace.setOption("tabSize", 3);
  ace.setOption("useWorker", "true");
});
$('.cpp').ace({ theme: 'eclipse', lang: 'c_cpp' }).each(function(idx, editor) {
  var ace = $(editor).data('ace').editor.ace;
  ace.setOption("autoScrollEditorIntoView", "true");
  ace.setOption("displayIndentGuides", "true");
  ace.setOption("enableBasicAutocompletion", "true");
  ace.setOption("enableLiveAutocompletion", "false");
  ace.setOption("enableSnippets", "true");
  ace.setOption("minLines", 10);
  ace.setOption("maxLines", 30);
  ace.setOption("showInvisibles", "true");
  ace.setOption("tabSize", 3);
  ace.setOption("useWorker", "true");
});
$('.py').ace({ theme: 'eclipse', lang: 'python' }).each(function(idx, editor) {
  var ace = $(editor).data('ace').editor.ace;
  ace.setOption("autoScrollEditorIntoView", "true");
  ace.setOption("displayIndentGuides", "true");
  ace.setOption("enableBasicAutocompletion", "true");
  ace.setOption("enableLiveAutocompletion", "false");
  ace.setOption("enableSnippets", "true");
  ace.setOption("minLines", 10);
  ace.setOption("maxLines", 30);
  ace.setOption("showInvisibles", "true");
  ace.setOption("tabSize", 3);
  ace.setOption("useWorker", "true");
});
$('.default').ace({ theme: 'eclipse', lang: 'text' }).each(function(idx, editor) {
  var ace = $(editor).data('ace').editor.ace;
  ace.setOption("autoScrollEditorIntoView", "true");
  ace.setOption("displayIndentGuides", "true");
  ace.setOption("minLines", 10);
  ace.setOption("maxLines", 30);
  ace.setOption("showInvisibles", "true");
  ace.setOption("tabSize", 3);
  ace.setOption("useWorker", "true");
});
