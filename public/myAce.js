$('.java').ace({ theme: 'eclipse', lang: 'java' }).each(function(idx, editor) {
  var ace = $(editor).data('ace').editor.ace;
  ace.setOption("autoScrollEditorIntoView", "true");
  ace.setOption("displayIndentGuides", "true");
  ace.setOption("enableBasicAutocompletion", "true");
  ace.setOption("enableLiveAutocompletion", "false");
  ace.setOption("enableSnippets", "true");
  ace.setOption("maxLines", 30);
  ace.setOption("showInvisibles", false);
  ace.setOption("tabSize", 2);
  ace.setOption("useWorker", "true");
});
$('.cpp').ace({ theme: 'tomorrow_night', lang: 'c_cpp' }).each(function(idx, editor) {
  var ace = $(editor).data('ace').editor.ace;
  ace.setOption("autoScrollEditorIntoView", "true");
  ace.setOption("displayIndentGuides", "true");
  ace.setOption("enableBasicAutocompletion", "true");
  ace.setOption("enableLiveAutocompletion", "false");
  ace.setOption("enableSnippets", "true");
  ace.setOption("maxLines", 30);
  ace.setOption("showInvisibles", "true");
  ace.setOption("tabSize", 2);
  ace.setOption("useWorker", "true");
});
$('.text').ace({ theme: 'terminal', lang: 'plain_text' });
$('.html').ace({ theme: 'chrome', lang: 'html' });
$('.default').ace({ theme: 'xcode', lang: 'text' });
