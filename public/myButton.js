$(document).ready(function() {
  var n = 0;
  $("div.directory").each(function(index) {
    var div_id = $(this).attr('id');
    var button = "add_" + div_id;
    var button_value = "Add file to " + div_id;
    // Top level
    if (div_id == 'TOP') {
      // change button's value
      button_value = "Add file"
      // setup add_directory
      $(this).append($('<p><input type=\"button\" id=\"add_directory\" name=\"add_directory\" value=\"Add directory\">'));
      $("#add_directory").click(function() {
        $("#add_directory").attr("disabled", true);
        var p = $("#add_directory").parents("div");
        p.append($('<p>Directory name: <input type="text" id="new_directory" name="new_directory"></p>'));
      });
    }
    // All level(s)
    // setup add_file
    $(this).append($('<p><input type=\"button\" id=\"' + button + '\" name=\"' + button + '\" value=\"' + button_value + '\">'));
    $('#' + button).click(function() {
      var p = $("#" + button).parents("div");
      p.append($('<p>File name: <input type="text" id="new_' + div_id + ':' + n + '_filename" name="new_' + div_id + ':' + n + '_filename"></p>'));
      p.append($('<textarea id="new_' + div_id + ':' + n + '_content" name="new_' + div_id + ':' + n + '_content" class="java"></textarea>'));
      n++;
    });
  });
});
