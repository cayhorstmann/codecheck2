define("ace/theme/kuroir",["require","exports","module","ace/lib/dom"], function(require, exports, module) {

exports.isDark = false;
exports.cssClass = "ace-kuroir";
exports.cssText = "\
.ace-kuroir .ace_gutter {\
background: #F3F3F4;\
color: #333;\
}\
.ace-kuroir .ace_print-margin {\
width: 1px;\
background: #e8e8e8;\
}\
.ace-kuroir {\
background-color: #F3F3F4;\
color: #363636;\
}\
.ace-kuroir .ace_cursor {\
color: #202020;\
}\
.ace-kuroir .ace_marker-layer .ace_selection {\
background: #F3F3F4;\
}\
.ace-kuroir .ace_marker-layer .ace_step {\
background: #F3F3F4;\
}\
.ace-kuroir .ace_marker-layer .ace_active-line {\
background: #F3F3F4;\
}\
.ace-kuroir .ace_gutter-active-line {\
background-color: #F3F3F4;\
}\
.ace-kuroir .ace_marker-layer .ace_selected-word {\
border: 1px solid rgba(245, 170, 0, 0.57);\
}\
.ace-kuroir .ace_comment.ace_doc {\
color: #0054a8;\
}\
.ace-kuroir .ace_comment.ace_doc.ace_tag {\
color: black;\
}\
.ace-kuroir .ace_invisible {\
color: #BFBFBF\
}\
.ace-kuroir .ace_fold {\
border-color: #363636;\
}\
.ace-kuroir .ace_constant.ace_language.ace_boolean{\
color: #427b2d;\
}\
.ace-kuroir .ace_keyword {color: #c82254;}\
.ace-kuroir .ace_keyword.ace_operator {color: black;}\
.ace-kuroir .ace_constant{color:#CD6839;}.ace-kuroir .ace_constant.ace_numeric{color:#427b2d;}.ace-kuroir .ace_support{color:#104E8B;}.ace-kuroir .ace_support.ace_function{color:#000000;}.ace-kuroir .ace_support.ace_constant{color:#CF6A4C;}.ace-kuroir .ace_storage{color:#A52A2A;}.ace-kuroir .ace_invalid.ace_illegal{color:#FD1224;\
background-color:rgba(255, 6, 0, 0.15);}.ace-kuroir .ace_invalid.ace_deprecated{text-decoration:underline;\
font-style:italic;\
color:#FD1732;\
background-color:#E8E9E8;}.ace-kuroir .ace_string{color:#00615a;}.ace-kuroir .ace_string.ace_regexp{color:#417E00;\
background-color:#C9D4BE;}.ace-kuroir .ace_comment{color:#0054a8;}.ace-kuroir .ace_variable{color:#009ACD;}.ace-kuroir .ace_meta.ace_tag{color:#005273;}.ace-kuroir .ace_markup.ace_heading{color:#B8012D;\
background-color:rgba(191, 97, 51, 0.051);}.ace-kuroir .ace_markup.ace_list{color:#8F5B26;}\
";

var dom = require("../lib/dom");
dom.importCssString(exports.cssText, exports.cssClass);
});                (function() {
                    window.require(["ace/theme/kuroir"], function(m) {
                        if (typeof module == "object" && typeof exports == "object" && module) {
                            module.exports = m;
                        }
                    });
                })();
            
