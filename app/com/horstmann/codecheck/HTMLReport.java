package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Set;

import javax.imageio.ImageIO;

public class HTMLReport implements Report {
    protected StringBuilder builder;
    private List<String> footnotes = new ArrayList<>();
    private int metaOffset;

    // TODO: Directory
    public HTMLReport(String title) {
        builder = new StringBuilder();
        // Internal HTML can't deal with this
        // TODO: Better property name? Comes from Runner
        if (!"false".equals(System.getProperty("labrat.img.inline")))
            builder.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        builder.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
        builder.append("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n");
        builder.append("<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>\n");
        metaOffset = builder.length();
        comment("Version", "0.3");
        builder.append("<style type=\"text/css\">\n");
        builder.append(".header {font-weight: bold; font-size: 1.2em; }\n");
        builder.append(".item {font-weight: bold;}\n");
        builder.append(".pass {color: green;}\n");
        builder.append(".fail {color: red;}\n");
        builder.append(".note {color: blue; font-weight: bold;}\n");
        builder.append("table.file td {padding-right: 1em; background: #FFF; }\n");
        builder.append(".linenumber {color: gray;}\n");
        builder.append(".footnote {font-size: 0.7em;}\n");
        builder.append("table {font-size: 0.9em;}\n");
        builder.append("td, th { background: #EEE; margin: 0.5em; padding: 0.25em;}\n");
        builder.append("table.output td {vertical-align: top;}\n");
        builder.append("div.footnotes { border-top: 1px solid gray; padding-top: 0.5em; }\n");
        builder.append("</style>\n");
        builder.append("<title>");
        escape(title);
        builder.append("</title>\n</head>\n<body>\n");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#header(java.lang.String)
     */
    @Override
    public HTMLReport header(String section, String text) {
        if (text != null && !text.trim().equals("")) {
            builder.append("<p class=\"header");        
            if (section != null) { builder.append(" "); builder.append(section); }
            builder.append("\">");
            escape(text);
            builder.append("</p>\n");
        }
        return this;
    }


    @Override
    public HTMLReport run(String text) {
        if (text != null && !text.trim().equals("")) {
            builder.append("<p class=\"item\">");
            escape(text);
            builder.append("</p>\n");
        }
        return this;
    }

    private HTMLReport caption(String text) {
        if (text != null && !text.trim().equals("")) {
            builder.append("<p class=\"caption\">");
            escape(text);
            builder.append(":</p>\n");
        }
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#output(java.lang.String)
     */
    @Override
    public HTMLReport output(CharSequence text) {
        if (text == null || text.equals(""))
            return this;
        builder.append("<pre class=\"output\">");
        escape(text);
        builder.append("</pre>\n");
        return this;
    }
    
    public HTMLReport input(String input) {
        if (input != null && input.trim().length() > 0) caption("Input");
        return output(input);
    }
    
    public HTMLReport args(String args) {
        if (args == null || args.trim().length() == 0) return this; 
        caption("Command line arguments");
        return output(args);
    }
    
    @Override
    public Report footnote(String text) {
        footnotes.add(text);
        return this;
    }

    @Override
    public HTMLReport output(List<String> lines, Set<Integer> matches,
            Set<Integer> mismatches) {
        builder.append("<pre class=\"output\">");
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (matches.contains(i))
                passSpan(line);
            else if (mismatches.contains(i))
                failSpan(line);
            else
                escape(line);
            builder.append("\n");
        }
        builder.append("</pre>\n");
        return this;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#error(java.lang.String,
     * java.lang.String)
     */
    @Override
    public HTMLReport error(String message) {
        caption("Error");
        output(message);
        return this;
    }

    @Override
    public HTMLReport systemError(String message) {
        failSpan("System Error:");
        output(message);
        return this;
    }

    @Override
    public HTMLReport systemError(Throwable t) {
        return systemError(Util.getStackTrace(t));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#image(java.lang.String, byte[])
     */
    @Override
    public HTMLReport image(String captionText, BufferedImage img) {
        if (img == null)
            return this;
        caption(captionText);
        image(img);
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#image(byte[])
     */
    @Override
    public HTMLReport image(BufferedImage img) {
        if (img == null)
            return this;
        try {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            ImageIO.write(img, "PNG", out);
            out.close();
            byte[] pngBytes = out.toByteArray();
            String data = Base64.getEncoder().encodeToString(pngBytes);
            builder.append("<p class=\"screencapture\">");
            builder.append("<img alt=\"screen capture\" src=\"data:image/png;base64,");
            builder.append(data);
            builder.append("\"/>");
            builder.append("</p>\n");
        } catch (IOException ex) {
            builder.append("<p>Cannot display image</p>");
        }
        return this;
    }
    
    @Override
    public HTMLReport file(String file, String contents) {
        caption(file);
        output(contents); // TODO: Line numbers?
        return this;
    }

    private HTMLReport escape(CharSequence s) {
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '<')
                builder.append("&lt;");
            else if (c == '>') // Not strictly necessary
                builder.append("&gt;");
            else if (c == '&')
                builder.append("&amp;");
            else if (c == '〈')
                builder.append("<b>");
            else if (c == '〉')
                builder.append("</b>");
            else
                builder.append(c);
        }
        return this;
    }

    public static StringBuilder htmlEscape(CharSequence s) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '<')
                builder.append("&lt;");
            else if (c == '>') // Not strictly necessary
                builder.append("&gt;");
            else if (c == '&')
                builder.append("&amp;");
            else
                builder.append(c);
        }
        return builder;
    }
    
    public static StringBuilder attrEscape(CharSequence s) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '"')
                result.append("&quot;");
            else if (c == '\'') // Not strictly necessary in "" delimited
                                // attribute values
                result.append("&apos;");
            else if (c == '&')
                result.append("&amp;");
            else
                result.append(c);
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#add(com.horstmann.codecheck.Score)
     */
    @Override
    public HTMLReport add(Score score) {
        header("score", "Score");
        builder.append("<p class=\"score\">");
        builder.append("" + score);
        builder.append("</p>\n");
        return this;
    }

    protected void addFootnotes() {
        if (footnotes.size() > 0) {
            builder.append("<div class=\"footnotes\">");
            for (String footnote : footnotes) {
                builder.append("<div class=\"footnote\">");
                escape(footnote);
                builder.append("</div>\n");
            }
            builder.append("</div>\n");
        }
    }
       
    @Override
    public void close() {
        addFootnotes();
        builder.append("</body></html>\n");     
    }
    
    @Override
    public String getText() { return builder.toString(); }
    
    @Override
    public String extension() { return "html"; }

    private HTMLReport tableStart(String klass) {
        builder.append("<table");
        if (klass != null)
            builder.append(" class=\"").append(klass).append("\"");
        builder.append(">\n");
        return this;
    }

    private HTMLReport tableEnd() {
        builder.append("</table>\n");
        return this;
    }

    private HTMLReport rowStart() {
        builder.append("<tr>");
        return this;
    }

    private HTMLReport rowEnd() {
        builder.append("</tr>\n");
        return this;
    }

    private HTMLReport cellStart() {
        builder.append("<td>");
        return this;
    }

    private HTMLReport cellEnd() {
        builder.append("</td>\n");
        return this;
    }

    private HTMLReport cell(CharSequence text) {
        cellStart();
        if (text.length() == 0)
            builder.append("&#160;");
        else
            escape(text);
        cellEnd();
        return this;
    }

    private HTMLReport codeCell(CharSequence text) {
        cellStart();
        builder.append("<pre>");
        escape(text);
        builder.append("</pre>");
        cellEnd();
        return this;
    }

    private HTMLReport headerCell(CharSequence text) {
        builder.append("<th>");
        if (text.length() == 0)
            builder.append("&#160;");
        else
            escape(text);
        builder.append("</th>");
        return this;
    }

    private HTMLReport passSpan(String text) {
        builder.append("<span class=\"pass\">");
        escape(text);
        builder.append("</span>");
        return this;
    }

    private HTMLReport failSpan(String text) {
        builder.append("<span class=\"fail\">");
        escape(text);
        builder.append("</span>");
        return this;
    }

    private HTMLReport noteSpan(String text) {
        builder.append("<span class=\"note\">");
        escape(text);
        builder.append("</span>");
        return this;
    }

    @Override
    public HTMLReport pass(boolean b) {
        if (b)
            passSpan("pass ");
        else
            failSpan("fail ");
        return this;
    }

    @Override
    public HTMLReport compareTokens(String filename, List<Match> matchData) {
        caption(filename);
        tableStart("output").rowStart().headerCell("Actual")
                .headerCell("Expected").rowEnd().rowStart();
        builder.append("<td>");
        builder.append("<pre>");
        for (Match m : matchData) {
            if (m.matches)
                escape(m.actual);
            else {
                failSpan(m.actual);
                if (m.explanation != null) {
                    builder.append("\n");
                    noteSpan(m.explanation);
                }
            }
            builder.append("\n");
        }
        builder.append("</pre>");
        cellEnd().cellStart();
        builder.append("<pre>");
        for (Match m : matchData) {
            escape(m.expected);
            if (m.explanation != null) 
                builder.append("\n"); // To match the other column
            builder.append("\n");
        }
        builder.append("</pre>");
        cellEnd().rowEnd().tableEnd();
        return this;
    }

    @Override
    public HTMLReport runTable(String[] methodNames, String[] argNames, String[][] args,
            String[] actual, String[] expected, boolean[] outcomes) {
        tableStart("run").rowStart();
        headerCell("");
        if (methodNames != null)
            headerCell("Name");
        
        for (String n : argNames)
            headerCell(n);
        headerCell("Actual").headerCell("Expected").rowEnd();
        for (int i = 0; i < args.length; i++) {
            rowStart();
            cellStart().pass(outcomes[i]).cellEnd();
            if (methodNames != null)
                codeCell(methodNames[i]);
            for (String a : args[i])
                codeCell(a.trim());
            codeCell(actual[i]);
            codeCell(expected[i]);
            rowEnd();
        }
        tableEnd();
        return this;
    }

    public HTMLReport comment(String key, String value) {
        String meta = "<meta name=\"" + attrEscape(key) + "\" content=\""
                + attrEscape(value) + "\"/>\n";
        builder.insert(metaOffset, meta);
        metaOffset += meta.length();
        return this;
    }

    public HTMLReport hiddenOutputMessage() {
        output("[hidden]");
        return this; 
    }
}
