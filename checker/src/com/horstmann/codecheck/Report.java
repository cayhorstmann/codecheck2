package com.horstmann.codecheck;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Report {
    private StringBuilder builder;

    public Report(String title) {
        builder = new StringBuilder();
        // Internal HTML can't deal with this
        // TODO: Better property name? Comes from Runner
        if (!"false".equals(System.getProperty("labrat.img.inline")))
            builder.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
        builder.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
        builder.append("<html xmlns=\"http://www.w3.org/1999/xhtml\"><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" /><title>"
                       + htmlEscape(title) + "</title></head><body>");
    }

    public void header(String text) {
        builder.append("<p><b>" + htmlEscape(text) + "</b></p>\n");
    }

    public void html(String caption, CharSequence content) {
        if (caption != null && !caption.trim().equals(""))
            builder.append("<p>" + caption + ":</p>");
        builder.append(content);
    }

    public void html(CharSequence content) {
        html(null, content);
    }

    public void output(String text) {
        output(null, text);
    }

    public void output(String caption, String text) {
        if (text == null || text.trim().equals(""))
            return;
        if (caption != null && !caption.trim().equals(""))
            builder.append("<p>" + caption + ":</p>");
        builder.append("<pre>" + htmlEscape(text) + "</pre>");
    }

    public void warning(String message) {
        if (message == null || message.trim().equals(""))
            return;
        builder.append("<p>Warning:</p><pre>" + htmlEscape(message) + "</pre>");
    }

    public void info(String message) {
        if (message == null || message.trim().equals(""))
            return;
        builder.append("<p>" + htmlEscape(message) + "</p>");
    }

    public void error(String caption, String message) {
        if (message == null || message.trim().equals(""))
            return;
        builder.append("<p>" + caption + ":</p><pre>" + htmlEscape(message) + "</pre>");
    }

    public void error(String message) {
        error("Error", message);
    }

    public void systemError(String message) {
        if (message == null || message.trim().equals(""))
            return;
        builder.append("<p><font color=\"red\">Error:</font></p><pre>" + htmlEscape(message) + "</pre>");
    }

    public void systemError(Throwable t) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        PrintStream pout = new PrintStream(out);
        t.printStackTrace(pout);
        systemError(out.toString());
    }

    public void image(String caption, Path file) {
        builder.append("<p>");
        builder.append(caption);
        builder.append("</p>");
        image(file);
    }

    public void image(Path file) {
        builder.append("<p>");
        if ("false".equals(System.getProperty("labrat.img.inline"))) {
            builder.append("<img src=\"");
            builder.append(file.toUri().toString());
            builder.append("\">");
        } else
            try {
                String data = new sun.misc.BASE64Encoder().encode(Files.readAllBytes(file));
                builder.append("<img alt=\"screen capture\" src=\"data:image/png;base64,");
                builder.append(data);
                builder.append("\"/>");
            } catch (IOException ex) {
                error("No image");
            }
        builder.append("</p>");
    }

    public void image(String caption, byte[] pngBytes) {
        builder.append("<p>");
        builder.append(caption);
        builder.append("</p>");
        image(pngBytes);
    }

    public void image(byte[] pngBytes) {
        builder.append("<p>");
        if (pngBytes == null) {
            error("No image");
        } else {
            String data = new sun.misc.BASE64Encoder().encode(pngBytes);
            builder.append("<img alt=\"screen capture\" src=\"data:image/png;base64,");
            builder.append(data);
            builder.append("\"/>");
        }
        builder.append("</p>");
    }

    public void file(Path dir, Path file) {
        builder.append("<p>" + file.toString() + "</p>");
        Path source = dir.resolve(file);
        boolean lineNumbers = file.toString().endsWith(".java");
        if (Files.exists(source)) {
            try {
                List<String> lines = Files.readAllLines(source, Charset.forName("UTF-8"));
                if (lineNumbers) {
                    builder.append("<table><tr><td style=\"padding-right: 1em;\"><pre>");
                    for (int i = 1; i <= lines.size(); i++) {
                        builder.append(String.format("<span style=\"color: gray;\">%3d</span>\n", i));
                    }
                    builder.append("</pre></td><td>");
                }
                builder.append("<pre>");
                for (String line : lines) {
                    builder.append(htmlEscape(line));
                    builder.append("\n");
                }
                builder.append("</pre>");
                if (lineNumbers) {
                    builder.append("</td></tr></table>");
                }
            } catch (IOException e) {
                systemError(e);
            }

        } else {
            builder.append("<p>Not found</p>");
        }
    }

    public static StringBuilder htmlEscape(CharSequence s) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '<')
                b.append("&lt;");
            else if (c == '>')
                b.append("&gt;");
            else if (c == '&')
                b.append("&amp;");
            else
                b.append(c);
        }
        return b;
    }

    public void add(Score score) {
        builder.append("<p><b>Score </b></p>\n" + score);
    }

    public void save(Path out) throws IOException {
        builder.append("</body></html>\n");
        Files.write(out, builder.toString().getBytes());
    }
}
