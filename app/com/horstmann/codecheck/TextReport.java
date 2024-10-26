package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

public class TextReport implements Report {
    private StringBuilder builder;
    private int sections;
    private String section;
    private List<String> footnotes = new ArrayList<>();
    private boolean hidden;

    public TextReport(String title) {
    	// TODO This version is only built by the client-side Ant script
        builder = new StringBuilder();
        try {
	        builder.append("codecheck version ");
	        builder.append(ResourceBundle.getBundle(
	                "com.horstmann.codecheck.codecheck").getString("version"));
	        builder.append(" ");
	        builder.append(" started ");
	        builder.append(new Date());
	        builder.append("\n\n");
        } catch (MissingResourceException e) {
        	builder = new StringBuilder();
        }
    }

    private TextReport add(CharSequence s) {
        if (s == null) return this;
        builder.append(s);
        if (s.length() > 0 && s.charAt(s.length() - 1) != '\n')
            builder.append("\n");
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#header(java.lang.String)
     */
    @Override
    public TextReport header(String section, String text) {
        this.section = section;
        hidden = false;
        if ("studentFiles".equals(section) || "providedFiles".equals(section)) return this;

        if (sections > 0)
            builder.append("\n");
        sections++;
        add(text);
        repeat('=', text.length());
        builder.append("\n");
        builder.append("\n");
        return this;
    }

    private TextReport caption(String text) {
        if (text != null && !text.trim().equals("")) {
            builder.append(">>> ");            
            builder.append(text);
            builder.append(" <<<\n");            
        }
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#output(java.lang.String)
     */
    @Override
    public TextReport output(CharSequence text) {
    	if (hidden) add("[Hidden]"); else add(text);
        return this;
    }

    @Override
    public TextReport args(String args) {
        if (args != null && args.trim().length() > 0)
            add("Command line arguments: " + args);
        return this;
    }

    @Override
    public TextReport input(String input) {
        addCaptioned("Input", hidden ? "[Hidden]" : input);
        return this;
    }

    @Override
    public Report footnote(String text) {
        footnotes.add(text);
        return this;
    }

    private TextReport addCaptioned(String captionText, CharSequence text) {
        if (text == null || text.equals(""))
            return this;
        caption(captionText);
        add(text);
        return this;
    }

    @Override
    public TextReport output(List<String> lines, Set<Integer> matches,
            Set<Integer> mismatches) {
    	if (hidden) {
    		add("[Hidden]");
    	} else {
	        for (int i = 0; i < lines.size(); i++) {
	            String line = lines.get(i);
	            if (matches.contains(i))
	                builder.append("+ ");
	            else if (mismatches.contains(i))
	                builder.append("- ");
	            else
	                builder.append("  ");
	            add(line);
	        }
    	}
        return this;
    }

    private TextReport error(String captionText, String message) {
        caption(captionText);
        add(message);
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#error(java.lang.String)
     */
    @Override
    public TextReport error(String message) {
        error("Error", message);
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#systemError(java.lang.String)
     */
    @Override
    public TextReport systemError(String message) {
        error("System Error", message);
        return this;
    }
    
    @Override
    public TextReport systemError(Throwable t) {
        return systemError(Util.getStackTrace(t));
    }    

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Report#image(java.lang.String, byte[])
     */
    @Override
    public TextReport image(String captionText, BufferedImage img) {
        image(img);
        return this;
    }

    @Override
    public TextReport image(BufferedImage image) {
        /*
        try {
            imageCount++;
            Path out = dir.resolve("report" + imageCount + ".png");
            ImageIO.write(image, "PNG", out.toFile());
        } catch (IOException ex) {
            error("No image");
        }
        */
        return this;
    }

    @Override
    public TextReport file(String file, String contents) {
        if ("studentFiles".equals(section) || "providedFiles".equals(section)) return this;
        caption(file);
        add(contents); // TODO: Line numbers?
        return this;
    }
    
    public TextReport file(String fileName, byte[] contents, boolean hidden) {
    	// Only happens in providedFiles
    	if (hidden) {
    		caption(fileName);
            add("[Hidden]");                		
    	}
     	return this;
    }

    @Override
    public TextReport run(String caption, String mainclass, boolean hidden) {
        caption(caption);
        this.hidden = hidden;
        return this;
    }

    @Override
    public TextReport add(Score score) {
        if (footnotes.size() > 0) {
            builder.append("\n---\n");
            for (String footnote : footnotes) {
                builder.append(footnote);
                builder.append("\n");
            }
        }
        builder.append("\n");
        add("Score");
        add("" + score);
        return this;
    }

    @Override
    public String getText() { return builder.toString(); }
    
    @Override
    public String extension() { return "txt"; }

    @Override
    public TextReport pass(boolean b) {
        if (b)
            add("[pass]");
        else
            add("[fail]");
        return this;
    }

    private int longest(String header, String[][] entries, int col) {
        int longest = header.length();
        for (int i = 0; i < entries.length; i++)
            longest = Math.max(longest, entries[i][col].length());
        return longest;
    }

    private int longest(String header, String[] entries) {
        int longest = header.length();
        for (int i = 0; i < entries.length; i++)
            longest = Math.max(longest, entries[i].trim().length());
        return longest;
    }

    private void repeat(char c, int count) {
        for (int i = 0; i < count; i++)
            builder.append(c);
    }

    private void pad(String s, int col) {
        s = s.trim().replace("\n", " ");
        builder.append(s);
        repeat(' ', col - s.length());
    }

    @Override
    public TextReport compareTokens(String filename, List<Match> matchData) {
        caption(filename);
        String caption1 = "Actual output";
        String caption2 = "Expected output";

        int col1 = caption1.length() + 3;
        int col2 = caption2.length();
        for (Match m : matchData) {
            col1 = Math.max(col1, m.actual.length());
            col2 = Math.max(col2, m.expected.length());
        }

        builder.append("  ");
        pad(caption1, col1);
        add(caption2);
        builder.append("  ");
        repeat('-', col1 + col2);
        builder.append("\n");
        for (Match m : matchData) {
            if (m.matches)
                builder.append("  ");
            else
                builder.append("- ");

            pad(m.actual, col1);
            add(m.expected);
        }
        return this;
    }

    @Override
    public TextReport runTable(String[] methodNames, String[] argNames,
            String[][] args, String[] actual, String[] expected,
            boolean[] outcomes, boolean[] hidden, String mainclass) {

        int cols0 = 0;
        if (methodNames != null) {
            for (String n : methodNames)
                cols0 = Math.max(cols0, n.length());
            cols0++;
        }

        int n = args[0].length;
        int[] cols = new int[n + 2];
        for (int c = 0; c < n; c++)
            cols[c] = longest(argNames[c], args, c) + 1;
        cols[n] = longest("Actual", actual) + 1;
        cols[n + 1] = longest("Expected", expected) + 1;

        pad("", cols0);
        for (int j = 0; j < n; j++)
            pad(argNames[j], cols[j]);
        pad("Actual", cols[n]);
        add("Expected");
        repeat('-', cols0);
        for (int j = 0; j < n + 2; j++)
            repeat('-', cols[j]);
        add("------");

        for (int i = 0; i < args.length; i++) {
        	if (hidden != null && hidden[i]) {
	            pad("?", cols0);
	            for (int j = 0; j < n; j++)
	                pad("?", cols[j]);
	            pad("?", cols[n]);
	            pad("?", cols[n + 1]);        	
        	} else {
	            if (methodNames != null)
	                pad(methodNames[i], cols0);
	            for (int j = 0; j < n; j++)
	                pad(args[i][j], cols[j]);
	            pad(actual[i], cols[n]);
	            pad(expected[i], cols[n + 1]);
        	}
            pass(outcomes[i]);
        }
        return this;
    }

    public TextReport comment(String key, String value) {
        if (builder.length() > 0 && builder.charAt(builder.length() - 1) != '\n')
            builder.append('\n');
        builder.append("# ");
        builder.append(key);
        builder.append(": ");
        builder.append(value);
        builder.append('\n');
        return this;
    }
}
