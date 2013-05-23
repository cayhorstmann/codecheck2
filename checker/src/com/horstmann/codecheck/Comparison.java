package com.horstmann.codecheck;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Scanner;

public class Comparison {
    public static final String DEFAULT_TOKEN = "line";
    private boolean outcome;

    private String token = DEFAULT_TOKEN;
    private double tolerance = 1.0E-12;
    private String ignoreRegex = "[\\W&&[^+-.]]"; // TODO
    private boolean ignoreCase = true;

    public boolean getOutcome() {
        return outcome;
    }

    public String execute(String contents1, String contents2, Properties props) throws IOException {
        StringBuilder result = new StringBuilder();
        List<String> tokens1 = getTokens(contents1);
        List<String> tokens2 = getTokens(contents2);

        while (tokens1.size() < tokens2.size())
            tokens1.add("");
        while (tokens2.size() < tokens1.size())
            tokens2.add("");

        // TODO: Fix highlighting

        outcome = true;

        StringBuilder output1 = new StringBuilder();
        StringBuilder output2 = new StringBuilder();
        for (int i = 0; i < tokens1.size(); i++) {
            String token1 = trimEnd(tokens1.get(i));
            String token2 = trimEnd(tokens2.get(i));
            boolean b = compare(token1, token2);
            if (b && token1.equals(token2)) {
                output1.append(Report.htmlEscape(token1));
            } else {
                if (token1.length() > 0) {
                    output1.append(props.getProperty(b ? "comparison.questionableStart" : "comparison.mismatchStart"));
                    output1.append(Report.htmlEscape(token1));
                    output1.append(props.getProperty(b ? "comparison.questionableEnd" : "comparison.mismatchEnd"));
                }
                // output2.append(props.getProperty("comparison.expectedStart"));
                // output2.append(props.getProperty("comparison.expectedEnd"));
                outcome = false;
            }
            output1.append("\n");
            output2.append(Report.htmlEscape(token2));
            output2.append("\n");
        }

        if (outcome) {
            result.append("<pre>");
            result.append(output1);
            result.append("</pre>");
        } else {
            result.append(props.getProperty("comparison.start"));
            result.append(props.getProperty("callmethod.rowStart"));
            result.append(props.getProperty("callmethod.cellStart"));
            result.append("<pre>");
            result.append(output1);
            result.append("</pre>");
            result.append(props.getProperty("callmethod.cellEnd"));
            result.append(props.getProperty("callmethod.cellStart"));
            result.append("<pre>");
            result.append(output2);
            result.append("</pre>");
            result.append(props.getProperty("callmethod.cellEnd"));
            result.append(props.getProperty("callmethod.rowEnd"));
            result.append(props.getProperty("callmethod.end"));
        }
        return result.toString();
    }

    private String trimEnd(String s) {
        if (s == null)
            return s;
        int i = s.length() - 1;
        while (i >= 0 && Character.isSpaceChar(s.charAt(i)))
            i--;
        return s.substring(0, i + 1);
    }

    public List<String> getTokens(String contents) {
        List<String> r = new ArrayList<String>();
        Scanner in = new Scanner(contents);
        /*
         * TODO: Would this ever be useful for negative delimiters?
         */
        /*
         * String delimiter = ""; if (token.equals("line")) // TODO:
         * precompile delimiter = "[\\r][\\n]?|[\\n]"; else if
         * (token.equals("number")) delimiter = "[^0-9-]+"; else if
         * (token.equals("word")) delimiter = "[\\p{javaWhiteSpace}]+"; else
         * delimiter = token; // TODO: really? in.useDelimiter(delimiter);
         */
        String pattern = "";
        if (token.equals("line")) {
            while (in.hasNextLine())
                r.add(in.nextLine().replace("\uFEFF", "")); // strip out BOM. TODO: Only in first line?
            in.close();
            return r;
        } else if (token.equals("word")) {
            while (in.hasNext())
                r.add(in.next());
            in.close();
            return r;
        } else if (token.equals("number"))
            pattern = "[-+]?[0-9]+\\.?[0-9]*([eE][-+]?[0-9]+)?";
        else
            pattern = token;
        String s;
        while ((s = in.findWithinHorizon(pattern, 0)) != null)
            r.add(s);
        in.close();
        return r;
    }

    public boolean compare(String a, String b) {
        if (token.equals("number")) {
            return compareAsNumbers(a, b);
        }

        if (token.equals("word")) {
            return ignoreCase ? a.equalsIgnoreCase(b) : a.equals(b);
        }

        // TODO: Make ignoreRegex settable
        String[] as = a.trim().split(ignoreRegex);
        // TODO: doesn't seem quite right,
        // or maybe concat afterwards?
        String[] bs = b.trim().split(ignoreRegex);
        if (as.length != bs.length)
            return false;
        for (int i = 0; i < as.length; i++)
            if (!compareAsNumbers(as[i], bs[i])) {
                if (ignoreCase) {
                    if (!as[i].equalsIgnoreCase(bs[i]))
                        return false;
                } else {
                    if (!as[i].equals(bs[i]))
                        return false;
                }
            }
        return true;
    }

    private boolean compareAsNumbers(String a, String b) {
        try {
            double x = Double.parseDouble(a);
            double y = Double.parseDouble(b);
            return Math.abs(x - y) <= tolerance;
        } catch (NumberFormatException ex) {
            return false;
        }
    }

    public void setIgnoreCase(boolean ignoreCase) {
        this.ignoreCase = ignoreCase;
    }

    public void setIgnoreRegex(String ignoreRegex) {
        this.ignoreRegex = ignoreRegex;
    }

    public void setToken(String token) {
        if (token == null)
            this.token = DEFAULT_TOKEN;
        else
            this.token = token;
    }

    public void setTolerance(double tolerance) {
        this.tolerance = tolerance;
    }
}
