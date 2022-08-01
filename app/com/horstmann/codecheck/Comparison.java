package com.horstmann.codecheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Comparison {
    private double tolerance = 1.0E-6;
    private boolean ignoreCase = true;
    private boolean ignoreSpace = true;
    private static final int MANY_MORE_LINES = 10;
        // If actual lines > expected + MANY_MORE_LINES, truncate actual output
    
    public boolean execute(String input, String actual, String expected, Report report, String filename, boolean hidden) {
        List<String> lines1 = getLines(actual.replaceAll("〈[^〉]*〉\n", "")); 
        List<String> lines2 = getLines(expected.replaceAll("〈[^〉]*〉\n", ""));

        List<Report.Match> matches = new ArrayList<>();
        boolean outcome = lines1.size() == lines2.size();
        int i;
        for (i = 0; i < lines1.size() && i < lines2.size(); i++) {
           String line1 = lines1.get(i);
           String line2 = lines2.get(i);
           Report.Match m = compare(line1, line2);
           outcome &= m.matches;
           matches.add(m);
        }
        if (outcome) {
            if (filename != null) {
                report.file(filename, actual);
            }
            else {
                if (!hidden)
                    report.output(actual);
                else 
                    //report.comment("hidden", "Given the instructions, the output is hidden!");
                    report.hiddenOutputMessage();

            }
        }
        else {
            // Print inputs which are getting replaced
            report.input(input);
            while (i < lines2.size()) {
                Report.Match m = new Report.Match();
                m.actual = "";
                m.expected = lines2.get(i);
                m.matches = false;
                m.explanation = null;
                matches.add(m);
                i++;
            }
            while (i < lines1.size() && i < lines2.size() + MANY_MORE_LINES) {
                Report.Match m = new Report.Match();
                m.actual = lines1.get(i);
                m.expected = "";
                m.matches = false;
                m.explanation = null;
                matches.add(m);
                i++;
            }
            if (i < lines1.size()) {
                Report.Match m = new Report.Match();
                m.actual = ". . .";
                m.expected = "";
                m.matches = false;
                m.explanation = null;
                matches.add(m);
            }
            report.compareTokens(filename, matches);
        }
        return outcome;
        /*
        // TODO: This is where the newlines went to die
        
        List<String> lines1 = getLines(actual.replace("〉\n", "〉")); 
        List<String> lines2 = getLines(expected.replace("〉\n", "〉"));

        List<Report.Match> matches = new ArrayList<>();
        boolean outcome = lines1.size() == lines2.size();
        int i;
        for (i = 0; i < lines1.size() && i < lines2.size(); i++) {
           String line1 = lines1.get(i).replaceAll("〈[^〉]*〉", "");
           String line2 = lines2.get(i).replaceAll("〈[^〉]*〉", "");
           Report.Match m = compare(line1, line2);
           outcome &= m.matches;
           matches.add(m);
        }
        if (outcome) {
            // TODO: Report needs to deal with 〈...〉 (replace with <b>...</b>\n)
            if (filename != null) {
                report.file(filename, actual);
            }
            else {
                report.output(actual);
            }
        }
        else {
            // Print inputs which are getting replaced
            report.input(input);
            while (i < lines2.size()) {
                Report.Match m = new Report.Match();
                m.actual = "";
                m.expected = lines2.get(i).replaceAll("〈[^〉]*〉", "");
                m.matches = false;
                m.explanation = null;
                matches.add(m);
                i++;
            }
            while (i < lines1.size() && i < lines2.size() + MANY_MORE_LINES) {
                Report.Match m = new Report.Match();
                m.actual = lines1.get(i).replaceAll("〈[^〉]*〉", "");
                m.expected = "";
                m.matches = false;
                m.explanation = null;
                matches.add(m);
                i++;
            }
            if (i < lines1.size()) {
                Report.Match m = new Report.Match();
                m.actual = ". . .";
                m.expected = "";
                m.matches = false;
                m.explanation = null;
                matches.add(m);
            }
            report.compareTokens(filename, matches);
        }
        return outcome;
        */
    }

    private static List<String> getLines(String contents) {
        List<String> r = new ArrayList<String>();
        if (contents == null) return r;
        Scanner in = new Scanner(contents);
        boolean first = true;
        while (in.hasNextLine()) {
            if (first) {
                r.add(in.nextLine().replace("\uFEFF", "")); // strip out BOM.
                first = false;
            }
            else r.add(in.nextLine());
        }
        in.close();
        // Trim blank lines from end
        int i = r.size() - 1;
        while (i >= 0 && r.get(i).trim().isEmpty()) { r.remove(i); i--; }
        return r;
    }

    private String[] getTokens(String line) {
        if (!ignoreSpace) return new String[] { line };
        List<String> r = new ArrayList<String>();
        Scanner in = new Scanner(line);
        while (in.hasNext()) r.add(in.next());
        in.close();
        return r.toArray(new String[r.size()]);
    }
    
    private static String normalizeWS(String s) { 
        return s.replaceAll("\\pZ+", " ").trim(); 
    }
    
    public Report.Match compare(String a, String b) {
        Report.Match m = new Report.Match();
        m.actual = a;
        m.expected = b;
        m.matches = false;
        m.explanation = null;
        
        String[] as = getTokens(a);
        String[] bs = getTokens(b);
        if (as.length != bs.length)
            return m;
        for (int i = 0; i < as.length; i++)
            if (!compareAsNumbers(as[i], bs[i])) {
                if (ignoreCase) {
                    if (!as[i].equalsIgnoreCase(bs[i])) {
                        if (!ignoreSpace && normalizeWS(as[i]).equalsIgnoreCase(normalizeWS(bs[i])))
                            m.explanation = "Check white space";
                        return m;
                    }
                } else {
                    if (!as[i].equals(bs[i])) {
                        if (as[i].equalsIgnoreCase(bs[i])) {
                            m.explanation = "Check letter case";
                        }
                        else if (!ignoreSpace && normalizeWS(as[i]).equals(normalizeWS(bs[i])))
                            m.explanation = "Check white space";
                        return m;
                    }
                }
            }
        m.matches = true;
        return m;
    }

    public boolean compareAsNumbers(String a, String b) {
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

    public void setTolerance(double tolerance) {
        this.tolerance = tolerance;
    }
    
    public void setIgnoreSpace(boolean ignoreSpace) {
        this.ignoreSpace = ignoreSpace;
    }
}
