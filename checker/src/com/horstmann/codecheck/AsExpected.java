package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Scanner;

public class AsExpected {
    private double tolerance = 1.0E-6;
    private boolean outcome;

    public void setTolerance(double tolerance) {
        this.tolerance = tolerance;
    }

    public boolean getOutcome() {
        return outcome;
    }

    // TODO: Only take the last non-blank line before expected
    // (any contents)
    // ...
    // (any contents)
    // (optional tag and colon) (actual)
    // Expected: (expected)
    // repeat
    // Give error when reaching end of input without Expected:

    // TODO: Change this into a side-by-side table

    public String eval(String value, Properties props, Score score) {
        StringBuilder result = new StringBuilder();
        List<String> lines = new ArrayList<>();
        try (Scanner in = new Scanner(value)) {
            while (in.hasNextLine())
                lines.add(in.nextLine());
            List<Integer> expecteds = new ArrayList<>();
            for (int i = 0; i < lines.size(); i++)
                if (getPrefix(lines.get(i)).equals("expected"))
                    expecteds.add(i);
            result.append("<pre>");
            outcome = true;
            if (expecteds.size() == 0) {
                result.append(Report.htmlEscape(value));
                result.append(props.getProperty("expected.unexpectedStart"));
                result.append("Missing \"Expected: ...\"\n");
                result.append(props.getProperty("expected.unexpectedEnd"));
                outcome = false;
            } else {
                int copy = 0;
                for (int e : expecteds) {
                    String actual = "";
                    while (copy < e) {
                        String line = lines.get(copy);
                        if (line.trim().length() > 0)
                            actual = line;
                        result.append(Report.htmlEscape(line));
                        result.append("\n");
                        copy++;
                    }
                    String expected = lines.get(copy);
                    // The condition after || is needed if actual has no prefix
                    // but a colon
                    if (getPrefix(expected).equals("expected")
                            && (match(getSuffix(actual), getSuffix(expected)) || match(actual.trim().toLowerCase(),
                                    getSuffix(expected)))) {
                        result.append(props.getProperty("expected.matchedStart"));
                        result.append(Report.htmlEscape(expected));
                        result.append(props.getProperty("expected.matchedEnd"));
                        score.pass();
                    } else {
                        result.append(props.getProperty("expected.unexpectedStart"));
                        result.append(Report.htmlEscape(expected));
                        result.append(props.getProperty("expected.unexpectedEnd"));
                        score.fail();
                        outcome = false;
                    }
                    result.append("\n");
                    copy++;
                }
                while (copy < lines.size()) {
                    String line = lines.get(copy);
                    result.append(Report.htmlEscape(line));
                    result.append("\n");
                    copy++;
                }
            }
        }
        result.append("</pre>");
        return result.toString();
    }

    private static String getPrefix(String s) {
        int n = s.indexOf(':');
        if (n == -1)
            return "";
        else
            return s.substring(0, n).toLowerCase();
    }

    private static String getSuffix(String s) {
        int n = s.indexOf(':');
        if (n == -1)
            return s.trim().toLowerCase();
        else
            return s.substring(n + 1).trim().toLowerCase();
    }

    private boolean match(String a, String b) {
        if (a.equals(b))
            return true;
        try {
            double x = Double.parseDouble(a);
            double y = Double.parseDouble(b);
            if (Math.abs(x - y) < tolerance)
                return true;
        } catch (NumberFormatException ex) {
            // at least one of them isn't a number
        }
        return false;
    }

    public static int size(Path p) {
        int sz = 0;
        for (String line: Util.readLines(p)) {
            if (line.contains("Expected: ")) sz++;
        }
        return sz;
    }
}
