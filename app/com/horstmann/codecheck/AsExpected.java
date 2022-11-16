package com.horstmann.codecheck;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AsExpected {
    private Comparison comp;
    private boolean hidden = false; 
    
    public AsExpected(Comparison comp) {
        this.comp = comp;
    }
    
        // Scoring doesn't work when outerr contains an exception report because we don't know how many
        // test cases have not occurred. 
        // Need to count the number of expected cases in the file 
    
        public boolean isHidden() {
            return hidden; 
        }
    
        public void setHidden(boolean value)
        {
            hidden = value; 
        }

    public static int expectedTests(String contents) { // TODO: Shouldn't this be in Language?
        Pattern expecteds = Pattern.compile("Expected: "); // TODO: What if this is commented out? 
        Matcher matcher = expecteds.matcher(contents);
        int count = 0;
        while (matcher.find())
            count++;
        return count;
    }
    
    public void eval(String value, Report report, Score score, String tester) {
        int expectedTests = expectedTests(tester);
        List<String> lines = new ArrayList<>();
        try (Scanner in = new Scanner(value)) {
            while (in.hasNextLine())
                lines.add(in.nextLine());
            Set<Integer> matches = new HashSet<>();
            Set<Integer> mismatches = new HashSet<>();
            int actualTests = 0;
            for (int i = 0; i < lines.size(); i++) {
                if (getPrefix(lines.get(i)).equals("expected")) {
                    actualTests++;
                    if (i == 0) {
                        report.error("No actual value for \"Expected: ...\" in line 1\n");
                        mismatches.add(0);
                        score.pass(false, report);                      
                    } else {
                        String expected = lines.get(i);
                        String actual = lines.get(i - 1);                   
                        // The second comparison is needed if actual has no prefix
                        // but a colon
                        Report.Match m = comp.compare(getSuffix(actual), getSuffix(expected));
                        if (!m.matches) m = comp.compare(actual, getSuffix(expected));
                        if (m.matches) {
                            score.pass(true, report);
                            matches.add(i);
                        }
                        else {
                            mismatches.add(i);
                            score.pass(false, report);
                        }
                    }
                }
            }
            if (matches.size() == 0 && mismatches.size() == 0) {
                report.output(value);
                report.error("Missing \"Expected: ...\"\n");
            } else {
                if (hidden == false)
                    report.output(lines, matches, mismatches);
                else 
                    report.hiddenOutputMessage(); 
            }
            // If the program run ends in an exception, it can happen that the number of
            // actual vs. expected values is less than it should be. In that case,
            // the score must be adjusted.
            if (actualTests < expectedTests) {
                int diff = expectedTests - actualTests;
                report.error("Program exited before all expected values were printed.");
                for (int i = 1; i <= diff; i++) score.pass(false, report);
            }
        }
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
            return s;
        else {
            if (n + 1 < s.length() && s.charAt(n + 1) == ' ') n++;        
            return s.substring(n + 1);
        }
    }
}
