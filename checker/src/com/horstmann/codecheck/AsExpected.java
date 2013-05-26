package com.horstmann.codecheck;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;

public class AsExpected {
	private Comparison comp;
	
	public AsExpected(Comparison comp) {
		this.comp = comp;
	}
	
    public boolean eval(String value, Report report, Score score) {
    	boolean outcome = true;
        List<String> lines = new ArrayList<>();
        try (Scanner in = new Scanner(value)) {
            while (in.hasNextLine())
                lines.add(in.nextLine());
            Set<Integer> matches = new HashSet<>();
            Set<Integer> mismatches = new HashSet<>();
            if (lines.size() > 0 && getPrefix(lines.get(0)).equals("expected")) { 
                report.error("No actual value for \"Expected: ...\" in line 1\n");
                outcome = false;
                mismatches.add(0);
                score.fail();
            }
            for (int i = 1; i < lines.size(); i++)
                if (getPrefix(lines.get(i)).equals("expected")) {
                	String actual = lines.get(i - 1);
                	String expected = lines.get(i);
                
                	// The condition after || is needed if actual has no prefix
                    // but a colon
                    if (comp.compare(getSuffix(actual), getSuffix(expected)) 
                    		|| comp.compare(actual, getSuffix(expected))) {
                        score.pass();
                    	matches.add(i);
                    }
                    else {
                    	mismatches.add(i);
                    	score.fail();
                    }
                }
            if (matches.size() == 0 && mismatches.size() == 0) {
                report.output(value);
                report.error("Missing \"Expected: ...\"\n");
                outcome = false;
            } else {
            	report.output(lines, matches, mismatches);
            }
        }
        return outcome;
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
}
