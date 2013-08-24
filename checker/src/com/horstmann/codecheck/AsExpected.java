package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AsExpected {
	private Comparison comp;
	
	public AsExpected(Comparison comp) {
		this.comp = comp;
	}
	
	public static int expectedTests(Path tester) {
		String contents = Util.read(tester);
		Pattern expecteds = Pattern.compile("System\\s*.\\s*out\\s*.\\s*print(ln)?\\s*\\(\\s*\"Expected: ");
		Matcher matcher = expecteds.matcher(contents);
		int count = 0;
		while (matcher.find())
		    count++;
		return count;
	}
	
    public void eval(String value, Report report, Score score, Path tester) {
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
                        score.fail();                		
                	} else {
                    	String expected = lines.get(i);
                		String actual = lines.get(i - 1);	                
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
                }
            }
            if (matches.size() == 0 && mismatches.size() == 0) {
                report.output(value);
                report.error("Missing \"Expected: ...\"\n");
            } else {
            	report.output(lines, matches, mismatches);
            }
            // If the program run ends in an exception, it can happen that the number of
            // actual vs. expected values is less than it should be. In that case,
            // the score must be adjusted.
            if (actualTests < expectedTests) {
            	int diff = expectedTests - actualTests;
            	report.error("Program exited before all expected values were printed.");
            	for (int i = 1; i <= diff; i++) score.fail();
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
            return s.trim().toLowerCase();
        else
            return s.substring(n + 1).trim().toLowerCase();
    }
}
