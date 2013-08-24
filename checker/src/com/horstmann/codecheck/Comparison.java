package com.horstmann.codecheck;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Comparison {
    private double tolerance = 1.0E-6;
    private boolean ignoreCase = true;
    private boolean ignoreSpace = true;
    
    public boolean execute(String actual, String expected, Report report, String title) throws IOException {
        List<String> lines1 = getLines(actual);
        List<String> lines2 = getLines(expected);

        List<Boolean> matches = new ArrayList<Boolean>();
        boolean outcome = lines1.size() == lines2.size();
        for (int i = 0; i < lines1.size() && i < lines2.size(); i++) {
           String line1 = lines1.get(i);
           String line2 = lines2.get(i);
           boolean b = compare(line1, line2);
           outcome &= b;
           matches.add(b);
        }
        if (outcome) 
           report.output(title, actual);
        else 
           report.compareTokens(matches, lines1, lines2);
        return outcome;
    }

    private static List<String> getLines(String contents) {
        List<String> r = new ArrayList<String>();
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
    
    public boolean compare(String a, String b) {
        String[] as = getTokens(a);
        String[] bs = getTokens(b);
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
