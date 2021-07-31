package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Annotations {
    public static final Set<String> VALID_ANNOTATIONS = Set.of(
            "HIDE", "SHOW", "EDIT", "SOLUTION", "CALL", "SUB", "ID", "SAMPLE", "ARGS", 
            "IN", "OUT", "TIMEOUT", "TOLERANCE", "IGNORECASE", "IGNORESPACE", "MAXOUTPUTLEN",
            "REQUIRED", "FORBIDDEN", "SCORING", "INTERLEAVE");    
    
    private class Annotation {
        Path path;
        String key;
        String args;
        String before;
        String next;
    }

    private Language language;
    private List<Annotation> annotations = new ArrayList<>();
    private Set<String> keys = new HashSet<>();
    private Set<Path> solutions = new TreeSet<>();
    private Set<Path> hidden = new TreeSet<>();
    
    public Annotations(Language language) {
        this.language = language;
    }

    public void read(Map<Path, byte[]> files) {
        for (Map.Entry<Path, byte[]> entry : files.entrySet()) read(entry.getKey(), entry.getValue());
    }

    private void read(Path p, byte[] contents) {
    	if (!language.isSource(p)) return; 
    	String[] delims = language.pseudoCommentDelimiters();
        List<String> lines =  Util.lines(contents);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            String key = getPseudoComment(line, delims[0], delims[1]);
            if (key != null) {
                Annotation a = new Annotation();
                int k = line.indexOf(delims[0] + key);
                a.before = line.substring(0, k);
                a.key = key;
                keys.add(a.key);
                a.args = line.substring(k + delims[0].length() + key.length(), line.length() - delims[1].length()).trim();
                a.next = "";
                if (i < lines.size() - 1) {
                    line = lines.get(i + 1);
                    if (getPseudoComment(line, delims[0], delims[1]) == null)
                        a.next = line.trim();
                }
                if (Arrays.asList("SOLUTION", "SHOW", "EDIT").contains(a.key)) 
                    solutions.add(p);
                if (a.key.equals("HIDE")) 
                	hidden.add(p);
                a.path = p;
                annotations.add(a);
            }
        }
    }

    public Set<Path> getSolutions() {        
        return Collections.unmodifiableSet(solutions);
    }
    
    public Set<Path> getHidden() {
        return Collections.unmodifiableSet(hidden);
    }

    public String findUniqueKey(String key) {
        Annotation match = null;
        for (Annotation a : annotations) {
            if (a.key.equals(key)) {
                if (match == null) match = a;
                else if (!match.args.equals(a.args)) {
                    // If one of them is in a hidden file and the other is not,
                    // we'll allow the non-hidden one to override. This is useful 
                    // with ezgraphics.py that has ##OUT out.png which may occasionally
                    // be overridden
                    boolean aHidden = hidden.contains(a.path);
                    boolean matchHidden = hidden.contains(match.path);
                    if (aHidden == matchHidden)
                        throw new CodeCheckException("Duplicate " + key + " in " + a.path + " and " + match.path);
                    if (matchHidden) match = a;
                }
            }
        }
        return match == null ? null : match.args;
    }
    
    public List<String> findKeys(String key) {
        List<String> result = new ArrayList<>();
        for (Annotation a : annotations) 
            if (a.key.equals(key)) 
                result.add(a.args);
            
        return result;
    }
    
    public double findUniqueDoubleKey(String key, double defaultValue) {
        Annotation match = null;
        double result = 0;
        for (Annotation a : annotations) {
            if (a.key.equals(key)) {
                if (match == null) {
                    try {
                        result = Double.parseDouble(a.args);
                        match = a;
                    } catch (NumberFormatException ex) {
                        throw new CodeCheckException(key + " has bad double argument " + a.args + " in " + a.path);
                    }
                } else if (!match.args.equals(a.args)) throw new CodeCheckException("Duplicate " + key + " in " + a.path + " and " + match.path);
            }
        }
        return match == null ? defaultValue : result;
    }


    public boolean checkConditions(Map<Path, String> submissionFiles, Report report) {
        for (Annotation a : annotations) {
            boolean forbidden = a.key.equals("FORBIDDEN");
            if (a.key.equals("REQUIRED") || forbidden) {
                StringBuilder contents = new StringBuilder();
                for (String line : Util.lines(submissionFiles.get(a.path))) {
                    // TODO: Removing comments like this is language specific
                    contents.append(line.replaceAll("//.*$", ""));
                    contents.append(" ");
                }
                boolean found = Pattern.compile(a.args).matcher(contents).find();
                if (found == forbidden) { // found && forbidden || !found && required
                    String nextLine = a.next;
                    String[] delims = language.pseudoCommentDelimiters();
                    String message;
                    if (nextLine.startsWith(delims[0]) && nextLine.endsWith(delims[1]))
                        message = nextLine.substring(delims[0].length(), nextLine.length() - delims[1].length()).trim();
                    else 
                        message = (forbidden ? "Found " : "Did not find ") + a.args;
                    report.error(a.path + ": " + message);
                    return false;
                }
            }
        }
        return true;
    }

    public boolean has(String key) {
        return keys.contains(key);
    }

    public Substitution findSubstitution() {
        Substitution sub = new Substitution(language);
        for (Annotation a : annotations) {
            if (a.key.equals("SUB"))
                sub.addVariable(a.path, a.before, a.args);
        }
        return sub;
    }

    /**
     * Checks if a path is a sample
     * @param p the path without student/solution directory
     * @return true if the path was annotated as a sample
     */
    public boolean isSample(Path p) {
        for (Annotation a : annotations) {
            if (a.key.equals("SAMPLE") && a.path.equals(p)) return true;
        }
        return false;
    }

    public Calls findCalls() {
        Calls calls = new Calls(language);
        for (Annotation a : annotations) {
            if (a.key.equals("CALL"))
                calls.addCall(a.path, a.args, a.next);
        }
        return calls;
    }

	public static String getPseudoComment(String line, String start, String end) {
		//TODO: Would be more efficient to find the [A-Z]+ after start and check if in VALID_ANNOTATIONS
		for (String type : VALID_ANNOTATIONS) 
			if (isPseudocomment(line, type, start, end))
				return type;
		return null;
	}

	public static boolean isPseudocomment(String line, String type, String start, String end) {
		line = line.trim();
		if (!line.endsWith(end)) return false;
		if (type.equals("SUB")) return line.contains(start + type + " ");
		if (!line.startsWith(start + type)) return false;
			
		int slen = start.length();
		int tlen = type.length();
		int elen = end.length();
		if (line.length() == slen + tlen + elen)
			return true;
		// If there is stuff after the type, there must be a white space
		return Character.isWhitespace(line.charAt(slen + tlen));
	}
}
