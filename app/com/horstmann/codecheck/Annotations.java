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
    private static Set<String> validAnnotations = new HashSet<>(Arrays.asList(
            "HIDE", "SHOW", "EDIT", "SOLUTION", "CALL", "SUB", "ID", "SAMPLE", "ARGS", 
            "IN", "OUT", "TIMEOUT", "TOLERANCE", "IGNORECASE", "IGNORESPACE", "MAXOUTPUTLEN",
            "REQUIRED", "FORBIDDEN", "NOSCORE"));    
    
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
        Pattern pattern = Pattern.compile("(.*\\s|)" + delims[0] + "([A-Z\\[\\]]+)(\\s.*|)" + delims[1] + "\\s*");
        List<String> lines =  Util.lines(contents);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            Matcher matcher = pattern.matcher(line);
            if (matcher.matches()) {
                Annotation a = new Annotation();
                a.before = matcher.group(1).trim();
                a.key = matcher.group(2);
                if (!validAnnotations.contains(a.key))
                    throw new CodeCheckException("Unknown pseudocomment " + a.key + " in " + a.path);                
                keys.add(a.key);
                a.args = matcher.group(3).trim();
                a.next = "";
                if (i < lines.size() - 1) {
                    line = lines.get(i + 1);
                    if (!pattern.matcher(line).matches())
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
}
