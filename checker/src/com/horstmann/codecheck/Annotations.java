package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
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

    public void read(Path useDir, Set<Path> useFiles, Path solutionDir, Set<Path> solutionFiles, Report r) {
        for (Path p : useFiles) read(useDir, p, false);
        for (Path p : solutionFiles) read(solutionDir, p, true);
        for (Annotation a : annotations) {
            if (a.key.equals("HIDE") && !solutions.contains(a.path)) hidden.add(a.path);
        }
        
        // Annotations in solution files and hidden files are ok. 
        for (Annotation a : annotations) {
            if (!validAnnotations.contains(a.key) ||
                    (!solutions.contains(a.path) && !hidden.contains(a.path)))                    
                r.systemError("Unknown pseudocomment " + a.key + " in " + a.path);
        }
    }

    private void read(Path dir, Path p, boolean inSolution) {
    	String[] delims = language.pseudoCommentDelimiters();
        Pattern pattern = Pattern.compile("(.*\\s|)" + delims[0] + "([A-Z\\[\\]]+)(\\s.*|)" + delims[1]);
        List<String> lines =  Util.readLines(dir.resolve(p));
        if (inSolution) solutions.add(p);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            Matcher matcher = pattern.matcher(line);
            if (matcher.matches()) {
                Annotation a = new Annotation();
                a.before = matcher.group(1).trim();
                a.key = matcher.group(2);
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


    public boolean checkConditions(Path dir, Report report) {
        for (Annotation a : annotations) {
            boolean forbidden = a.key.equals("FORBIDDEN");
            if (a.key.equals("REQUIRED") || forbidden) {
                StringBuilder contents = new StringBuilder();
                for (String line : Util.readLines(dir.resolve(a.path))) {
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
