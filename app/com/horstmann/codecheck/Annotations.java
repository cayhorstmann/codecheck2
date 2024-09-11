package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

public class Annotations {
    public static final Set<String> VALID_ANNOTATIONS = new LinkedHashSet<>(List.of(
            "CALL HIDDEN", "IN HIDDEN", "HIDDEN", "HIDE", "SHOW", "EDIT", "SOLUTION", "CALL", "SUB", "ID", "SAMPLE", "ARGS", 
            "IN", "OUT", "TIMEOUT", "TOLERANCE", "IGNORECASE", "IGNORESPACE", "MAXOUTPUTLEN",
            "REQUIRED", "FORBIDDEN", "SCORING", "INTERLEAVE", "TILE", "FIXED", "OR", "PSEUDO"));    
    public static final Set<String> NON_BLANK_BEFORE_OK = Set.of("SUB", "PSEUDO"); 

    public static class Annotation {
        public boolean isValid;
        public Path path;
        public String key = "";
        public String args;
        public String before;
        public String next = "";
    }
    
    /**
        @param line the line of code to parse
        @param start the starting comment delimiter
        @param end the ending comment delimiter
        @return the parsed annotation, or an empty annotation if none found
     */
    public static Annotation parse(String line, String start, String end) {
        Annotation ann = new Annotation();
        int i = line.indexOf(start);
        if (i < 0) return ann;
        String before = line.substring(0, i); 
        i += start.length();
        String line1 = line.substring(i).stripTrailing();
        if (end.length() > 0) {
            if (!line1.endsWith(end)) return ann;
            line1 = line1.substring(0, line1.length() - end.length()).stripTrailing();
        }
        boolean found = false;
        Iterator<String> iter = VALID_ANNOTATIONS.iterator();
        String key = "";
        while (!found && iter.hasNext()) {
            key = iter.next();
            if (line1.startsWith(key)) found = true;
        }
        if (!found) return ann;
        if (key.length() < line1.length() && !Character.isWhitespace(line1.charAt(key.length()))) return ann;
        // Only a few annotations can have non-blank before
        if (!before.isBlank() && !NON_BLANK_BEFORE_OK.contains(key)) return ann;
        ann.isValid = true;
        ann.before = before;
        ann.key = key;
        ann.args = line1.substring(key.length()).strip();
        return ann;
    }               

    private Language language;
    private List<Annotation> annotations = new ArrayList<>();
    private Set<String> keys = new HashSet<>();
    private Set<Path> solutions = new TreeSet<>();
    private Set<Path> hidden = new TreeSet<>();
    private Set<Path> hiddenCallFiles = new TreeSet<>();
    private Set<Path> hiddenTestFiles = new TreeSet<>();
    
    public Annotations(Language language) {
        this.language = language;
    }

    public void read(Map<Path, byte[]> files) {
        for (Map.Entry<Path, byte[]> entry : files.entrySet()) read(entry.getKey(), entry.getValue());
    }

    public static String getArg(String line, String key, String start, String end) {
        int k = line.indexOf(start + key);
        return line.substring(k + start.length() + key.length(), line.length() - end.length()).strip();     
    }
    
    private void read(Path p, byte[] contents) {
        if (!language.isSource(p)) return; 
        String[] delims = language.pseudoCommentDelimiters();
        List<String> lines =  Util.lines(contents);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            Annotation a = parse(line, delims[0], delims[1]);
            if (a.isValid) {
                keys.add(a.key);
                if (i < lines.size() - 1) {
                    line = lines.get(i + 1);
                    if (!parse(line, delims[0], delims[1]).isValid)
                        a.next = line.trim();
                }
                if (Arrays.asList("SOLUTION", "SHOW", "EDIT", "TILE").contains(a.key)) 
                    solutions.add(p);
                if (a.key.equals("HIDE")) 
                    hidden.add(p);
                if (a.key.equals("CALL HIDDEN"))
                    hiddenCallFiles.add(p); 
                if (a.key.equals("HIDDEN")) {
                    hiddenTestFiles.add(p); 
                    hidden.add(p);
                }
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

    public Set<Path> getHiddenCallFiles() {
        return Collections.unmodifiableSet(hiddenCallFiles);
    }

    public Set<Path> getHiddenTestFiles() {
        return Collections.unmodifiableSet(hiddenTestFiles);
    }

    public String findUnique(String key) {
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
    
    public List<String> findAll(String key) {
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
        String[] delims = language.pseudoCommentDelimiters();
        for (Annotation a : annotations) {
            boolean forbidden = a.key.equals("FORBIDDEN");
            if (a.key.equals("REQUIRED") || forbidden) {
                StringBuilder contents = new StringBuilder();
                for (String line : Util.lines(submissionFiles.get(a.path))) {
                	String commentPattern = Pattern.quote(delims[0]) + ".*" + Pattern.quote(delims[1]);
                    contents.append(line.replaceAll(commentPattern, ""));
                    contents.append(" ");
                }
                boolean found = Pattern.compile(a.args).matcher(contents).find();
                if (found == forbidden) { // found && forbidden || !found && required
                    String nextLine = a.next;
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

    /*
     * TODO: It would be more robust if substitutions could only be
     * in non-editable lines. 
     */
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
        int callNum = 0; 
        for (int a=0; a<annotations.size(); a++) {
            if (annotations.get(a).key.equals("CALL")) {
                 calls.addCall(annotations.get(a).path, annotations.get(a).args, annotations.get(a).next);
                 callNum += 1; 
            }
            else if (annotations.get(a).key.equals("CALL HIDDEN")) {
                calls.addCall(annotations.get(a).path, annotations.get(a).args, annotations.get(a).next);
                calls.getCall(callNum).setHidden(true);
                callNum += 1; 
            }
        }
        return calls;
    }
}
