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
import java.util.regex.Pattern;

public class Annotations {
    public static final Set<String> VALID_ANNOTATIONS = Set.of(
            "HIDDEN", "HIDE", "SHOW", "EDIT", "SOLUTION", "CALL", "SUB", "ID", "SAMPLE", "ARGS", 
            "IN", "OUT", "TIMEOUT", "TOLERANCE", "IGNORECASE", "IGNORESPACE", "MAXOUTPUTLEN",
            "REQUIRED", "FORBIDDEN", "SCORING", "INTERLEAVE", "TILE", "FIXED", "OR", "PSEUDO");    
    public static final Set<String> NON_BLANK_BEFORE_OK = Set.of("SUB", "PSEUDO"); 

    public static class Annotation {
        public boolean isValid;
        public Path path;
        public String key = "";
        public String args;
        public String before;
        public String next = "";
    }
    
    public static Annotation parse(String line, String start, String end) {
        Annotation ann = new Annotation();
        int i = line.indexOf(start);
        if (i < 0) return ann;
        String before = line.substring(0, i); 
        i += start.length();
        String line1 = line.stripTrailing();
        if (!line1.endsWith(end)) return ann;
        int j = line1.length() - end.length(); 
        int k = i;
        while (k < j && Character.isAlphabetic(line.charAt(k))) k++;
        if (k < j && !Character.isWhitespace(line.charAt(k))) return ann;
        String key = line.substring(i, k);
        if (!VALID_ANNOTATIONS.contains(key)) return ann;
        // Only a few annotations can have non-blank before
        if (!before.isBlank() && !NON_BLANK_BEFORE_OK.contains(key)) return ann;
        ann.isValid = true;
        ann.before = before;
        ann.key = key;
        ann.args = line.substring(k, j).strip();
        return ann;
    }               
    

    private Language language;
    private List<Annotation> annotations = new ArrayList<>();
    private Set<String> keys = new HashSet<>();
    private Set<Path> solutions = new TreeSet<>();
    private Set<Path> hidden = new TreeSet<>();
    private Set<Path> hiddenTests = new TreeSet<>(); 
    
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
                if (a.key.equals("HIDDEN"))
                    hiddenTests.add(p);  
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

    public Set<Path> getHiddenTests() {
        return Collections.unmodifiableSet(hiddenTests);
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
            if (getHiddenTests().contains(a.path)) // OR (a.key.equals("HIDDEN"))
                sub.setHidden(true); 
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
            if (getHiddenTests().contains(a.path)) // OR (a.key.equals("HIDDEN"))
                calls.setHidden(true); 
        }
        return calls;
    }
    
    // having HIDDEN work with specific CALLs
    // public Calls findCalls() {
    //     Calls calls = new Calls(language);
    //     int callNum = 0; 
    //     for (int a=0; a<annotations.size(); a++) {
    //         if (annotations.get(a).key.equals("CALL")) {
    //             calls.addCall(annotations.get(a).path, annotations.get(a).args, annotations.get(a).next);
    //             if (a > 0 && annotations.get(a-1).key.equals("HIDDEN")) {
    //                 calls.getCall(callNum).setHidden(true);
    //             }
    //             callNum += 1; 
    //         }
    //     }
    //     return calls;
    // }
}
