package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class Annotations {
    private static Set<String> validAnnotations = new HashSet<>(Arrays.asList(
            "HIDE", "CALL", "SUB", "ID", "SAMPLE", "ARGS", "OUT", "TIMEOUT", "TOLERANCE", "IGNORECASE", "IGNORESPACE", "REQUIRED", "FORBIDDEN", "NOSCORE", "FOR"));    
    
    class Annotation {
        Path path;
        String key;
        String args;
        String before;
        String next;
        boolean inSolution;
    }

    private Language language;
    private List<Annotation> annotations = new ArrayList<>();
    private Set<String> keys = new HashSet<>();
    
    public Annotations(Language language) {
        this.language = language;
    }

    public void read(Path dir, Set<Path> ps, boolean inSolution) {
        for (Path p : ps) read(dir, p, inSolution);
    }

    private void read(Path dir, Path p, boolean inSolution) {
    	String[] delims = language.pseudoCommentDelimiters();
        Pattern pattern = Pattern.compile("(.* |)" + delims[0] + "([A-Z\\[\\]]+)( .*|)" + delims[1]);
        List<String> lines =  Util.readLines(dir.resolve(p));
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
                a.inSolution = inSolution;
                a.path = p;
                annotations.add(a);
            }
        }
    }

    public void check(Report r) {
        // Student file annotations only make sense when the file has HIDE
        Set<Path> hidden = new HashSet<>();
        for (Annotation a : annotations) {
            if (a.key.equals("HIDE")) hidden.add(a.path);
        }  
        
        for (Annotation a : annotations) {
            boolean ok = validAnnotations.contains(a.key)
                    && (a.inSolution && !a.key.equals("HIDE") || 
                       !a.inSolution && hidden.contains(a.path)); 
            if (!ok)
                r.systemError("Unknown pseudocomment " + a.key + " in " + a.path);
        }
    }
    
    public Set<Path> findHidden() {
        Set<Path> result = new HashSet<>();
        for (Annotation a : annotations) {
            if (a.key.equals("HIDE")) result.add(Util.tail(a.path));
        }
        return result;
    }

    public String findUniqueKey(String key) {
        Annotation match = null;
        for (Annotation a : annotations) {
            if (a.key.equals(key)) {
                if (match == null) match = a;
                else if (!match.args.equals(a.args)) throw new RuntimeException("Duplicate " + key + " in " + a.path + " and " + match.path);
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
                        throw new RuntimeException(key + " has bad double argument " + a.args + " in " + a.path);
                    }
                } else if (!match.args.equals(a.args)) throw new RuntimeException("Duplicate " + key + " in " + a.path + " and " + match.path);
            }
        }
        return match == null ? defaultValue : result;
    }


    public boolean checkConditions(Path dir, Report report) {
        for (Annotation a : annotations) {
            boolean forbidden = a.key.equals("FORBIDDEN");
            if (a.key.equals("REQUIRED") || forbidden) {
                Path p = Util.tail(a.path);
                StringBuilder contents = new StringBuilder();
                for (String line : Util.readLines(dir.resolve(p)))
                {
                   // TODO: Removing comments like this is language specific
                	contents.append(line.replaceAll("//.*$", ""));
               		contents.append(" ");
                }
                boolean found = Pattern.compile(a.args).matcher(contents).find();
                if (found == forbidden) { // found && forbidden || !found && required
                    String message = a.next.startsWith("//") ? a.next.substring(2).trim() :
                                     ((forbidden ? "Found " : "Did not find ") + a.args);
                    report.error(p + ": " + message);
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
            if (a.key.equals("SAMPLE") && Util.tail(a.path).equals(p)) return true;
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
