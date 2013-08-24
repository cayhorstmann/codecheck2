package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class Annotations {
    class Annotation {
        Path path;
        String key;
        String args;
        String before;
        String next;
    }

    private List<Annotation> annotations = new ArrayList<>();
    private Set<String> keys = new HashSet<>();

    public void read(Path dir, Set<Path> ps) {
        for (Path p : ps) read(dir, p);
    }

    public void read(Path dir, Path p) {
        Pattern pattern = Pattern.compile("(.* |)//([A-Z]+)( .*|)");
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
                annotations.add(a);
                a.path = p;
            }
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
                else throw new RuntimeException("Duplicate " + key + " in " + a.path + " and " + match.path);
            }
        }
        return match == null ? null : match.args;
    }
    
    public String findUniqueKey(String key, String defaultValue) {
    	String result = findUniqueKey(key);
    	return result == null ? defaultValue : result;
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
                } else throw new RuntimeException("Duplicate " + key + " in " + a.path + " and " + match.path);
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
        Substitution sub = new Substitution();
        for (Annotation a : annotations) {
            if (a.key.equals("SUB"))
                sub.addVariable(a.path, a.before, a.args);
        }
        return sub;
    }

    public boolean isSample(String classname) {
        for (Annotation a : annotations) {
            if (a.key.equals("SAMPLE") && Util.javaClass(Util.tail(a.path)).equals(classname)) return true;
        }
        return false;
    }


    public Calls findCalls() {
        Calls calls = new Calls();
        for (Annotation a : annotations) {
            if (a.key.equals("CALL"))
                calls.addCall(a.path, a.args, a.next);
        }
        return calls;
    }
}
