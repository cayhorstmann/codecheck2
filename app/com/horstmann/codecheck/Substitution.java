package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Substitution {
    private Path file;
    private Map<String, ArrayList<String>> subs = new LinkedHashMap<>();
    private int size;
    private Language language;
    
    public Substitution(Language language) {
        this.language = language;
    }
    
    public void addVariable(Path file, String decl, String args) {
        if (this.file == null)
            this.file = file;
        else if (!this.file.equals(file))
            throw new CodeCheckException("SUB in " + this.file + " and " + file);
        Pattern pattern = language.variableDeclPattern();
        Matcher matcher = pattern.matcher(decl.trim());
        if (matcher.matches()) {
            String name = matcher.group("name").trim();
            ArrayList<String> values = new ArrayList<>();
            if (subs.containsKey(name))
                throw new CodeCheckException("More than one SUB in " + file + " for " + name);
            subs.put(name, values);
            values.add(matcher.group("rhs")); 
            for (String v : language.substitutionSeparator().split(args))
                if (v.trim().length() > 0)
                    values.add(v);
            if (size == 0)
                size = values.size();
            else if (values.size() != size)
                throw new CodeCheckException("SUB in " + file + " have unequal length");
        }
    }

    public int getSize() {
        return size;
    }

    public Path getFile() {
        return file;
    }

    public List<String> names() {
        List<String> r = new ArrayList<>();
        for (String n : subs.keySet()) r.add(n);
        return r;
    }

    public List<String> values(int i) {
        List<String> r = new ArrayList<>();
        for (String n : subs.keySet()) r.add(subs.get(n).get(i));
        return r;
    }
    
    public static String removeComment(String line, String start, String end) {
        int i = line.indexOf(start);
    	if (i < 0) return line;
        if (end.isBlank()) return line.substring(0, i);
       	int j = line.lastIndexOf(end);
       	if (j < 0) return line;
        return line.substring(0, i) + line.substring(j + end.length());
    }
    
    public String substitute(String contents, int n) throws IOException { 
        Pattern pattern = language.variableDeclPattern();
        List<String> lines = Util.lines(contents);
        StringBuilder out = new StringBuilder();
        String[] delims = language.pseudoCommentDelimiters();
        String start = delims[0];
        String end = delims[1];
        Set<String> alreadyUsed = new HashSet<String>();
        for (String l : lines) {
        	String line = removeComment(l, start, end);
        	int i = 0;
        	while (i < line.length() && Character.isWhitespace(line.charAt(i))) i++;
        	int j = line.length() - 1;
        	while (j >= i && Character.isWhitespace(line.charAt(j))) j--;
            Matcher matcher = pattern.matcher(line.substring(i, j + 1));
            if (matcher.matches()) {
                String name = matcher.group("name");
                if (subs.containsKey(name) && !alreadyUsed.contains(name)) {
                	alreadyUsed.add(name);
                	out.append(line.substring(0, i + matcher.start("rhs")));
                    out.append(subs.get(name).get(n));
                    out.append(line.substring(i + matcher.end("rhs")));
                    out.append("\n");
                } else {
                    out.append(line);
                    out.append("\n");
                }
            } else {
                out.append(line);
                out.append("\n");
            }
        }
        return out.toString();
    }
}
