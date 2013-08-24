package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Substitution {
    private Path file;
    private Map<String, ArrayList<String>> subs = new LinkedHashMap<>();
    private int size;

    public void addVariable(Path file, String decl, String args) {
        if (this.file == null)
            this.file = file;
        else if (!this.file.equals(file))
            throw new RuntimeException("SUB in " + this.file + " and " + file);
        String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*([^;]+);\\s*";
        Pattern pattern = Pattern.compile(patternString);
        Matcher matcher = pattern.matcher(decl);
        if (matcher.matches()) {
            String name = matcher.group(1).trim();
            ArrayList<String> values = new ArrayList<>();
            subs.put(name, values);
            values.add(matcher.group(2));
            for (String v : args.split(";"))
                if (v.trim().length() > 0)
                    values.add(v);
            if (size == 0)
                size = values.size();
            else if (values.size() != size)
                throw new RuntimeException("SUB in " + file + " have unequal length");
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

    void substitute(Path from, Path to, int n) throws IOException {
        String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*([^;]+);.*";
        Pattern pattern = Pattern.compile(patternString);
        List<String> lines = Util.readLines(from);
        try (PrintWriter out = new PrintWriter(Files.newBufferedWriter(to, StandardCharsets.UTF_8))) {
            for (String line : lines) {
                Matcher matcher = pattern.matcher(line);
                if (matcher.matches()) {
                    String name = matcher.group(1);
                    if (subs.containsKey(name)) {
                        out.print(line.substring(0, matcher.start(2)));
                        out.print(subs.get(name).get(n));
                        out.println(line.substring(matcher.end(2)));
                    } else out.println(line);
                } else out.println(line);
            }
        }
    }

}
