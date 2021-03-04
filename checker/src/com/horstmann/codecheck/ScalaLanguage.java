package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScalaLanguage implements Language {
    
    @Override
    public String getExtension() {
        return "scala";
    }
    
    private static Pattern mainPattern1 = Pattern
            .compile("def\\s+main\\s*\\(\\s*\\S+\\s*:\\s*Array\\[\\s*String\\s*\\]\\s*\\)");
    private static Pattern mainPattern2 = Pattern
            .compile("object\\s+\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*\\s+extends\\s+App\\s");
    

    @Override
    public boolean isMain(Path p) {
        if (!isSource(p)) return false;
        String contents = Util.read(p);
        return contents != null &&
            (mainPattern1.matcher(contents).find() || mainPattern2.matcher(contents).find());
    }

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls) {
        List<String> lines = Util.lines(contents);
        
        // Find class name
        int i = 0;
        while (i < lines.size() && !lines.get(i).contains("//CALL")) i++;
        if (i == lines.size())
            throw new CodeCheckException("Can't find object in " + file);
        while (i >= 0 && !lines.get(i).trim().startsWith("object")) i--;
        if (i < 0)
            throw new CodeCheckException("Can't find object in " + file);
        String[] tokens = lines.get(i).split("\\P{javaJavaIdentifierPart}+");
        int j = 0;
        if (tokens[0].length() == 0) j++;
        if (j + 1 >= tokens.length) throw new CodeCheckException("Can't find object name in " + file);
        String objectName = tokens[j + 1];
        lines.add(0, "object " + objectName + "CodeCheck extends App {");
        lines.add(1, "object Solution {");
        lines.add("}}");
        i = 0;
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            String submissionFun = objectName + "." + call.name;
            String solutionFun = "Solution." + submissionFun;
            lines.add(++i, "if (args(0) == \"" + (k + 1) + "\") {");
            lines.add(++i, "val actual = " + submissionFun + "(" + call.args + ")");
            lines.add(++i, "val expected = " + solutionFun + "(" + call.args + ")");
            lines.add(++i, "println(runtime.ScalaRunTime.stringOf(expected))");
            lines.add(++i, "println(runtime.ScalaRunTime.stringOf(actual))");
            lines.add(++i, "println(actual == expected) }");
         }
        Path p = Paths.get(objectName + "CodeCheck.scala");
        Map<Path, String> testModules = new HashMap<>();
        testModules.put(p, Util.join(lines, "\n"));
        return testModules;        
    }
    
    private static Pattern functionPattern = Pattern.compile(
            "\\s*(def|val)\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*([\\[(=]|$).*");
    
    @Override public String functionName(String declaration) {
        Matcher matcher = functionPattern.matcher(declaration);
        if (matcher.matches()) return matcher.group("name");
        else return null;
    }
        
    private static Pattern variablePattern = Pattern.compile(
            "(val|var)\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)(\\s*:[^=]+\\s*)?\\s*=\\s*(?<rhs>[^;]+);.*");

    @Override
    public Pattern variablePattern() { return variablePattern; }
}
