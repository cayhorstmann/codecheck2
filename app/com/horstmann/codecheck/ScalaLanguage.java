package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.ArrayList;
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
    public boolean isMain(Path p, String contents) {
        return mainPattern1.matcher(contents).find() || mainPattern2.matcher(contents).find();
    }

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        // Requirement: File name must equal to object containing called method
        String objectName = moduleOf(file);
        List<String> lines = new ArrayList<>(); 
        lines.add("object " + objectName + "CodeCheck extends App {");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            String submissionFun = objectName + "." + call.name;
            lines.add("if (args(0) == \"" + (k + 1) + "\") {");
            lines.add("  val result = " + submissionFun + "(" + call.args + ")");
            lines.add("  println(runtime.ScalaRunTime.stringOf(result))");
            lines.add("}");
        }
        lines.add("}");
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
        
    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
    		"(val|var)\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)(\\s*:[^=]+\\s*)?\\s*=\\s*(?<rhs>[^;]+);");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
}
