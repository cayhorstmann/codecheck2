package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import com.horstmann.codecheck.Calls.Call;

public class CSharpLanguage implements Language {
    @Override
    public String getExtension() {
        return "cs";
    }
    
    private static String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);.*";
    private static Pattern pattern = Pattern.compile(patternString);
    
    @Override
    public Pattern variablePattern() {
        return pattern;
    }
    
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Call> calls) {
        // TODO Auto-generated method stub
        return null;
    }
}