package com.horstmann.codecheck;

import java.util.regex.Pattern;

public class CSharpLanguage implements Language {
    @Override
    public String getExtension() {
        return "cs";
    }
    
    private static String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);.*";
    private static Pattern pattern = Pattern.compile(patternString);
    
    @Override
    public Pattern variableDeclPattern() {
        return pattern;
    }
}