package com.horstmann.codecheck;

import java.util.regex.Pattern;

public class CSharpLanguage implements Language {
    @Override
    public String getExtension() {
        return "cs";
    }

    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
}