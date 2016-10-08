package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Pattern;

import com.horstmann.codecheck.Calls.Call;

public class CSharpLanguage implements Language {
    @Override
    public String getExtension() {
        return "cs";
    }
    
    private static String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*([^;]+);.*";
    private static Pattern pattern = Pattern.compile(patternString);
    
    @Override
    public Pattern variablePattern() {
        return pattern;
    }
    
    @Override
    public List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Call> calls) throws IOException {
        // TODO Auto-generated method stub
        return null;
    }

}
