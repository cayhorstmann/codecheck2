package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class CLanguage implements Language {

    @Override
    public String getExtension() {
        return "c";
    }
    
    @Override
    public boolean isSource(Path p) {
        String name = p.toString();
        return name.endsWith(".c") || name.endsWith(".h") || name.endsWith(".s") || name.endsWith(".S");
    }
    
    private static Pattern mainPattern = Pattern.compile("\\s*((int|void)\\s+)?main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");

    @Override 
    public Pattern mainPattern() { return mainPattern; }

    // TODO: Implement this
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls) {
        
        String moduleName = moduleOf(file);
        //List<String> lines = Util.readLines(sourceDir.resolve(file));
        
        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), "");
        return paths;
    }

    private static String patternString = ".*\\S\\s+(?<name>[A-Za-z][A-Za-z0-9]*)(\\s*[*\\[\\]]+)?\\s*=\\s*(?<rhs>[^;]+);.*";
    private static Pattern pattern = Pattern.compile(patternString);

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#variablePattern()
     */
    @Override
    public Pattern variablePattern() {
        return pattern;
    }
    
    @Override
    public List<String> modifiers(String declaration) {
        // We save the type and the rest of the declaration 
        int n = declaration.indexOf("{");
        if (n >= 0) declaration = declaration.substring(0, n);
        n = declaration.indexOf("(") - 1;
        while (n >= 0 && Character.isWhitespace(declaration.charAt(n))) n--;
        while (n >= 0 && !Character.isWhitespace(declaration.charAt(n))) n--;        
        
        return Arrays.asList(declaration.substring(0, n + 1).trim(), declaration.substring(n + 1));
    }
    
    private static Pattern ERROR_PATTERN = Pattern.compile(".+/(?<file>[^/]+\\.cpp):(?<line>[0-9]+):(?<col>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }    
}
