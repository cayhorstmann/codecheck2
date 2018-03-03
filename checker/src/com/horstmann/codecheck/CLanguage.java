package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

public class CLanguage implements Language {

    @Override
    public String getExtension() {
        return "c";
    }
    
    @Override
    public boolean isSource(Path p) {
        String name = p.toString();
        return name.endsWith(".c") || name.endsWith(".h");
    }
    
    private static Pattern mainPattern = Pattern.compile("\\s*((int|void)\\s+)?main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isMain(java.nio.file.Path,
     * java.nio.file.Path)
     */
    @Override
    public boolean isMain(Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(p);
        if (contents == null) return false;
        return mainPattern.matcher(contents).find();
    }

    // TODO: Implement this
    @Override
    public List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Calls.Call> calls)
            throws IOException {
        
        String moduleName = moduleOf(Util.tail(file));
        //List<String> lines = Util.readLines(sourceDir.resolve(file));
        
        List<Path> paths = new ArrayList<>();
        paths.add(pathOf(moduleName + "CodeCheck"));
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
