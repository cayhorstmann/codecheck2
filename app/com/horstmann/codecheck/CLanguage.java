package com.horstmann.codecheck;

import java.nio.file.Path;
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
    
    private static Pattern MAIN_PATTERN = Pattern.compile("\\s*((int|void)\\s+)?main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");
    @Override public Pattern mainPattern() { return MAIN_PATTERN; }

    private static Pattern VARIABLE_PATTERN = Pattern.compile(".*\\S\\s+(?<name>[A-Za-z][A-Za-z0-9_]*)(\\s*[*\\[\\]]+)?\\s*=\\s*(?<rhs>[^;]+);.*");
    @Override public Pattern variableDeclPattern() { return VARIABLE_PATTERN; }
       
    private static Pattern ERROR_PATTERN = Pattern.compile(".+/(?<file>[^/]+\\.cpp):(?<line>[0-9]+):(?<col>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }    

    // TODO: Implement this
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        
        String moduleName = moduleOf(file);
        //List<String> lines = Util.readLines(sourceDir.resolve(file));
        
        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), "");
        return paths;
    }
}
