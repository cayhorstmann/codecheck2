package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BashLanguage implements Language {

    @Override
    public String getExtension() {
        return "sh";
    }
    
    @Override
    public boolean isMain(Path filename, String contents) {
    	return moduleOf(filename).matches("main|tester[0-9]*"); 
    }
    
    @Override
    public String[] pseudoCommentDelimiters() {
        return new String[] { "##", "" };
    }
    
    private static Pattern VARIABLE_PATTERN = Pattern.compile("^\\s*(?<name>[A-Za-z][A-Za-z0-9_]*)=(?<rhs>.*)$");
    @Override public Pattern variableDeclPattern() { return VARIABLE_PATTERN; }
       
    private static Pattern FUNCTION_PATTERN = Pattern.compile("^\\s*function\\s+(?<name>[A-Za-z][A-Za-z0-9_]*).*");
    @Override
    public String functionName(String declaration) { 
    	Matcher matcher = FUNCTION_PATTERN.matcher(declaration);
    	if (matcher.matches()) return matcher.group("name");
    	else return null;
    }
    
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        
        String moduleName = moduleOf(file);
        String result = contents + "\n";
        result += "case $1 in\n";
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            result += "  " + (k + 1) + ")\n    " + call.name + " " + call.args + "\n    ;;\n";
        }
        result += "esac\n";
        
        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), result);
        return paths;
    }
}
