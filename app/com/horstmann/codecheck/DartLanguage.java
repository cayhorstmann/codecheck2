package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.ArrayList;


public class DartLanguage implements Language {

    @Override
    public String getExtension() { return "dart"; }
    
    private static Pattern MAIN_PATTERN = Pattern.compile("\\s*((int|void)\\s+)?main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");
    @Override public Pattern mainPattern() { return MAIN_PATTERN; }

    private static Pattern VARIABLE_DECL_PATTERN 
    	= Pattern.compile(".*\\S\\s+(?<name>[A-Za-z][A-Za-z0-9]*)(\\s*[*\\[\\]]+)?\\s*=\\s*(?<rhs>[^;]+);");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
       
    private static Pattern ERROR_PATTERN = Pattern.compile(".+/(?<file>[^/]+\\.cpp):(?<line>[0-9]+):(?<col>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }    

    // TODO Add test case to samples
    @Override
    public Map<Path, String> writeTester(Path file, String contents,
                                         List<Calls.Call> calls,
                                         ResourceLoader resourceLoader) {
        String moduleName = moduleOf(file);
        List<String> lines = new ArrayList<>(); 
        lines.add("import '" + moduleName + ".dart';");
        lines.add("main() {");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add("   var result = " + moduleName + "()." + call.name + "("
                     + call.args + ");");
             lines.add("      print(result);");
        }
        lines.add("}");

        Path p = pathOf(moduleName + "CodeCheck");
        Map<Path, String> testFiles = new HashMap<>();
        testFiles.put(p, Util.join(lines, "\n"));
        return testFiles;
    }
}
