package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.horstmann.codecheck.Calls.Call;

public class JavaScriptLanguage implements Language {
    @Override
    public String getExtension() {
        return "js"; // TODO: Change to mjs when we support ECMAScript modules
    }
    
    @Override public boolean isSource(Path p) {
        String pathString = p.toString();
        return pathString.endsWith(".js") || pathString.endsWith(".mjs");
    }
    
    @Override public boolean isLanguage(Collection<Path> files) {
        for (Path p : files)
            if (isSource(p)) return true;
        return false;                    
    }    
    
    @Override public boolean isMain(Path p, String contents) {
        if (!isSource(p))
            return false;
        for (String line : Util.lines(contents)) 
            if (line.matches("^\\s*export\\s+.*$")) return false;
        return true;
    }
        
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Call> calls) {        
        String moduleName = moduleOf(file);
        Set<String> functionNames = new TreeSet<>();
        for (Calls.Call call : calls) functionNames.add(call.name);
        List<String> lines = new ArrayList<>();
        lines.add("const codecheck = require('./codecheck.js')"); // TODO: Update to ECMAScript module
        lines.add("const studentFunctions = function(){");
        lines.addAll(Util.lines(contents));
        lines.add("return {");
        for (String functionName : functionNames)
            lines.add(functionName + ", ");
        lines.add("}}()");
        lines.add("const solutionFunctions = function(){");
        lines.addAll(Util.lines(contents));
        lines.add("return {");
        for (String functionName : functionNames)
            lines.add(functionName + ", ");
        lines.add("}}()");
        
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add("if (process.argv[process.argv.length - 1] === '" + (k + 1) + "') {");
            lines.add("const actual = studentFunctions." + call.name + "(" + call.args + ")");
            lines.add("const expected = solutionFunctions." + call.name + "(" + call.args + ")");
            lines.add("console.log(JSON.stringify(expected))");
            lines.add("console.log(JSON.stringify(actual))");
            lines.add("console.log(codecheck.deepEquals(actual, expected))");
            lines.add("}");            
        }
        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), Util.join(lines, "\n"));
        paths.put(Paths.get("codecheck.js"), Util.readString(getClass().getResourceAsStream("codecheck.js"))); // TODO mjs when we support ECMAScript modules
        return paths;
    }

    private static String patternString = "(var|const|let)\\s+(?<name>[A-Za-z][A-Za-z0-9]*)\\s*=(?<rhs>[^;]+);?";
    private static Pattern pattern = Pattern.compile(patternString);

    @Override
    public Pattern variablePattern() {
        return pattern;
    }    
}
