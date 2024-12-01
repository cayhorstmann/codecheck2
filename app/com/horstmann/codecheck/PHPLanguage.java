package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.horstmann.codecheck.Calls.Call;

public class PHPLanguage implements Language {
    @Override
    public String getExtension() {
        return "php"; 
    }
    
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Call> calls, ResourceLoader resourceLoader) throws IOException {        
        String moduleName = moduleOf(file);
        Set<String> functionNames = new TreeSet<>();
        for (Calls.Call call : calls) functionNames.add(call.name);
        List<String> lines = new ArrayList<>();
        lines.addAll(Util.lines(contents));
        for (int i = lines.size() - 1; i >= 0; i--) {
        	if (lines.get(i).strip().equals("?>")) {
        		lines.set(i, "");
        		i = 0;
        	}
        }
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add("if ($argv[1] === '" + (k + 1) + "') {");
            lines.add("$result = " + call.name + "(" + call.args + ");");
            lines.add("var_export($result);"); // TODO Or var_dump, print_r
            lines.add("}");            
        }
        lines.add("?>");
        Map<Path, String> paths = new LinkedHashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), Util.join(lines, "\n"));
        return paths;
    }

    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	"^\\s*(?<name>\\$[A-Za-z][A-Za-z0-9_]*)\\s*=(?<rhs>[^;]+);");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
    
    public boolean isUnitTest(Path fileName) { return fileName.toString().matches(".*(T|_t)est[0-9]*.php"); }
    
    private static final Pattern successPattern = Pattern.compile("OK \\([0-9]+ tests?, (?<runs>[0-9.]+) assertions\\)");
    private static final Pattern failurePattern = Pattern.compile("Tests: [0-9]+, Assertions: (?<runs>[0-9]+), Failures: (?<failures>[0-9]+)\\.");
    @Override public Pattern unitTestSuccessPattern() { return successPattern; }
    @Override public Pattern unitTestFailurePattern() { return failurePattern; }    
    
}
