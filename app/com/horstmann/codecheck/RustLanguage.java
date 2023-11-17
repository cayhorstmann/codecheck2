package com.horstmann.codecheck;

import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.ArrayList;

public class RustLanguage implements Language {
    public String getExtension() { return "rs"; };
    //Implement main pattern and the rest
    //after that do preload.sh

    private static Pattern mainPattern = Pattern.compile("fn\\s+main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");
    @Override public Pattern mainPattern() { return mainPattern; }

    private static Pattern VARIABLE_DECL_PATTERN =Pattern.compile("let\\s+(mut)?(?<name>[A-Za-z][A-Za-z0-9]*)\\s*=\\s*(?<rhs>.+)");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        String moduleName = moduleOf(file);
        List<String> lines = new ArrayList<>();
        lines.add("use std::env;");
        lines.add("mod " + moduleName + ";");
        lines.add("fn main() {");
        lines.add("    let args: Vec<String> = env::args().collect();");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add("    if args[1] == \"" + (k + 1) + "\" {");
            lines.add("        let result = " + moduleName + "::" + call.name + "(" + call.args + ");");
            lines.add("        println!(\"{:?}\", result);");
            lines.add("    }");
        }
        lines.add("}");
        Path p = pathOf(moduleName + "CodeCheck");
        Map<Path, String> testFiles = new HashMap<>();
        testFiles.put(p, Util.join(lines, "\n"));
        return testFiles;
    }

    private static Pattern ERROR_PATTERN = Pattern.compile("(.+/)?(?<file>[^/]+\\.rs):(?<line>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }



 
}
