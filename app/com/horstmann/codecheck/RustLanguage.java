package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class RustLanguage implements Language {
    public String getExtension() { return "rs"; };

    private static Pattern mainPattern = Pattern.compile("fn\\s+main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");
    @Override public Pattern mainPattern() { return mainPattern; }

    private static Pattern VARIABLE_DECL_PATTERN =Pattern.compile("let\\s*(mut\\s*)?(?<name>[A-Za-z][A-Za-z0-9]*)\\s*(:\\s*(((i|u)(8|16|32|64|128|size))|(f32|f64|bool|char)|(\\[(((i|u)(8|16|32|64|128|size))|(f32|f64|bool|char))\\s*;\\s*[0-9]*\\s*\\]))\\s*)?=\\s*(?<rhs>.+);\\s*");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }

    @Override
    public boolean isUnitTest(Path fileName) {
        return fileName.toString().matches(".*(T|_t)est[0-9]*.rs");
    }

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

    private static final Pattern successPattern = Pattern.compile("running (?<runs>[0-9]+) tests?[\\s\\S]*test result: ok. [0-9]+ passed; (?<failures>[0-9]+) failed;");
    @Override public Pattern unitTestSuccessPattern() { return successPattern; }
    private static final Pattern failurePattern = Pattern.compile("running (?<runs>[0-9]+) tests?[\\s\\S]*test result: FAILED. [0-9]+ passed; (?<failures>[0-9]+) failed;");
    @Override public Pattern unitTestFailurePattern() { return failurePattern; }  

    // TODO: Change regex to include error message
    private static Pattern ERROR_PATTERN = Pattern.compile("--> (?<file>[^\\s\\S]+\\.rs):(?<line>[0-9]+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }



 
}
