package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class DartLanguage implements Language {

    @Override
    public String getExtension() { return "dart"; }
    
    private static Pattern MAIN_PATTERN = Pattern.compile("\\s*((int|void)\\s+)?main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");
    @Override public Pattern mainPattern() { return MAIN_PATTERN; }

    private static Pattern VARIABLE_PATTERN = Pattern.compile(".*\\S\\s+(?<name>[A-Za-z][A-Za-z0-9]*)(\\s*[*\\[\\]]+)?\\s*=\\s*(?<rhs>[^;]+);.*");
    @Override public Pattern variableDeclPattern() { return VARIABLE_PATTERN; }
       
    private static Pattern ERROR_PATTERN = Pattern.compile(".+/(?<file>[^/]+\\.cpp):(?<line>[0-9]+):(?<col>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }    

    // TODO: Implement this
    @Override
    public Map<Path, String> writeTester(Path file, String contents,
                                         List<Calls.Call> calls,
                                         ResourceLoader resourceLoader) {

        String moduleName = moduleOf(file);
        String classname = moduleOf(file);
        //List<String> lines = Util.readLines(sourceDir.resolve(file));

        List<String> lines = Util.lines(contents);
        int i = 0;
        lines.add(i++, "import '" + moduleName + ".dart';");
        lines.add(i++, "main() {");
        for (int k = 0; k < calls.size(); k++) {

            Calls.Call call = calls.get(k);
            lines.add(i++, "     var  expected = "
                    + call.name + "(" + call.args
                    + ");");
            lines.add(i++, "      print(expected);");
            lines.add(i++, "   var   actual = "
                    + moduleName + "()." + call.name + "("
// + call.name + "("

                    + call.args + ");");
            lines.add(i++, "      print(actual);");
            lines.add(i++, "      if (expected == actual) ");
            lines.add(i++, "        print(\"true\"); ");
            lines.add(i++, "      else ");
            lines.add(i++, "        print(\"false\"); ");
        }
        lines.add(i++, "}");
        //lines.add("main();");

        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), "");

        Path p = pathOf(classname + "CodeCheck");
        Map<Path, String> testFiles = new HashMap<>();
        testFiles.put(p, Util.join(lines, "\n"));
        return testFiles;
    }
}
