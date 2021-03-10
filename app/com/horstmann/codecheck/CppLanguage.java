package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

public class CppLanguage implements Language {

    @Override
    public String getExtension() {
        return "cpp";
    }
    
    @Override public Interleave echoesStdin() { return Interleave.ALWAYS; }
    
    @Override
    public boolean isSource(Path p) {
        String name = p.toString();
        return name.endsWith(".cpp") || name.endsWith(".h");
    }
    
    private static Pattern mainPattern = Pattern.compile("\\s*((int|void)\\s+)?main\\s*\\([^)]*\\)\\s*(\\{\\s*)?");

    @Override 
    public Pattern mainPattern() { return mainPattern; }

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls) {
        
        // function in solution needs to be in separate namespace
        // function of student needs be externed;
        // imports on top
        // Look at line in solution file following //CALL
        // Remove any trailing {, add ;
        // add solution wrapped in namespace solution { ... }
        
        String moduleName = moduleOf(file);
        List<String> lines = Util.lines(contents);
        int i = 0;
        boolean done = false;
        while (!done) {
            if (i == lines.size()) done = true;
            else {
                String line = lines.get(i).trim();
                if (line.length() == 0 || line.startsWith("#include") || line.startsWith("using ") || line.startsWith("//")) i++;
                else done = true;
            }                
        }
        
        lines.add(i++, "#include \"codecheck.h\"");
        
        Set<String> externs = new LinkedHashSet<>();
        for (Calls.Call c : calls) 
           externs.add(c.modifiers.get(0) + " " + c.modifiers.get(1) + ";"); 
        lines.add(i++, "namespace solution {");
        for (String extern : externs) {        
            lines.add(i++, extern); // extern function from solution
        }
        lines.add(i++, "}");
        lines.add(i++, "int main(int argc, char *argv[]) {");
        // We declare the student functions locally in main so that they don't conflict with
        // solution functions
        for (String extern : externs) {        
            lines.add(i++, extern); // extern function from student            
        }
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add(i++, 
                    "    if (codecheck::eq(argv[1], \"" + (k + 1) + "\")) {");
            lines.add(i++,                    
                    "        codecheck::compare(solution::" + call.name + "(" + call.args + "), " + call.name + "(" + call.args + "));");
                // compare expected and actual
            lines.add(i++, "}");
        }
        lines.add(i++, "   return 0;");
        lines.add(i++, "}");
        lines.add(i++, "namespace solution {");
        lines.add("}");
        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), Util.join(lines, "\n"));
        paths.put(Paths.get("codecheck.cpp"), Util.readString(getClass().getResourceAsStream("codecheck.cpp")));
        paths.put(Paths.get("codecheck.h"), Util.readString(getClass().getResourceAsStream("codecheck.h")));
        return paths;
    }

    private static String patternString = ".*\\S\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)(\\s*[*\\[\\]]+)?\\s*=\\s*(?<rhs>[^;]+);.*";
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
