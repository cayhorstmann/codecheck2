package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.ArrayList;
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
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) throws IOException {
        
        // imports on top
        // Look at line in solution file following //CALL
        // Remove any trailing {, add ;
        // add solution wrapped in namespace solution { ... }
        
        String moduleName = moduleOf(file);
        List<String> lines = new ArrayList<>();
        contents
           .lines()
           .filter(l -> l.trim().startsWith("#include "))
           .forEach(l -> lines.add(l));
        
        lines.add("#include \"codecheck.h\"");
        lines.add("#include <cstdlib>");
        Set<String> externs = new LinkedHashSet<>();
        for (Calls.Call c : calls) 
           externs.add(c.modifiers.get(0) + " " + c.modifiers.get(1) + ";"); 
        for (String extern : externs) {        
            lines.add(extern); // extern function from student            
        }
        lines.add("int main(int argc, char *argv[]) {");
        lines.add("    int arg = std::atoi(argv[1]);");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add("    if (arg == " + (k + 1) + ") {");
            lines.add("        codecheck::print(" + call.name + "(" + call.args + "));");
            lines.add("}");
        }
        lines.add("   return 0;");
        lines.add("}");
        Map<Path, String> paths = new HashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), Util.join(lines, "\n"));
        // TODO: Include these in the same file
        paths.put(Paths.get("codecheck.cpp"), resourceLoader.loadResourceAsString("codecheck.cpp"));
        paths.put(Paths.get("codecheck.h"), resourceLoader.loadResourceAsString("codecheck.h"));
        return paths;
    }

    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
    	".*\\S\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)(\\s*[*\\[\\]]+)?\\s*=\\s*(?<rhs>[^;]+);");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
    
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
