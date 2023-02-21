package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
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
            // https://nodejs.org/api/modules.html
            if (line.matches("^\\s*(exports\\.|module\\.exports\\s*=).*$")) return false;
        return true;
    }
        
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Call> calls, ResourceLoader resourceLoader) throws IOException {        
        String moduleName = moduleOf(file);
        Set<String> functionNames = new TreeSet<>();
        for (Calls.Call call : calls) functionNames.add(call.name);
        List<String> lines = new ArrayList<>();
        lines.addAll(Util.lines(contents));
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add("if (process.argv[process.argv.length - 1] === '" + (k + 1) + "') {");
            lines.add("const result = " + call.name + "(" + call.args + ")");
            lines.add("console.log(JSON.stringify(result))");
            lines.add("}");            
        }
        Map<Path, String> paths = new LinkedHashMap<>();
        paths.put(pathOf(moduleName + "CodeCheck"), Util.join(lines, "\n"));
        return paths;
    }

    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	"(var|const|let)\\s+(?<name>[A-Za-z][A-Za-z0-9]*)\\s*=(?<rhs>[^;]+);?");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
}
