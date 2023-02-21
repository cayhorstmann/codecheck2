package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class MatlabLanguage implements Language {

    public MatlabLanguage() {
        // TODO Auto-generated constructor stub
    }
    
    @Override
    public String getExtension() {
        return "m";
    }

    private static Pattern mainPattern = Pattern.compile("def\\s+main\\s*\\(\\s*\\)\\s*:");
    private static Pattern fundefPattern = Pattern.compile("def\\s+[A-Za-z0-9_]+\\s*\\(\\s*([A-Za-z0-9_]+(\\s*,\\s*[A-Za-z0-9_]+)*\\s*)?\\s*\\)\\s*:");

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isMain(java.nio.file.Path,
     * java.nio.file.Path)
     */
    @Override
    public boolean isMain(Path p, String contents) {
        if (mainPattern.matcher(contents).find()) return true;
        if (fundefPattern.matcher(contents).find()) return false;
        return true;
    }

    // TODO: Implement correctly
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        String moduleName = moduleOf(file);
        List<String> lines = Util.lines(contents);
        int i = 0;
        lines.add(i++, "from sys import argv");
        lines.add(i++, "import " + moduleName);        
        lines.add(i++, "def main() :");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add(i++, 
                    "    if argv[1] == \"" + (k + 1) + "\" :");
            lines.add(i++, 
                    "        result = " + call.name + "(" + call.args + ")");
            lines.add(i++,
                    "        print(result)");
        }
        lines.add("main()");
        Path p = pathOf(moduleName + "CodeCheck");
        Map<Path, String> testFiles = new HashMap<>();
        testFiles.put(p, Util.join(lines, "\n"));
        return testFiles;        
    }

    @Override
    public String[] pseudoCommentDelimiters() {
        return new String[] { "%%", "" };
    }

    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	"(?<name>[A-Za-z][A-Za-z0-9]*)\\s*=\\s*(?<rhs>.+)");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
}
