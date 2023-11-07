package com.horstmann.codecheck;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.ArrayList; 

public class PythonLanguage implements Language {

    public PythonLanguage() {
        // TODO Auto-generated constructor stub
    }

    @Override
    public String getExtension() {
        return "py";
    }
    
    //@Override public Interleave echoesStdin() { return Interleave.UNGRADED; }    
    @Override public Interleave echoesStdin() { return Interleave.ALWAYS; }
    
    private static Pattern mainPattern = Pattern.compile("def\\s+main\\s*\\(\\s*\\)\\s*:");
    //private static Pattern fundefPattern = Pattern.compile("def\\s+[A-Za-z0-9_]+\\s*\\(\\s*([A-Za-z0-9_]+(\\s*,\\s*[A-Za-z0-9_]+)*\\s*)?\\s*\\)\\s*:");

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isMain(java.nio.file.Path,
     * java.nio.file.Path)
     */
    @Override
    public boolean isMain(Path p, String contents) {
        if (!isSource(p))
            return false;
        if (mainPattern.matcher(contents).find()) return true;
        // Unindented statement indicates script
        for (String line : Util.lines(contents)) {
            if (line.length() != 0 && !Character.isWhitespace(line.charAt(0)) && !line.matches("^(#|(def|class|from|import|async)\\s|[A-Za-z0-9_]+\\s*=).*$"))
                return true;            
        }
        return false;
    }

    /*
     * TODO: Improve the printing
     * Strings should be quoted and \n (and therefore \\, \") escaped
     * Strings in lists should be double-quoted
     */
    
    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        String moduleName = moduleOf(file);
        List<String> lines = new ArrayList<>();
        int i = 0;
        lines.add(i++, "from sys import argv");
        lines.add(i++, "import " + moduleName);        
        lines.add(i++, "def main() :");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add(i++, 
                    "    if argv[1] == \"" + (k + 1) + "\" :");
            lines.add(i++, 
                    "        result = " + moduleName + "." + call.name + "("  + call.args + ")");
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
        return new String[] { "##", "" };
    }

    private static Pattern varPattern = Pattern.compile("(?<name>[A-Za-z][A-Za-z0-9]*)\\s*=\\s*(?<rhs>.+)");
    @Override public Pattern variableDeclPattern() { return varPattern; }

    private static Pattern errPattern = Pattern.compile("(?<msg>.*)[\"(](?<file>[A-Za-z0-9_]+\\.py)\"?, line (?<line>[0-9]+).*");    
    @Override public Pattern errorPattern() { return errPattern; }
    
    public boolean isUnitTest(Path fileName) { return fileName.toString().matches(".*(T|_t)est[0-9]*.py"); }
    
    private static final Pattern successPattern = Pattern.compile("Ran (?<runs>[0-9]+) tests? in [0-9.]+s\\s+OK");
    private static final Pattern failurePattern = Pattern.compile("Ran (?<runs>[0-9]+) tests? in [0-9.]+s\\s+FAILED \\([^=]+=(?<failures>[0-9]+)\\)");
    @Override public Pattern unitTestSuccessPattern() { return successPattern; }
    @Override public Pattern unitTestFailurePattern() { return failurePattern; }    
}
