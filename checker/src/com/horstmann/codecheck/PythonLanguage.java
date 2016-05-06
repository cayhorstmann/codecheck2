package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class PythonLanguage implements Language {

    public PythonLanguage() {
        // TODO Auto-generated constructor stub
    }

    @Override
    public boolean isSource(Path p) {
        return p.toString().endsWith(".py");
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
    public boolean isMain(Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(p);
        if (contents == null) return false;
        if (mainPattern.matcher(contents).find()) return true;
        if (fundefPattern.matcher(contents).find()) return false;
        return true;
    }

    private String moduleOf(Path path) {
        String name = path.toString();
        if (!name.endsWith(".py"))
            return null;
        return name.substring(0, name.length() - 3); // drop .py
    }

    private Path pathOf(String moduleName) {
        Path p = FileSystems.getDefault().getPath("", moduleName);
        Path parent = p.getParent();
        if (parent == null)
            return FileSystems.getDefault().getPath(moduleName + ".py");
        else
            return parent.resolve(p.getFileName().toString() + ".py");
    }

    @Override
    public List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Calls.Call> calls)
            throws IOException {
        String moduleName = moduleOf(Util.tail(file));
        List<String> lines = Util.readLines(sourceDir.resolve(file));
        int i = 0;
        lines.add(i++, "from sys import argv");
        lines.add(i++, "import " + moduleName);        
        lines.add(i++, "def main() :");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add(i++, 
                    "    if argv[1] == \"" + (k + 1) + "\" :");
            lines.add(i++, 
                    "        expected = "
                     + call.name + "(" + call.args
                    + ")");
            lines.add(i++,
                    "        print(expected)");
            lines.add(i++, 
                    "        actual = "
                    + moduleName + "." + call.name + "("
                    + call.args + ")");
            lines.add(i++, 
                    "        print(actual)");
            lines.add(
                    i++,
                    "        if expected == actual :");
            lines.add(i++, 
                    "            print(\"true\")");
            lines.add(
                    i++,
                    "        else :");
            lines.add(i++, 
                    "            print(\"false\")");
        }
        lines.add("main()");
        Path p = pathOf(moduleName + "CodeCheck");
        Files.write(targetDir.resolve(p), lines, StandardCharsets.UTF_8);        
        List<Path> testModules = new ArrayList<>();
        testModules.add(p);
        return testModules;        
    }

    @Override
    public String[] pseudoCommentDelimiters() {
        return new String[] { "##", "" };
    }

    private static String patternString = "\\s*([A-Za-z][A-Za-z0-9]*)\\s*=\\s*(.+)";
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
}
