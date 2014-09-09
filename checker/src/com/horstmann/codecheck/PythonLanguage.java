package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public class PythonLanguage implements Language {

    public PythonLanguage() {
        // TODO Auto-generated constructor stub
    }

    @Override
    public boolean isSource(Path p) {
        return p.toString().endsWith(".py");
    }

    @Override
    public boolean isTester(String modulename) {
        return modulename != null && modulename.matches("(.+_)?tester[0-9]*");
    }

    @Override
    public boolean isUnitTest(String modulename) {
        return false;
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
    public boolean isMain(Path dir, Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(dir, p);
        if (contents == null) return false;
        if (mainPattern.matcher(contents).find()) return true;
        if (fundefPattern.matcher(contents).find()) return false;
        return true;
    }

    @Override
    public String moduleOf(Path path) {
        String name = path.toString();
        if (!name.endsWith(".py"))
            return null;
        return name.substring(0, name.length() - 3); // drop .py
    }

    @Override
    public Path pathOf(String moduleName) {
        Path p = FileSystems.getDefault().getPath("", moduleName);
        Path parent = p.getParent();
        if (parent == null)
            return FileSystems.getDefault().getPath(moduleName + ".py");
        else
            return parent.resolve(p.getFileName().toString() + ".py");
    }

    @Override
    public boolean compile(String modulename, Path dir, Report report) {
        List<String> cmd = new ArrayList<>();
        cmd.add("python3");        
        cmd.add("-m");
        cmd.add("py_compile");
        cmd.add(dir.resolve(pathOf(modulename)).toString());
        String errorReport = Util.runProcess(cmd, null, Integer.MAX_VALUE).trim();
        if (errorReport.length() > 0) {
            report.error("Compiler error", errorReport);
            return false;
        } else
            return true;
    }

    @Override
    public String run(String mainclass, Path classpathDir, String args,
            String input, int timeoutMillis) throws IOException {
        List<String> cmd = new ArrayList<>();
        cmd.add("python3");
        cmd.add(classpathDir.resolve(pathOf(mainclass)).toString());
        if (args != null) cmd.addAll(Arrays.asList(args.split("\\s+")));
        
        return Util.runProcess(cmd, input, timeoutMillis);
    }

    @Override
    public void writeTester(Path sourceDir, Path targetDir, Path file,
            List<String> modifiers, String name, List<String> argsList)
            throws IOException {
        String moduleName = moduleOf(Util.tail(file));
        List<String> lines = Util.readLines(sourceDir.resolve(file));
        int i = 0;
        lines.add(i++, "from sys import argv");
        lines.add(i++, "import " + moduleName);        
        lines.add(i++, "def main() :");
        for (int k = 0; k < argsList.size(); k++) {
            lines.add(i++, 
                    "    if argv[1] == \"" + (k + 1) + "\" :");
            lines.add(i++, 
                    "        expected = "
                     + name + "(" + argsList.get(k)
                    + ")");
            lines.add(i++,
                    "        print(expected)");
            lines.add(i++, 
                    "        actual = "
                    + moduleName + "." + name + "("
                    + argsList.get(k) + ")");
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
        Files.write(targetDir.resolve(pathOf(moduleName + "CodeCheck")), lines,
                StandardCharsets.UTF_8);        
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

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#substitutionSeparator()
     */
    @Override
    public String substitutionSeparator() {
        return ";";
    }
    
    @Override
    public void runUnitTest(String moduleName, Path path, Report report, Score score) {             
    }
    
    @Override
    public boolean accept(Path file, Path dir, Set<Path> studentFiles,
            Report report, Score score) {
        return false;
    }
}
