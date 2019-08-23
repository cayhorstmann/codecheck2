package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public interface Language {

    /**
     * Gets the extension required for source files in this language. 
     * @return the extension (without a period), or null if there 
     * is no one such extension.
     */
    String getExtension(); 
    
    /**
     * Tests if a file is a source file in this language.
     * @param p the path to the file
     * @return true if it is a source file
     */
    default boolean isSource(Path p) {
        String extension = getExtension();
        return extension != null && p.toString().endsWith("." + extension);
    }

    /**
     * Derives a "module" name from a source file. For example, in Java, the module of
     * foo/bar/Baz.java would be foo.bar.Baz. By default, the source extension is removed.
     * @param path the path of a source file
     * @return the module name, or null if this file doesn't represent a module 
     */
    default String moduleOf(Path path) {
        String name = path.toString();
        String extension = getExtension();        
        if (extension == null || !name.endsWith("." + extension))
            return null;
        return name.substring(0, name.length() - extension.length() - 1); // drop .extension
    }
    
    /**
     * Turns a "module" name into a source file. For example, in Java, the module foo.bar.Baz
     * is turned into foo/bar/Baz.java. By default, the extension is added.
     * @param moduleName a module name
     * @return a path to a file representing this module
     */
    default Path pathOf(String moduleName) {
        String extension = getExtension();
        if (extension == null) extension = "";
        else extension = "." + extension;
        Path p = FileSystems.getDefault().getPath("", moduleName);
        Path parent = p.getParent();
        if (parent == null)
            return FileSystems.getDefault().getPath(moduleName + extension);
        else
            return parent.resolve(p.getFileName().toString() + extension);
    }

    
    /**
     * Tests if this collection of files contains source files
     * of this language. For example, for C++, there would have to be 
     * at least one .cpp file. A .h file is source, but not an indication for C++
     * The default implementation, which works for Java etc., just checks 
     * that there is at least one file with the source extension
     * @param files
     * @return
     */
    default boolean isLanguage(Collection<Path> files) {
        String extension = getExtension();
        // Don't call isSource because that may have been overridden to classify 
        // header files as source
        for (Path p : files)
            if (extension != null && p.toString().endsWith("." + extension)) return true;
        return false;                    
    }

    /**
     * Tests if a file is an "Expected" style unit test file in this language.
     * @param p the path to the file
     * @return true if it is an "Expected" style unit test file
     */
    default boolean isTester(Path fileName) { 
        return fileName != null && moduleOf(fileName).matches(".*Tester[0-9]*");
    }


    /**
     * Tests if a file is an "XUnit" style unit test file in this language.
     * @param p the path to the file
     * @return true if it is an "XUnit" style unit test file
     */
    default boolean isUnitTest(Path fileName) { return false; }

    /**
     * Tests if a file is a "main" file, i.e. an entry point for execution. By default,
     * a file whose module name ends in Runner or Tester, optionally followed by a number, matches.
     * Many languages instead check whether the file contains something such as "public 
     * static void main"
     * @param fileName the path to the file
     * @return true if it is a "main" file
     */
    default boolean isMain(Path fileName) { 
        return moduleOf(fileName).matches(".*(Runn|Test)er[0-9]*"); 
    }

    // TODO: Why wire in the report? Maybe needs to return a compilation result with success, messages, name of executable?
    /**
     * Compiles a program
     * @param sourceFiles the source files that need to be compiled. The first one
     * is the "main" one. 
     * @param dir the directory containing the files
     * 
     * @return null for no error, or an error report if there was an error.
     */
    default String compile(List<Path> sourceFiles, Path dir) {
        List<String> cmd = new ArrayList<>();
        if (System.getProperty("os.name").toLowerCase().contains("win")) // We lose
            cmd.add(Util.getHomeDir() + "\\comprog.bat");
        else
            cmd.add(Util.getHomeDir() + "/comprog");
        cmd.add(getLanguage());
        for (Path p : sourceFiles)
            cmd.add(dir.resolve(p).toString());
        StringBuilder output = new StringBuilder();
        final int MAX_OUTPUT_LEN = 10_000;
        int exitValue = Util.runProcess(cmd, null, Integer.MAX_VALUE, output, MAX_OUTPUT_LEN);
        if (exitValue != 0) {
            return output.toString();
        } else
            return null;
    }

    /**
     * Runs a program
     * @param sourceFile the path of the main file (from which the
     * program name needs to be derived, in concert with the compile method) 
     * @param dir the directory containing the main file
     * @param args the command-line arguments
     * @param input the input to pass to stdin  
     * @return the combined stdout/stderr of the run 
     */
    default String run(Path sourceFile, Set<Path> dependentFiles, Path dir, String args,
            String input, int timeoutMillis, int maxOutputLen, boolean interleaveIO) throws Exception {
        List<String> cmd = new ArrayList<>();
        if (System.getProperty("os.name").toLowerCase().contains("win")) // We lose
            cmd.add(Util.getHomeDir() + "\\runprog.bat");
        else
            cmd.add(Util.getHomeDir() + "/runprog");
        int timeoutSeconds = Math.max(1, (timeoutMillis + 500) / 1000);
        cmd.add("" + timeoutSeconds);
        cmd.add("" + interleaveIO);
        cmd.add(getLanguage());        
        cmd.addAll(runCommand(dir, sourceFile, dependentFiles, args));
        
        if (args != null) cmd.addAll(Arrays.asList(args.split("\\s+")));
        StringBuilder output = new StringBuilder();        
        Util.runProcess(cmd, input, timeoutMillis, output, maxOutputLen);
        return output.toString();
    }

    default List<String> runCommand(Path dir, Path sourceFile, Set<Path> dependentFiles, String args)
    {
        List<String> cmd = new ArrayList<>();
        String programName = dir.resolve(sourceFile).toString();
        cmd.add(programName);        
        return cmd;
    }
    
    /**
     * Gets the language string for this language.
     * @return the language string (which may be passed on to the comprog/runprog scripts)
     */
    default String getLanguage() {
        String lang = getClass().getName();
        lang = lang.substring(lang.lastIndexOf(".") + 1).replace("Language", "");
        return lang;
    }
    

    /**
     * Writes a CALL tester.
     * @param sourceDir the directory of the source of the files
     * @param targetDir the target directory to write the tester to
     * @param file the template file
     * @param modifiers any modifiers for the test generation
     * @param name the name of the method being tested
     * @param argsList the args to pass to the calls
     * @return a list containing the Path to the tester (with the main method or its equivalent),
     * relative to the target directory, followed by the paths to any helper files
     * @throws IOException
     */
    List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Calls.Call> calls)
            throws IOException;

    default String[] pseudoCommentDelimiters() { return new String[] { "//", "" }; }

    /**
     * The regex for recognizing a variable declaration in the language.
     * @return the regex
     */
    Pattern variablePattern();

    /**
     * Gets the separator between substitutions in SUB
     * @return
     */
    default String substitutionSeparator() { return ";"; }
    
    default void runUnitTest(Path mainFile, Set<Path> dependentFiles, Path workdir, Report report, Score score,  int timeoutMillis, int maxOutputLen) {        
    }

    /**
     * Accepts an optional file for processing (such as checkstyle.xml)
     * 
     * @param file
     *            the file to check for optional processing
     * @param dir
     *            the directory containing the student files
     * @param studentFiles
     *            all student files
     * @param report
     *            the report to add to, if an action was taken
     * @param score
     *            the score object to reward
     * @return true if an action was taken
     */
    default boolean accept(Path file, Path dir, Report report,
            Score score) throws IOException { 
        return false; 
    }

    /**
     * Parses the function name from a function declaration. 
     * The default implementation works for C-like languages.
     * 
     * @param declaration
     *            the function declaration (e.g. public static int square(int n)
     *            in Java)
     * @return the function name, or null if not a function declaration
     */
    default String functionName(String declaration) {        
        int i = declaration.indexOf("(");
        if (i == -1)
            return null;
        while (i > 0 && Character.isWhitespace(declaration.charAt(i - 1)))
            i--;
        if (i == 0)
            return null;
        int j = i;
        while (j > 0 && !Character.isWhitespace(declaration.charAt(j - 1)))
            j--;
        return declaration.substring(j, i);
    }
    
    /**
     * Parses a function declaration for any modifiers that are needed for
     * test calls (e.g. static in Java). 
     * 
     * @param declaration
     *            the function declaration (e.g. public static int square(int n)
     *            in Java)
     * @return a list of the modifiers
     */
    default List<String> modifiers(String declaration) {
        return Collections.emptyList();
    }
    
    enum Interleave { ALWAYS, UNGRADED, NEVER }
    /**
     * Reports whether, when running the program, stdin is echoed to stdout, making input
     * and output interleaved.
     * @return true if stdin is echoed 
     */
    default Interleave echoesStdin() { return Interleave.NEVER; }
    
    default Pattern errorPattern() { return null; }
    
    default List<Error> errors(String report, boolean compileTime) {
        Pattern pattern = errorPattern();
        if (compileTime && pattern != null) {
            List<Error> result = new ArrayList<>();
            String[] lines = report.split("\n");
            int i = 0;
            while (i < lines.length) {
                Matcher matcher = pattern.matcher(lines[i]);
                
                if (matcher.matches()) {
                    result.add(new Error(matcher.group("file"), Integer.parseInt(matcher.group("line")), 0, matcher.group("msg")));
                }
                i++;
            }
            return result;
        }
        else return Collections.emptyList();
    }
}