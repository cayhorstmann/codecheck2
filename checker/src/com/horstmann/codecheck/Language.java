package com.horstmann.codecheck;

import java.io.IOException;
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
     * Tests if a file is a source file in this language.
     * @param p the path to the file
     * @return true if it is a source file
     */
    boolean isSource(Path p);
    
    /**
     * Tests if this collection of files contains source files
     * of this language. For example, for C++, there would have to be 
     * at least one .cpp file. A .file is source, but not an indication for C++
     * The default implementation, which works for Java etc., just checks 
     * that there is at least one source file.
     * @param files
     * @return
     */
    default boolean isLanguage(Collection<Path> files) {
        for (Path p : files)
            if (isSource(p)) return true;
        return false;                    
    }

    /**
     * Tests if a file is an "Expected" style unit test file in this language.
     * @param p the path to the file
     * @return true if it is an "Expected" style unit test file
     */
    default boolean isTester(Path modulename) { 
        return modulename != null && modulename.toString().matches(".*Tester[0-9]*\\.[^.]+");
    }


    /**
     * Tests if a file is an "XUnit" style unit test file in this language.
     * @param p the path to the file
     * @return true if it is an "XUnit" style unit test file
     */
    default boolean isUnitTest(Path modulename) { return false; }

    /**
     * Tests if a file is a "main" file, i.e. an entry point for execution.
     * @param p the path to the file
     * @return true if it is a "main" file
     */
    boolean isMain(Path p);

    // TODO: Why wire in the report? Maybe needs to return a compilation result with success, messages, name of executable?
    /**
     * Compiles a program
     * @param modules the modules that need to be compiled. The first one
     * is the "main" one. 
     * @param dir the directory containing the modules
     * @return null for no error, or an error report if there was an error.
     */
    default String compile(List<Path> modules, Path dir) {
        List<String> cmd = new ArrayList<>();
        if (System.getProperty("os.name").toLowerCase().contains("win")) // We lose
            cmd.add(Util.getHomeDir() + "\\comprog.bat");
        else
            cmd.add(Util.getHomeDir() + "/comprog");
        cmd.add(getLanguage());
        for (Path p : modules)
            cmd.add(dir.resolve(p).toString());
        StringBuilder output = new StringBuilder();
        int exitValue = Util.runProcess(cmd, null, Integer.MAX_VALUE, output);
        if (exitValue != 0) {
            return output.toString();
        } else
            return null;
    }

    /**
     * Runs a program
     * @param mainModule the path of the main module (from which the
     * program name needs to be derived, in concert with the compile method) 
     * @param dir the directory containing the main module
     * @param args the command-line arguments
     * @param input the input to pass to stdin  
     * @return the combined stdout/stderr of the run 
     */
    default String run(Path mainModule, Path dir, String args,
            String input, int timeoutMillis) throws Exception {
        List<String> cmd = new ArrayList<>();
        if (System.getProperty("os.name").toLowerCase().contains("win")) // We lose
            cmd.add(Util.getHomeDir() + "\\runprog.bat");
        else
            cmd.add(Util.getHomeDir() + "/runprog");
        cmd.add(getLanguage());
        String programName = dir.resolve(mainModule).toString();
        cmd.add(programName);
        if (args != null) cmd.addAll(Arrays.asList(args.split("\\s+")));
        StringBuilder output = new StringBuilder();        
        Util.runProcess(cmd, input, timeoutMillis, output);
        return output.toString();
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
     * @return the Path to the written tester relative to the target directory, and any helper modules
     * @throws IOException
     */
    List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Calls.Call> calls)
            throws IOException;

    default String[] pseudoCommentDelimiters() { return new String[] { "//", "" }; }

    Pattern variablePattern();

    /**
     * Gets the separator between substitutions in SUB
     * @return
     */
    default String substitutionSeparator() { return ";"; }
    
    default void runUnitTest(List<Path> modules, Path workdir, Report report, Score score,  int timeoutMillis) {        
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
    default boolean accept(Path file, Path dir, Set<Path> studentFiles, Report report,
            Score score) { 
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
    
    /**
     * Reports whether, when running the program, stdin is echoed to stdout, making input
     * and output interleaved.
     * @return true if stdin is echoed 
     */
    default boolean echoesStdin() { return false; }
    
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