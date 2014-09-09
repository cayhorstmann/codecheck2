package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public interface Language {

    boolean isSource(Path p);

    boolean isTester(String modulename);

    boolean isUnitTest(String modulename);

    boolean isMain(Path dir, Path p);

    String moduleOf(Path path);

    Path pathOf(String moduleName);

    boolean compile(String modulename, Path dir, Report report);

    /**
     * Runs a Java program
     *
     * @param mainclass
     * @param classpathDir
     * @param input
     * @return an array of two strings, holding the program output and errors
     * @throws IOException
     * @throws ReflectiveOperationException
     */
    String run(String mainmodule, Path moduleDir, String args, String input,
            int timeoutMillis) throws IOException, ReflectiveOperationException;

    void writeTester(Path sourceDir, Path targetDir, Path file,
            List<String> modifiers, String name, List<String> argsList)
            throws IOException;

    String[] pseudoCommentDelimiters();

    Pattern variablePattern();

    String substitutionSeparator();

    void runUnitTest(String modulename, Path dir, Report report, Score score);

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
    boolean accept(Path file, Path dir, Set<Path> studentFiles, Report report,
            Score score); // TODO: Make an object describing the problem

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
        // TODO: Use regexp--what if it's '\tstatic'?
        return declaration.contains(" static ") ? Collections.singletonList("static")
                : Collections.emptyList();
    }
}