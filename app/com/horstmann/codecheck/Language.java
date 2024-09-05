package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public interface Language {
    static Language[] languages = {
       new JavaLanguage(),
       new PythonLanguage(),
       new CLanguage(),
       new CppLanguage(),
       new ScalaLanguage(),
       new MatlabLanguage(),
       new RacketLanguage(),
       new JavaScriptLanguage(),
       new CSharpLanguage(),
       new HaskellLanguage(),
       new SMLLanguage(),
       new DartLanguage(),
       new RustLanguage(),
       new BashLanguage(),
       new KotlinLanguage()
    };

    static Language languageFor(Set<Path> files) {
        Language language = null;
        for (int k = 0; language == null && k < languages.length; k++) {
            if (languages[k].isLanguage(files)) 
                language = languages[k];
        }
        return language;
    }
    
    /**
     * Gets the extension required for source files in this language. 
     * @return the extension (without a period), or null if there 
     * is no one such extension.
     */
    String getExtension(); 
    
    /**
     * Tests if a file is a source file in this language. Some languages 
     * may have multiple forms of source files, e.g. C++ has 
     * .cpp and .h source files
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
     * @param fileNames
     * @return
     */
    default boolean isLanguage(Collection<Path> fileNames) {
        String extension = getExtension();
        // Don't call isSource because that may have been overridden to classify 
        // header files as source
        for (Path p : fileNames)
            if (extension != null && p.toString().endsWith("." + extension)) return true;
        return false;                    
    }

    /**
     * Tests if a file is an "Expected" style unit test file in this language.
     * @param p the path to the file
     * @return true if it is an "Expected" style unit test file
     */
    default boolean isTester(Path fileName) { 
        if (fileName == null) return false;
        String moduleName = moduleOf(fileName);
        if (moduleName == null) return false;
        return moduleName.matches(".*(T|_t)ester[0-9]*");
    }

    /**
     * Tests if a file is an "XUnit" style unit test file in this language.
     * @param p the path to the file
     * @return true if it is an "XUnit" style unit test file
     */
    default boolean isUnitTest(Path fileName) { return false; }

    default Pattern mainPattern() { return null; }
    
    /**
     * Tests if a file is a "main" file, i.e. an entry point for execution. By default,
     * a file whose module name ends in Runner or Tester, optionally followed by a number, matches.
     * Many languages instead check whether the file contains something such as "public 
     * static void main"
     * @param fileName the path to the file
     * @return true if it is a "main" file
     */
    default boolean isMain(Path fileName, String contents) { 
        String extension = getExtension();
        // Don't call isSource because that may have been overridden to classify 
        // header files as source
        if (!fileName.toString().endsWith("." + extension)) return false;
        Pattern p = mainPattern();
        if (p == null)
            return moduleOf(fileName).matches(".*(Runn|Test)er[0-9]*");
        else
            return p.matcher(contents).find();
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
     * @param file the relative path of the submitted/solved file
     * @param contents the file contents
     * @param calls the calls to be made
     * @return a map containing the file paths and contents of the the tester and any helper files, or null if this language
     * cannot produce a CALL tester.
     */
    default Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) throws IOException {
        return null;
    }

    default String[] pseudoCommentDelimiters() { return new String[] { "//", "" }; }

    /**
     * The regex for recognizing a variable declaration in the language.
     * @return the regex, with groups name and rhs
     */
    Pattern variableDeclPattern();

    /**
     * Gets the separator regex between substitutions in SUB
     * @return
     */
    default Pattern substitutionSeparator() { return Pattern.compile("\\s*;\\s*"); }
    
    default Pattern unitTestSuccessPattern() { return Pattern.compile("$."); }
    default Pattern unitTestFailurePattern() { return Pattern.compile("$."); }
    
    default void reportUnitTest(String result, Report report, Score score, boolean hidden) {
        if (hidden == false)
            report.output(result);
        else 
            report.hiddenOutputMessage(); 
        Matcher matcher = unitTestSuccessPattern().matcher(result);
        if (matcher.find()) {
           int runs = Integer.parseInt(matcher.group("runs"));
           score.add(runs, runs, report);
        }
        else {
           matcher = unitTestFailurePattern().matcher(result);
           if (matcher.find()) {
              int runs = Integer.parseInt(matcher.group("runs"));
              int failures = Integer.parseInt(matcher.group("failures"));
              score.add(runs - failures, runs, report);
           }
           else 
               report.systemError("Cannot determine unit test outcome");        
       }        
    }

    /**
     * Accepts an optional file for processing (such as checkstyle.xml)
     * 
     * @param file
     *            the file to check for optional processing
     * @param submissionDir
     *            the directory containing the student files
     */
    default String process(Path file, Map<Path, String> submissionFiles) { 
        return null; 
    }
    
    default void reportProcessResult(String result, Report report, Score score) {        
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
    
    default List<Error> errors(String report) {     
        Pattern pattern = errorPattern();
        if (pattern != null) {
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
        else 
            return Collections.emptyList();
    }
}