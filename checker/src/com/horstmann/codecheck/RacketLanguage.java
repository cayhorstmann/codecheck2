package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class RacketLanguage implements Language {
    
    //TODO: Turn off compiling
    
   public String[] pseudoCommentDelimiters() { return new String[] { ";;", "" }; }

   @Override
    public String getExtension() {
        return "rkt";
    }

   private static final Pattern functionPattern = Pattern.compile("\\s*\\(\\s*define\\s*\\(\\s*([^\\s]+).*");
   
   @Override
   public String functionName(String declaration) {
      Matcher matcher = functionPattern.matcher(declaration);
      if (matcher.matches()) 
         return matcher.group(1);
      else
         return null;
   }
   
   private static Pattern mainPattern = Pattern
            .compile("\\(\\s*define\\s*\\(\\s*main\\s+\\.[^)]+\\)");
   // (define (main . args)
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
        if (isUnitTest(p)) return false;
        if (contents.contains("(provide ")) return false; // TODO: Hack
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#writeTester(java.nio.file.Path,
     * java.nio.file.Path, java.nio.file.Path, java.util.List, java.lang.String,
     * java.util.List)
     */
    @Override
    public List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Calls.Call> calls)
            throws IOException {
       String moduleName = Util.removeExtension(Util.tail(file));
        // Copy source/module.rkt to target/module-solution.rkt
        Files.copy(sourceDir.resolve(file), targetDir.resolve(moduleName+ "-solution.rkt"));
        String testerModule = moduleName + "CodeCheck.rkt";
        Path testerFile = targetDir.resolve(testerModule);
        try (PrintWriter out = new PrintWriter(testerFile.toFile(), "UTF-8")) {
           out.println("#lang racket");
           out.println("(require (prefix-in solution:: \"" + moduleName + "-solution.rkt\"))");
           out.println("(require (prefix-in student:: \"" + moduleName + ".rkt\"))");
           out.println("(provide main)");
           out.println("(define (main . args)");
           out.println("(let ((arg (car args))) (cond");

           for (int k = 0; k < calls.size(); k++) {
              Calls.Call call = calls.get(k);
              out.println("((equal? \"" + (k + 1) + "\" arg)");
            

              out.println("(let ((actual (student::" + call.name + " " + call.args + "))");
              out.println("(expected (solution::" + call.name + " " + call.args + ")))");
              out.println("(writeln expected)");
              out.println("(writeln actual)");
              out.println("(display (if (equal? actual expected) \"true\\n\" \"false\\n\"))))");
           }
           
           out.println(")))");
        }
        
        List<Path> testModules = new ArrayList<>();
        testModules.add(testerFile);
        return testModules;
    }
    
    private static String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*([^;]+);.*";
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

    @Override
    public boolean isUnitTest(Path modulename) {
        return modulename != null && modulename.toString().matches(".*Test[0-9]*.rkt");
    }

   private static final Pattern successPattern = Pattern.compile("All ([0-9]+) tests passed");
   private static final Pattern failurePattern = Pattern.compile("Ran [0-9]+ checks.\\s+([0-9]+) of the ([0-9]+) checks failed");
   
   public @Override void runUnitTest(Path mainModule, Set<Path> dependentModules, Path dir, Report report,
            Score score, int timeout, int maxOutput) {
      try {
         String result = run(mainModule, dependentModules, dir, "", "", timeout, maxOutput); 
         Matcher matcher = successPattern.matcher(result);
         int runs = 0;
         int failures = 0;
         if (matcher.find())
         {
            runs = Integer.parseInt(matcher.group(1));
         }
         else
         {
            matcher = failurePattern.matcher(result);
            if (matcher.find())
            {
               failures = Integer.parseInt(matcher.group(1));
               runs = Integer.parseInt(matcher.group(2));
            }
         }
         report.run("Unit test");
         report.output(result + "\nRuns:" + runs + " Failures: " + failures);
         report.pass(failures == 0);
         score.add(runs - failures, runs, report);
      } catch (Throwable t) {
         report.systemError(t);
      }
   }

   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
