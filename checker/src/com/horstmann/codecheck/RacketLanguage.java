package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls) {
       String moduleName = moduleOf(file);       
        // Copy source/module.rkt to target/module-solution.rkt
       Map<Path, String> testerFiles = new HashMap<>();
       testerFiles.put(Paths.get(moduleName+ "-solution.rkt"), contents);
       StringBuilder out = new StringBuilder();
        
       out.append("#lang racket\n");
       out.append("(require (prefix-in solution:: \"" + moduleName + "-solution.rkt\"))\n");
       out.append("(require (prefix-in student:: \"" + moduleName + ".rkt\"))\n");
       out.append("(provide main)\n");
       out.append("(define (main . args)\n");
       out.append("(let ((arg (car args))) (cond\n");

       for (int k = 0; k < calls.size(); k++) {
          Calls.Call call = calls.get(k);
          out.append("((equal? \"" + (k + 1) + "\" arg)\n");
        

          out.append("(let ((actual (student::" + call.name + " " + call.args + "))\n");
          out.append("(expected (solution::" + call.name + " " + call.args + ")))\n");
          out.append("(writeln expected)\n");
          out.append("(writeln actual)\n");
          out.append("(display (if (equal? actual expected) \"true\\n\" \"false\\n\"))))\n");
       }
       
       out.append(")))\n");        
        
       testerFiles.put(Paths.get(moduleName + "CodeCheck.rkt"), out.toString());
       return testerFiles;
    }
    
    private static String patternString = ".*\\S\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);.*";
    private static Pattern pattern = Pattern.compile(patternString);

    @Override public Pattern variablePattern() { return pattern; }

    @Override
    public boolean isUnitTest(Path fileName) {
        return fileName != null && fileName.toString().matches(".*Test[0-9]*.rkt");
    }

   private static final Pattern successPattern = Pattern.compile("All (?<runs>[0-9]+) tests passed");
   private static final Pattern failurePattern = Pattern.compile("Ran [0-9]+ checks.\\s+(?<failures>[0-9]+) of the (?<runs>[0-9]+) checks failed");
   @Override public Pattern unitTestSuccessPattern() { return successPattern; }
   @Override public Pattern unitTestFailurePattern() { return failurePattern; }

   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
