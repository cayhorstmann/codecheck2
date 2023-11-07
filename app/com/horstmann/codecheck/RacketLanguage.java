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
    public boolean isMain(Path p, String contents) {
        if (mainPattern.matcher(contents).find()) return true;
        if (isUnitTest(p)) return false;
        if (contents.contains("(provide ")) return false; // TODO: Hack
        return true;
    }

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
       String moduleName = moduleOf(file);       
       StringBuilder out = new StringBuilder();
        
       out.append("#lang racket\n");
       out.append("(require (prefix-in submission:: \"" + moduleName + ".rkt\"))\n");
       out.append("(provide main)\n");
       out.append("(define (main . args)\n");
       out.append("(let ((arg (car args))) (cond\n");

       for (int k = 0; k < calls.size(); k++) {
          Calls.Call call = calls.get(k);
          out.append("((equal? \"" + (k + 1) + "\" arg)\n");
        

          out.append("(let ((result (submission::" + call.name + " " + call.args + ")))\n");
          out.append("(writeln result)))\n");
       }
       
       out.append(")))\n");        
        
       Map<Path, String> testerFiles = new HashMap<>();
       testerFiles.put(Paths.get(moduleName + "CodeCheck.rkt"), out.toString());
       return testerFiles;
    }

    // CAUTION: Because the rhs is likely to contain (), there must be a space
    // before the final ), e.g. (define l '(1 2 3) )
    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	"\\(\\s*define\\s+(?<name>[A-Za-z][A-Za-z0-9]*)\\s+(?<rhs>.+)\\s+\\)");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }

    
    @Override
    public boolean isUnitTest(Path fileName) {
        return fileName.toString().matches(".*(T|_t)est[0-9]*.rkt");
    }

   private static final Pattern successPattern = Pattern.compile("All (?<runs>[0-9]+) tests passed");
   private static final Pattern failurePattern = Pattern.compile("Ran [0-9]+ checks.\\s+(?<failures>[0-9]+) of the (?<runs>[0-9]+) checks failed");
   @Override public Pattern unitTestSuccessPattern() { return successPattern; }
   @Override public Pattern unitTestFailurePattern() { return failurePattern; }

   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
