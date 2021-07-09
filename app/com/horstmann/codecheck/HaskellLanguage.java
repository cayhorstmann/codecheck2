package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class HaskellLanguage implements Language {
   public String[] pseudoCommentDelimiters() { return new String[] { "--", "" }; }

   @Override
    public String getExtension() {
        return "hs";
    }

   private static final Pattern functionPattern = Pattern.compile("\\s*([^\\s]+)\\s*::.*->.*");
   
   @Override
   public String functionName(String declaration) {
      Matcher matcher = functionPattern.matcher(declaration);
      if (matcher.matches()) 
         return matcher.group(1);
      else
         return null;
   }
   
   private static Pattern mainPattern = Pattern
            .compile("main\\s*::\\s*IO\\s*\\(\\s*\\)");

   @Override 
   public Pattern mainPattern() { return mainPattern; }

    @Override public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        Map<Path, String> result = new HashMap<>();
        
        // Rewrite solution in module CodeCheckSolution
        String moduleName = moduleOf(file);
        Path solutionModuleFile = Paths.get("CodeCheckSolution.hs");
        StringBuilder out = new StringBuilder();
        List<String> in = Util.lines(contents);
        out.append("module CodeCheckSolution where\n");
        for (int i = 0; i < in.size(); i++) {
            String line = in.get(i);
            if (!line.trim().startsWith("module ")) {
                out.append(line);     
                out.append("\n");
            }
        }
        result.put(solutionModuleFile, out.toString());
        
        // Generate testCodeCheck.hs
        Path testFile = Paths.get("testCodeCheck.hs");
        out = new StringBuilder();
        out.append("import " + moduleName + "\n");
        out.append("import CodeCheckSolution\n");
        out.append("import Control.Exception\n");
        out.append("import System.Environment\n");
        out.append("main :: IO ()\n");
        out.append("main = do\n");
        out.append("    args <- getArgs\n");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            if (k == 0) out.append("    if (head args) == \"1\" then ");
            else if (k < calls.size() - 1) out.append("    else if (head args) == \"" + (k + 1) + "\" then ");
            else out.append("    else ");
            out.append(moduleName + "." + call.name + " " + call.args + " `comp` CodeCheckSolution."
                    + call.name + " " + call.args + "\n");
        }            
        out.append("  where\n");
        out.append("    exec expr = (do x <- evaluate expr ; return $ Just x)\n");
        out.append("      `catch` (\\(SomeException x) -> return Nothing)\n");
        out.append("    comp expr1 expr2 = do\n");
        out.append("      actual <- exec expr1\n");
        out.append("      expected <- exec expr2\n");
        out.append("      case (actual, expected) of\n");
        out.append("        (Nothing, Nothing) -> putStrLn \"error\\nerror\\ntrue\"\n");
        out.append("        (Just a, Just b) -> putStrLn$ (show a) ++ \"\\n\" ++ (show b) ++ (if a==b then \"\\ntrue\" else \"\\nfalse\")\n");
        out.append("        (Just a, Nothing) -> putStrLn $ (show a) ++ \"\\nerror\\nfalse\"\n");
        out.append("        (Nothing, Just b) -> putStrLn $ \"error\\n\" ++ (show b) ++ \"\\nfalse\"\n");
        
        result.put(testFile, out.toString());
        return result;
    }
        /*
 


    Student.maxNum [] `comp` CodeCheckSolution.maxNum []  -- function name and args
    Student.maxNum [1] `comp` CodeCheckSolution.maxNum [1]
    Student.maxNum [1,2,3] `comp` CodeCheckSolution.maxNum [1,2,3]
*/             
 
        
    
    private static String variablePatternString = "\\s*let\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>.+)";
    private static Pattern variablePattern = Pattern.compile(variablePatternString);

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#variablePattern()
     */
    @Override
    public Pattern variableDeclPattern() {
        return variablePattern;
    }
   
   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
