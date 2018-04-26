package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
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
    public boolean isMain(Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(p);
        if (contents == null) return false;
        return mainPattern.matcher(contents).find();
    }

    public List<Path> writeTester(Path sourceDir, Path targetDir, Path solutionFile,
            List<Calls.Call> calls) throws IOException {
        // Rewrite solution in module CodeCheckSolution
        
        String moduleName = moduleOf(solutionFile);
        Path solutionModule = targetDir.resolve("CodeCheckSolution.hs");
        try (PrintWriter out = new PrintWriter(solutionModule.toFile(), "UTF-8");
                Scanner in = new Scanner(solutionFile, "UTF-8")) {
            out.println("module CodeCheckSolution where");            
            while (in.hasNextLine()) {
                String line = in.nextLine();
                if (!line.trim().startsWith("module "))
                    out.println(line);     
            }
        }
        
        // Generate testCodeCheck.hs
        Path testModule = targetDir.resolve("testCodeCheck.hs");
        try (PrintWriter out = new PrintWriter(testModule.toFile(), "UTF-8")) {
            out.println("import " + moduleName);
            out.println("import CodeCheckSolution");
            out.println("import Control.Exception");
            out.println("import System.Environment");
            out.println("main :: IO ()");
            out.println("main = do");
            out.println("    args <- getArgs");
            for (int k = 0; k < calls.size(); k++) {
                Calls.Call call = calls.get(k);
                if (k == 0) out.print("    if (head args) == \"1\" then ");
                else if (k < calls.size() - 1) out.print("    else if (head args) == \"" + (k + 1) + "\" then ");
                else out.print("    else ");
                out.println(moduleName + "." + call.name + " " + call.args + " `comp` CodeCheckSolution."
                        + call.name + " " + call.args);
            }            
            out.println("  where");
            out.println("    exec expr = (do x <- evaluate expr ; return $ Just x)");
            out.println("      `catch` (\\(SomeException x) -> return Nothing)");
            out.println("    comp expr1 expr2 = do");
            out.println("      actual <- exec expr1");
            out.println("      expected <- exec expr2");
            out.println("      case (actual, expected) of");
            out.println("        (Nothing, Nothing) -> putStrLn \"error\\nerror\\ntrue\"");
            out.println("        (Just a, Just b) -> putStrLn$ (show a) ++ \"\\n\" ++ (show b) ++ (if a==b then \"\\ntrue\" else \"\\nfalse\")");
            out.println("        (Just a, Nothing) -> putStrLn $ (show a) ++ \"\\nerror\\nfalse\"");
            out.println("        (Nothing, Just b) -> putStrLn $ \"error\\n\" ++ (show b) ++ \"\\nfalse\"");
        }
        return Arrays.asList(testModule);
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
    public Pattern variablePattern() {
        return variablePattern;
    }
   
   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
