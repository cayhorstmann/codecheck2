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
        String moduleName = moduleOf(file);
        // Generate testCodeCheck.hs
        Path testFile = Paths.get("testCodeCheck.hs");
        StringBuilder out = new StringBuilder();
        out.append("import " + moduleName + "\n");
        out.append("import Control.Exception\n");
        out.append("import System.Environment\n");
        out.append("main :: IO ()\n");
        out.append("main = do\n");
        out.append("    args <- getArgs\n");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            if (k == 0 && calls.size() > 1) out.append("    if (head args) == \"1\" then ");
            else if (k < calls.size() - 1) out.append("    else if (head args) == \"" + (k + 1) + "\" then ");
            else if (calls.size() > 1) out.append("    else ");
            else out.append("    ");
            out.append("pr $ " + call.name + " " + call.args + "\n");
        }            
        out.append("  where\n");
        out.append("    exec expr = (do x <- evaluate expr ; return $ Just x)\n");
        out.append("      `catch` (\\(SomeException x) -> return Nothing)\n");
        out.append("    pr expr = do\n");
        out.append("      res <- exec expr\n");
        out.append("      case res of\n");
        out.append("        Nothing -> putStrLn \"error\"\n");
        out.append("        Just a -> putStrLn $ show a\n");
        
        result.put(testFile, out.toString());
        return result;
    }
    
    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	"\\s*let\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>.+)");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
   
   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
