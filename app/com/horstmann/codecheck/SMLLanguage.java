package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class SMLLanguage implements Language {
   public String[] pseudoCommentDelimiters() { return new String[] { "(*", "*)" }; }

   @Override
    public String getExtension() {
        return "sml";
    }

   private static final Pattern functionPattern = Pattern.compile("\\s*fun\\s+([\\pL][\\pL\\pN_]*).*");
   
   @Override
   public String functionName(String declaration) {
      Matcher matcher = functionPattern.matcher(declaration);
      if (matcher.matches()) 
         return matcher.group(1);
      else
         return null;
   }
   
   private static Pattern mainPattern = Pattern
            .compile("fun\\s+main\\s*\\(\\s*\\)\\s*=");

   @Override 
   public Pattern mainPattern() { return mainPattern; }

    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {        
        String moduleName = moduleOf(file);
        
        Path testFile = Paths.get("testCodeCheck.sml");
        StringBuilder out = new StringBuilder();
        out.append("use \"" + moduleName + ".sml\";\n");
        out.append("fun main() = print (PolyML.makestring (case hd(CommandLine.arguments()) of \n");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            if (k < calls.size() - 1) 
                out.append("  \"" + (k + 1) + "\" => (" +
                    call.name + " " + call.args + ") |\n");
            else 
                out.append("  _ => (" +
                    call.name + " " + call.args + ")) handle exn => \"exception\");\n");
        }            
        Map<Path, String> result = new HashMap<>();
        result.put(testFile, out.toString());
        return result;
    }
    
    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
    		"val\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);?");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }
   
   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.sml):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
