package com.horstmann.codecheck;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class SMLLanguage implements Language {
   public String[] pseudoCommentDelimiters() { return new String[] { "\\(\\*", "\\*\\)" }; }

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
        out.append("structure Solution = struct\n");
        out.append(contents);
        out.append("\n");
        out.append("end;\n");
        out.append("fun eval expr = SOME (expr ()) handle exn => NONE ;\n");
        out.append("fun comp expr1 expr2 = let\n");
        out.append("    val actual = eval expr1\n");
        out.append("    val expected = eval expr2\n");
        out.append("  in\n");
        out.append("    (case (actual, expected) of\n");
        out.append("      (NONE, NONE) => \"exception\\nexception\\ntrue\\n\" |\n");
        out.append("      (NONE, SOME y) => \"exception\\n\" ^ (PolyML.makestring y) ^ \"\\nfalse\\n\" |\n");
        out.append("      (SOME x, NONE) => (PolyML.makestring x) ^ \"\\nexception\\nfalse\\n\" |\n");
        out.append("      (SOME x, SOME y) => (PolyML.makestring x) ^ \"\\n\" ^ (PolyML.makestring y) ^ \"\\n\" ^ (PolyML.makestring (x = y)) ^ \"\\n\")\n");
        out.append("end;\n");
        out.append("fun main() = print (case hd(CommandLine.arguments()) of \n");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            if (k < calls.size() - 1) 
                out.append("  \"" + (k + 1) + "\" => comp (fn () => (Solution." + 
                    call.name + " " + call.args + ")) (fn () => (" +
                    call.name + " " + call.args + ")) |\n");
            else 
                out.append("  _ => comp (fn () => (Solution." + 
                    call.name + " " + call.args + ")) (fn () => (" +
                    call.name + " " + call.args + ")));\n");
        }            
        Map<Path, String> result = new HashMap<>();
        result.put(testFile, out.toString());
        return result;
    }
    
    private static String variablePatternString = "\\s*val\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>.+)";
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
   
   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.sml):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
