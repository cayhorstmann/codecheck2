package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
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
    public boolean isMain(Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(p);
        if (contents == null) return false;
        return mainPattern.matcher(contents).find();
    }

    public List<Path> writeTester(Path solutionDir, Path workDir, Path file,
            List<Calls.Call> calls) throws IOException {        
        String moduleName = moduleOf(file);
        
        Path testFile = workDir.resolve("testCodeCheck.sml");
        try (PrintWriter out = new PrintWriter(testFile.toFile(), "UTF-8")) {
            out.println("use \"" + moduleName + ".sml\";");
            out.println("structure Solution = struct");
            out.println(Util.read(solutionDir.resolve(file)));
            out.println("end;   ");
            out.println("fun eval expr = SOME (expr ()) handle exn => NONE ;");
            out.println("fun comp expr1 expr2 = let");
            out.println("    val actual = eval expr1");
            out.println("    val expected = eval expr2");
            out.println("  in");
            out.println("    (case (actual, expected) of");
            out.println("      (NONE, NONE) => \"exception\\nexception\\ntrue\\n\" |");
            out.println("      (NONE, SOME y) => \"exception\\n\" ^ (PolyML.makestring y) ^ \"\\nfalse\\n\" |");
            out.println("      (SOME x, NONE) => (PolyML.makestring x) ^ \"\\nexception\\nfalse\\n\" |");
            out.println("      (SOME x, SOME y) => (PolyML.makestring x) ^ \"\\n\" ^ (PolyML.makestring y) ^ \"\\n\" ^ (PolyML.makestring (x = y)) ^ \"\\n\")");
            out.println("end;");
            out.println("fun main() = print (case hd(CommandLine.arguments()) of ");
            for (int k = 0; k < calls.size(); k++) {
                Calls.Call call = calls.get(k);
                if (k < calls.size() - 1) 
                    out.println("  \"" + (k + 1) + "\" => comp (fn () => (Solution." + 
                        call.name + " " + call.args + ")) (fn () => (" +
                        call.name + " " + call.args + ")) |");
                else 
                    out.println("  _ => comp (fn () => (Solution." + 
                        call.name + " " + call.args + ")) (fn () => (" +
                        call.name + " " + call.args + ")));");
            }            
        }
        return Arrays.asList(testFile);
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
   
   private static Pattern ERROR_PATTERN = Pattern.compile("(?<file>[^/]+\\.rkt):(?<line>[0-9]+):(?<col>[0-9]+): (?<msg>.+)");
   @Override public Pattern errorPattern() { return ERROR_PATTERN; }     
}
