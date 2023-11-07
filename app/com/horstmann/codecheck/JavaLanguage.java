package com.horstmann.codecheck;

import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.ArrayList;

public class JavaLanguage implements Language {
    public String getExtension() { return "java"; };
    
    @Override
    public boolean isUnitTest(Path fileName) {
        return fileName.toString().matches(".*(T|_t)est[0-9]*.java");
    }

    private static Pattern mainPattern = Pattern
            .compile("public\\s+static\\s+void\\s+main\\s*\\(\\s*String(\\s*\\[\\s*\\]\\s*\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*|\\s+\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*\\s*\\[\\s*\\])\\s*\\)");

    @Override 
    public Pattern mainPattern() { return mainPattern; }
    
    public String moduleOf(Path path) {
        String name = Util.removeExtension(path); // drop .java
        return name.replace(FileSystems.getDefault().getSeparator(), ".");
    }

    public Path pathOf(String moduleName) {
        Path p = FileSystems.getDefault().getPath("", moduleName.split("[.]"));
        Path parent = p.getParent();
        if (parent == null)
            return FileSystems.getDefault().getPath(moduleName + ".java");
        else
            return parent.resolve(p.getFileName().toString() + ".java");
    }

    @Override
    public Map<Path, String> writeTester(Path file, String contents, List<Calls.Call> calls, ResourceLoader resourceLoader) {
        String className = moduleOf(file);
        List<String> lines = new ArrayList<>(); 
        contents
           .lines()
           .filter(l -> l.trim().startsWith("import "))
           .forEach(l -> lines.add(l));
        lines.add("public class " + className + "CodeCheck {"); 
        lines.add("    public static void main(String[] args) throws Exception");
        lines.add("    {");
         for (int k = 0; k < calls.size(); k++) {
             Calls.Call call = calls.get(k);
             boolean isStatic = call.modifiers.contains("static");
            lines.add("        if (args[0].equals(\"" + (k + 1) + "\"))");
            lines.add("        {");
             if (!isStatic) {
                lines.add("            " + className + " obj1 = new " + className
                        + "();");
            } 
            lines.add("            Object result = "
                    + (isStatic ? className : "obj1") + "." + call.name + "("
                    + call.args + ");");
            lines.add("            System.out.println(_toString(result));");
            lines.add("        }");
        }
        lines.add("    }");
        lines.add("    private static String _toString(Object obj)");
        lines.add("    {");
        lines.add("      if (obj == null) return \"null\";");  
        lines.add("      if (obj instanceof Object[])");
        lines.add("         return java.util.Arrays.deepToString((Object[]) obj);");
        lines.add("      if (obj.getClass().isArray())");
        lines.add("         try { return (String) java.util.Arrays.class.getMethod(\"toString\", obj.getClass()).invoke(null, obj); }");
        lines.add("         catch (Exception ex) {}");
        lines.add("      return obj.toString();");
        lines.add("    }");
        lines.add("}");
        Path p = pathOf(className + "CodeCheck");
        Map<Path, String> testFiles = new HashMap<>();
        testFiles.put(p, Util.join(lines, "\n"));
        return testFiles;
    }
    
    private static Pattern VARIABLE_DECL_PATTERN = Pattern.compile(
        	"((public|static|final|private|protected)\\s+)*[A-Za-z0-9_<>\\[\\]]+\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);");
    @Override public Pattern variableDeclPattern() { return VARIABLE_DECL_PATTERN; }

    @Override
    public String process(Path file, Map<Path, String> submissionFiles) {
        if (file.getFileName().toString().equals("checkstyle.xml")) {
            String cmd = "CheckStyle";
            for (Path p : submissionFiles.keySet()) {
                if (isSource(p)) cmd += " " + p;
            }
            return cmd;
        } else return null;
    }
    
    public void reportProcessResult(String result, Report report, Score score) {                    
        report.header("checkStyle", "CheckStyle");
        report.output(result);
        score.pass(result.equals("Starting audit...\nAudit done.\n"), report);
    }

    private static final Pattern successPattern = Pattern.compile("OK \\((?<runs>[0-9]+) tests?\\)");
    private static final Pattern failurePattern = Pattern.compile("Tests run: (?<runs>[0-9]+),\\s+Failures?: (?<failures>[0-9]+)");
    @Override public Pattern unitTestSuccessPattern() { return successPattern; }
    @Override public Pattern unitTestFailurePattern() { return failurePattern; }
    
    @Override
    public List<String> modifiers(String declaration) {
        // TODO: Use regexp--what if it's '\tstatic'?
        if (declaration.contains(" static ")) return Collections.singletonList("static"); else return Collections.emptyList();
    }
    
    public Interleave echoesStdin() { return Interleave.ALWAYS; }

    private static Pattern ERROR_PATTERN = Pattern.compile("(.+/)?(?<file>[^/]+\\.java):(?<line>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }        
}
