package com.horstmann.codecheck;

import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class JavaLanguage implements Language {
    public String getExtension() { return "java"; };
    
    @Override
    public boolean isUnitTest(Path fileName) {
        return fileName.toString().matches(".*Test[0-9]*.java");
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
        List<String> lines = Util.lines(contents);
        int i = 0;
        // TODO: Could be enum or record
        while (i < lines.size() && !lines.get(i).contains("class " + className))
            i++;
        if (i == lines.size())
            throw new CodeCheckException("Can't find class " + className
                    + " for inserting CALL in " + file);
        lines.set(i, lines.get(i).replace("class " + className,
            "class " + className + "CodeCheck { static class Solution { static class " + className));
        i = lines.size() - 1;
        while (i >= 0 && !lines.get(i).trim().equals("}"))
            i--;
        if (i == -1)
            throw new CodeCheckException("Can't find } for inserting CALL in "
                    + file);
        lines.add(i++, "        }");
        lines.add(i++, "    }");
        lines.add(i++, "    public static void main(String[] args) throws Exception");
        lines.add(i++, "    {");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            boolean isStatic = call.modifiers.contains("static");
            lines.add(i++, "        if (args[0].equals(\"" + (k + 1) + "\"))");
            lines.add(i++, "        {");
            if (!isStatic) {
                lines.add(i++, "            " + className + " obj1 = new " + className
                        + "();");
                lines.add(i++, "            Solution." + className + " obj2 = new Solution." + className + "();");
            }
            lines.add(i++, "            Object expected = "
                    + (isStatic ? "Solution."  + className : "obj2") + "." + call.name + "(" + call.args
                    + ");");
            lines.add(i++,
                    "            System.out.println(_toString(expected));");   
            lines.add(i++, "            Object actual = "
                    + (isStatic ? className : "obj1") + "." + call.name + "("
                    + call.args + ");");
            lines.add(i++, "            System.out.println(_toString(actual));");
            lines.add(
                    i++,
                    "            System.out.println(java.util.Objects.deepEquals(actual, expected));");
            lines.add(i++, "        }");
        }
        lines.add(i++, "    }");
        lines.add(i++, "    private static String _toString(Object obj)");
        lines.add(i++, "    {");
        lines.add(i++, "      if (obj == null) return \"null\";");  
        lines.add(i++, "      if (obj instanceof String) return \"\\\"\" + ((String) obj).replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\").replace(\"\\n\", \"\\\\n\") + \"\\\"\";");
        lines.add(i++, "      if (obj instanceof Object[])");
        lines.add(i++,
                "         return java.util.Arrays.deepToString((Object[]) obj);");
        lines.add(i++, "      if (obj.getClass().isArray())");
        lines.add(
                i++,
                "         try { return (String) java.util.Arrays.class.getMethod(\"toString\", obj.getClass()).invoke(null, obj); }");
        lines.add(i++, "         catch (Exception ex) {}");
        lines.add(i++, "      return obj.toString();");
        lines.add(i++, "    }");

        // expected == null ? null : expected instanceof Object[] ?
        // java.util.Arrays.deepToString((Object[]) expected) :
        // expected.getClass().isArray() ? java.util.Arrays.toString(expected) :
        // expected
        Path p = pathOf(className + "CodeCheck");
        Map<Path, String> testFiles = new HashMap<>();
        testFiles.put(p, Util.join(lines, "\n"));
        return testFiles;
    }
    
    private static String patternString = "\\s*[A-Za-z0-9_<>\\[\\]]+\\s+(?<name>\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*(?<rhs>[^;]+);.*";
    private static Pattern pattern = Pattern.compile(patternString);

    @Override
    public Pattern variableDeclPattern() {
        return pattern;
    }

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

    private static final Pattern successPattern = Pattern.compile("OK \\((?<runs>[0-9]+) tests\\)");
    private static final Pattern failurePattern = Pattern.compile("Tests run: (?<runs>[0-9]+),\\s+Failures: (?<failures>[0-9]+)");
    @Override public Pattern unitTestSuccessPattern() { return successPattern; }
    @Override public Pattern unitTestFailurePattern() { return failurePattern; }
    
    @Override
    public List<String> modifiers(String declaration) {
        // TODO: Use regexp--what if it's '\tstatic'?
        if (declaration.contains(" static ")) return Collections.singletonList("static"); else return Collections.emptyList();
    }
    
    public Interleave echoesStdin() { return Interleave.ALWAYS; }

    private static Pattern ERROR_PATTERN = Pattern.compile(".+/(?<file>[^/]+\\.java):(?<line>[0-9]+): error: (?<msg>.+)");
    @Override public Pattern errorPattern() { return ERROR_PATTERN; }        
}
