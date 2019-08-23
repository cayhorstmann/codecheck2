package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.horstmann.codecheck.Language.Interleave;

public class PythonLanguage implements Language {

    public PythonLanguage() {
        // TODO Auto-generated constructor stub
    }

    @Override
    public String getExtension() {
        return "py";
    }
    
    //@Override public Interleave echoesStdin() { return Interleave.UNGRADED; }    
    @Override public Interleave echoesStdin() { return Interleave.ALWAYS; }
    
    private static Pattern mainPattern = Pattern.compile("def\\s+main\\s*\\(\\s*\\)\\s*:");
    private static Pattern fundefPattern = Pattern.compile("def\\s+[A-Za-z0-9_]+\\s*\\(\\s*([A-Za-z0-9_]+(\\s*,\\s*[A-Za-z0-9_]+)*\\s*)?\\s*\\)\\s*:");

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isMain(java.nio.file.Path,
     * java.nio.file.Path)
     */
    @Override
    public boolean isMain(Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(p);
        if (contents == null) return false;
        if (mainPattern.matcher(contents).find()) return true;
        if (fundefPattern.matcher(contents).find()) return false;
        return true;
    }

    @Override
    public List<Path> writeTester(Path sourceDir, Path targetDir, Path file,
            List<Calls.Call> calls)
            throws IOException {
        String moduleName = moduleOf(file);
        List<String> lines = Util.readLines(sourceDir.resolve(file));
        int i = 0;
        lines.add(i++, "from sys import argv");
        lines.add(i++, "import " + moduleName);        
        lines.add(i++, "def main() :");
        for (int k = 0; k < calls.size(); k++) {
            Calls.Call call = calls.get(k);
            lines.add(i++, 
                    "    if argv[1] == \"" + (k + 1) + "\" :");
            lines.add(i++, 
                    "        expected = "
                     + call.name + "(" + call.args
                    + ")");
            lines.add(i++,
                    "        print(expected)");
            lines.add(i++, 
                    "        actual = "
                    + moduleName + "." + call.name + "("
                    + call.args + ")");
            lines.add(i++, 
                    "        print(actual)");
            lines.add(
                    i++,
                    "        if expected == actual :");
            lines.add(i++, 
                    "            print(\"true\")");
            lines.add(
                    i++,
                    "        else :");
            lines.add(i++, 
                    "            print(\"false\")");
        }
        lines.add("main()");
        Path p = pathOf(moduleName + "CodeCheck");
        Files.write(targetDir.resolve(p), lines, StandardCharsets.UTF_8);        
        List<Path> testFiles = new ArrayList<>();
        testFiles.add(p);
        return testFiles;        
    }

    @Override
    public String[] pseudoCommentDelimiters() {
        return new String[] { "##", "" };
    }

    private static String patternString = "\\s*(?<name>[A-Za-z][A-Za-z0-9]*)\\s*=\\s*(?<rhs>.+)";
    private static Pattern pattern = Pattern.compile(patternString);

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#variablePattern()
     */
    @Override
    public Pattern variablePattern() {
        return pattern;
    }

    // TODO: Same as Racket. 
    
    public boolean isUnitTest(Path fileName) { return fileName != null && fileName.toString().matches(".*Test[0-9]*.py"); }
    
    private static final Pattern successPattern = Pattern.compile("Ran ([0-9]+) tests in [0-9.]+s\\s+OK");
    private static final Pattern failurePattern = Pattern.compile("Ran (?<runs>[0-9]+) tests in [0-9.]+s\\s+FAILED \\([^=]+=(?<failures>[0-9]+)\\)");
    
    public @Override void runUnitTest(Path mainFile, Set<Path> dependentFiles, Path dir, Report report,
             Score score, int timeout, int maxOutput) {
       try {
          String result = run(mainFile, dependentFiles, dir, "", "", timeout, maxOutput, false); 
          Matcher matcher = successPattern.matcher(result);
          int runs = 0;
          int failures = 0;
          if (matcher.find())
          {
             runs = Integer.parseInt(matcher.group(1));
          }
          else
          {
             matcher = failurePattern.matcher(result);
             if (matcher.find())
             {
                failures = Integer.parseInt(matcher.group("failures"));
                runs = Integer.parseInt(matcher.group("runs"));
             }
          }
          report.run("Unit test");
          report.output(result + "\nRuns:" + runs + " Failures: " + failures);
          report.pass(failures == 0);
          score.add(runs - failures, runs, report);
       } catch (Throwable t) {
          report.systemError(t);
       }
    }

}
