package com.horstmann.codecheck;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Scanner;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.TreeSet;

/*
 * Run this program from the "work directory", the directory in which all compilation and
 * execution happens. The submission directory and problem directory are left untouched.
 *
 */

public class Main { 
    public static final double DEFAULT_TOLERANCE = 1.0E-6;
    public static final int DEFAULT_TIMEOUT_MILLIS = 30000;
    public static final int DEFAULT_MAX_OUTPUT_LEN = 100_000;
    public static final String DEFAULT_TOKEN = "line";
    public static final int MUCH_LONGER = 1000; // if longer than the expected by this amount, truncate 
        
    private int timeoutMillis;
    private int maxOutputLen;
    private Path workDir;
    private Properties checkProperties = new Properties();
    private Report report;
    private Path problemDir;
    private List<String> studentDirectories = new ArrayList<>();
    private Set<Path> studentFiles;
    private Set<Path> solutionFiles;
    private Set<Path> printFiles = new TreeSet<>();
    private Set<Path> requiredFiles = new TreeSet<>(); // tails only
    private Set<Path> mainModules = new TreeSet<>(); // tails only
    private Set<Path> dependentModules = new TreeSet<>(); // tails only
    
    private Score score = new Score();
    private Comparison comp = new Comparison();
    private Language language = null;
    // TODO: Use service loader???
    private Language[] languages = {
       new JavaLanguage(),
       new PythonLanguage(),
       new CLanguage(),
       new CppLanguage(),
       new ScalaLanguage(),
       new MatlabLanguage(),
       new RacketLanguage(),
       new JavaScriptLanguage(),
       new CSharpLanguage()
    };

    /**
     * Entry point to program.
     *
     * @param args
     *            command-line arguments. args[0] = level (1, 2, 3, ..., or
     *            check/grade for compatibility) args[1] = submission dir,
     *            args[2] = problem dir args[3] etc = metadata key=value pairs (optional)
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        new Main().run(args);
    }

    // TODO: Should be in Util
    public static boolean matches(Path path, String glob) {
        PathMatcher matcher = FileSystems.getDefault().getPathMatcher(
                                  "glob:" + glob.replace("/", FileSystems.getDefault().getSeparator()));
        return matcher.matches(path);
    }

    // TODO: Should be in Util
    public static Set<Path> filter(Set<Path> paths, String glob) {
        Set<Path> result = new TreeSet<>();
        for (Path p : paths)
            if (matches(p.getFileName(), glob))
                result.add(p);
        return result;
    }

    // TODO: Should be in Util
    public static Set<Path> filterNot(Set<Path> paths, String... glob) {
        Set<Path> result = new TreeSet<>();
        PathMatcher[] matcher = new PathMatcher[glob.length];
        for (int i = 0; i < matcher.length; i++)
            matcher[i] = FileSystems.getDefault().getPathMatcher(
                             "glob:" + glob[i].replace("/", FileSystems.getDefault().getSeparator()));
        for (Path p : paths) {
            boolean matchesOne = false;
            for (int i = 0; i < matcher.length && !matchesOne; i++)
                if (matcher[i].matches(p.getFileName()))
                    matchesOne = true;
            if (!matchesOne)
                result.add(p);
        }
        return result;
    }

    public void copySuppliedFiles() throws IOException {
        for (String s : studentDirectories) {
            Path dir = problemDir.resolve(s);        
            for (Path p : Util.getDescendantFiles(dir)) {
                Path source = dir.resolve(p);
                Path target = workDir.resolve(p);
                // Copy if it is not required to be submitted
                if (!requiredFiles.contains(p)) {
                    Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                }
            }
        }
    }

    // TODO: Should be in Util
    public void copyAll(Collection<Path> paths, Path fromDir, Path toDir)  throws IOException {
        for (Path p : paths) {
            Path source = fromDir.resolve(p);
            Path target = toDir.resolve(p);
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        }        
    }
    
    private boolean containsModule(Set<Path> files, Path modulename) {
        for (Path file : files)
            if (Util.tail(file).equals(modulename))
                return true;
        return false;
    }

    public boolean compile(Path mainModule) {
        return compile(Collections.singletonList(mainModule));
    }

    /**
     * Runs the compiler
     * @param modules a list of paths, starting with the path to the main module
     * @return true if compilation succeeds
     */
    public boolean compile(List<Path> modules) {
        List<Path> allModules = new ArrayList<>();
        allModules.addAll(modules);
        allModules.addAll(dependentModules);
        String errorReport = language.compile(allModules, workDir);
        if (errorReport == null) return true;        
        if (errorReport.trim().equals(""))
            report.error("Error compiling " + modules.get(0));
        else {
            report.error(errorReport);
            report.errors(language.errors(errorReport, true));
        }
        return false;
    }

    public Path compileSolution(Path mainModule, Substitution sub, int n) throws IOException {
        Path tempDir = Util.createTempDirectory();
        for (Path p : studentFiles) {
            Path source = problemDir.resolve(p);
            Path target = tempDir.resolve(Util.tail(p));
            Files.copy(source, target);
        }
        for (Path p : solutionFiles) {
            Path source = problemDir.resolve(p);
            Path target = tempDir.resolve(Util.tail(p));
            if (sub != null && sub.getFile().equals(p))
                sub.substitute(source, target, n);
            else
                Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        }
        List<Path> modules = new ArrayList<>();
        modules.add(mainModule);
        modules.addAll(dependentModules);        
        String errorReport = language.compile(modules, tempDir);
        if (errorReport == null) return tempDir;
        else {
            report.systemError("Error compiling " + modules + "\n" + errorReport);
            return null;
        }
    }

    private String getStringProperty(String key, String... fallbackKeysAndDefaultValue) {
        String value = checkProperties.getProperty(key);
        if (value != null)
            return value;
        for (int i = 0; i < fallbackKeysAndDefaultValue.length - 1; i++) {
            value = checkProperties.getProperty(fallbackKeysAndDefaultValue[i]);
            if (value != null)
                return value;
        }
        if (fallbackKeysAndDefaultValue.length > 0)
            return fallbackKeysAndDefaultValue[fallbackKeysAndDefaultValue.length - 1];
        else
            return null;
    }

    private void runTester(Path mainmodule, int timeout, int maxOutputLen) throws Exception {
        report.run("Running " + mainmodule);
        // TODO: Assume testers always in default package?
        
        // TODO: Scoring doesn't work when outerr contains an exception report because we don't know how many
        // test cases have not occurred. 
        // May need to count the number of expected cases in the 
        
        if (compile(mainmodule)) {
            String outerr = language.run(mainmodule, dependentModules, workDir, "", null, timeout, maxOutputLen);
            AsExpected cond = new AsExpected(comp);
            cond.eval(outerr, report, score, workDir.resolve(mainmodule));
        } else
            score.setInvalid();
    }

    private void testInputs(Map<String, String> inputs, Path mainmodule, Annotations annotations) throws Exception {
        /*
         * If there are no inputs, we feed in one empty input to execute the program.
         */
        if (inputs.size() == 0)
            inputs.put("", ""); 
        report.header("run", "Testing " + mainmodule);
        Path solutionDir = null;
        if (!annotations.isSample(mainmodule)) {
            solutionDir = compileSolution(mainmodule, null, 0);
            if (solutionDir == null) return;
        }        
        
        if (compile(mainmodule)) {
            for (String test : inputs.keySet()) {
                String input = inputs.get(test);
                testInput(mainmodule, annotations, solutionDir, test, input, timeoutMillis / inputs.size(), maxOutputLen / inputs.size());
            }
        } else
            score.setInvalid();
        
        Util.deleteDirectory(solutionDir);
    }

    private void testInput(Path mainmodule, Annotations annotations,
            Path solutionDir, String test, String input, int timeout, int maxOutput)
            throws Exception {
        List<String> runargs = annotations.findKeys("ARGS");
        if (runargs.size() == 0) runargs.add("");
        String out = annotations.findUniqueKey("OUT");
        String[] outFiles = out == null ? new String[0] : out.trim().split("\\s+");
        
        
        String runNumber = test.replace("test", "").trim();
        report.run(runNumber.length() > 0 ? "Test " + runNumber : null);
        
        for (String args : runargs) {
            testInput(mainmodule, solutionDir, test, input, args, outFiles, timeout / runargs.size(), maxOutput / runargs.size());
        }
    }
    
    private void testInput(Path mainmodule,
            Path solutionDir, String test, String input, String runargs, String[] outFiles, int timeout, int maxOutput)
            throws Exception {
        
        // Before the run, clear any output files in case they existed, 
        // and recopy any supplied files (in case an input has been mutated by a previous run) 
        
        for (String f : outFiles) Files.deleteIfExists(workDir.resolve(f));           
        copySuppliedFiles(); 

        report.args(runargs);

        if (!language.echoesStdin()) report.input(input);

        // Run student program and capture stdout/err and output files

        String outerr = language.run(mainmodule, dependentModules, workDir, runargs, input, timeout, maxOutput);
        List<String> contents = new ArrayList<>();
        List<CompareImages> imageComp = new ArrayList<>();
        
        for (String f : outFiles) {
            Path p = workDir.resolve(f);
            if (CompareImages.isImage(f)) {
                try {
                    imageComp.add(new CompareImages(p));
                } catch (Exception ex) {
                    report.output(outerr);
                    throw ex;
                }
            }
            else
                contents.add(Util.read(p));            
        }
                
        if (solutionDir == null) { // Run without testing
            report.output(outerr);
            for (String f : outFiles) {
                if (CompareImages.isImage(f))          
                    report.image("Image", imageComp.remove(0).first());                    
                else
                    report.file(f, contents.remove(0));
            }
            // No score
        } else { // Run solution in the same way 
        
            for (String f : outFiles) Files.deleteIfExists(workDir.resolve(f));           
            copySuppliedFiles(); // Might have been deleted or mutated
        
            String expectedOuterr = language.run(mainmodule, dependentModules, solutionDir, runargs, input, timeout, maxOutput);
        
            // Report on results

            if (expectedOuterr != null && expectedOuterr.length() > 0) {
                boolean outcome = comp.execute(outerr, expectedOuterr, report, null);
                score.pass(outcome, report);
            }        
        
            for (String f : outFiles) {
                Path p = workDir.resolve(f);
                if (CompareImages.isImage(f)) {
                    CompareImages ic = imageComp.remove(0);
                    ic.setOtherImage(p);
                    boolean outcome = ic.getOutcome();
                    report.image("Image", ic.first());
                    if (!outcome) {
                        report.image("Expected", ic.other());
                        report.image("Mismatched pixels", ic.diff());
                    }
                    score.pass(outcome, report);                   
                } else {
                    String expectedContents = Util.read(p);                
                    boolean outcome = comp.execute(contents.remove(0),
                            expectedContents, report, f);
                    score.pass(outcome, report);
                }
            }
        }       
    }

    private void getMainAndDependentModules() {
        Set<Path> files = new TreeSet<>();
        files.addAll(studentFiles);
        files.addAll(solutionFiles);
        for (Path p : files) {
            Path fullPath = problemDir.resolve(p);
            Path tailPath = Util.tail(p);
            if (language.isMain(fullPath))
                mainModules.add(tailPath);
            else if (!mainModules.contains(tailPath) // Just in case that there is a student version without the main method declaration 
                    && language.isSource(fullPath) && !language.isUnitTest(fullPath))
                dependentModules.add(tailPath);
        }
    }

    private void getRequiredModules() {
        for (Path p : solutionFiles)
            if (language.isSource(p))
                requiredFiles.add(Util.tail(p));
    }
    
    private void runUnitTests() {
        List<Path> unitTests = new ArrayList<>();
        for (Path p : studentFiles) {
            if (language.isUnitTest(p)) 
                unitTests.add(p);
        }
        if (unitTests.size() > 0) {
            report.header("unitTest", "Unit Tests");
            for (Path p: unitTests) {
                language.runUnitTest(Util.tail(p), dependentModules, workDir, report, score, timeoutMillis / unitTests.size(), maxOutputLen / unitTests.size());            
            }
        }
    }

    private void doSubstitutions(Path submissionDir, Substitution sub) throws Exception {
        report.header("sub", "Running program with substitutions");
        Path mainmodule = Util.tail(sub.getFile());
        if (compile(mainmodule)) {
        	int n = sub.getSize();
        	String[] argNames = sub.names().toArray(new String[0]);
            String[][] args = new String[n][argNames.length];
            String[] actual = new String[n];
            String[] expected = new String[n];
            boolean[] outcomes = new boolean[n];

            int timeout = timeoutMillis / Math.max(1, sub.getSize());
            int maxOutput = maxOutputLen / Math.max(1, sub.getSize());
            for (int i = 0; i < sub.getSize(); i++) {
                sub.substitute(submissionDir.resolve(mainmodule),
                               workDir.resolve(mainmodule), i);
                if (compile(mainmodule)) {
                    actual[i] = language.run(mainmodule, dependentModules, workDir, null, null, timeout, maxOutput);
                    Path tempDir = compileSolution(mainmodule, sub, i);
                    expected[i] = language.run(mainmodule, dependentModules, tempDir, null, null, timeout, maxOutput);                    
                    Util.deleteDirectory(tempDir);
                    int j = 0;
                    for (String v : sub.values(i)) { args[i][j] = v; j++; }                      
                    outcomes[i] = comp.compare(actual[i], expected[i]);
                    actual[i] = Util.truncate(actual[i], expected[i].length() + MUCH_LONGER);
                    score.pass(outcomes[i], report);
                }
            }
            report.runTable(null, argNames, args, actual, expected, outcomes);
        } else {
            score.setInvalid();
        }
    }

    private void doCalls(Path submissionDir, Calls calls) throws Exception {
        
        report.header("call", "Calling with Arguments");
        List<Path> testModules = calls.writeTester(problemDir, workDir);
        
        String[] names = new String[calls.getSize()];
        String[][] args = new String[calls.getSize()][1];
        String[] actual = new String[calls.getSize()];
        String[] expected = new String[calls.getSize()];
        boolean[] outcomes = new boolean[calls.getSize()];

        int timeout = timeoutMillis / calls.getSize();
        int maxOutput = maxOutputLen  / calls.getSize();
        
        if (compile(testModules)) {
            for (int i = 0; i < calls.getSize(); i++) {
                Path mainModule = testModules.get(0);
                Set<Path> otherModules = new TreeSet<>(dependentModules);
                for (int j = 1; j < testModules.size(); j++) otherModules.add(testModules.get(j));
            	String result = language.run(mainModule, otherModules, workDir, "" + (i + 1), null, timeout, maxOutput);
            	Scanner in = new Scanner(result);
                List<String> lines = new ArrayList<>();
                while (in.hasNextLine()) lines.add(in.nextLine());
                in.close();
                Calls.Call call = calls.getCall(i);
                names[i] = call.name;
            	args[i][0] = call.args;
                if (lines.size() == 3 && Arrays.asList("true", "false").contains(lines.get(2))) {
                	expected[i] = lines.get(0);
                	actual[i] = Util.truncate(lines.get(1), expected[i].length() + MUCH_LONGER);
                	outcomes[i] = lines.get(2).equals("true");                
                } else {
                    // Error in compilation or execution
                        StringBuilder msg = new StringBuilder();
                        for (String line : lines) { msg.append(line); msg.append('\n'); }
                        String message = msg.toString(); 
                        report.errors(language.errors(message, true));
                                        
                	expected[i] = "";  
                	actual[i] = message;
                	outcomes[i] = false;
                }
            	score.pass(outcomes[i], report);
            }
            report.runTable(names, new String[] { "Arguments" }, args, actual, expected, outcomes);
        } else {
            score.setInvalid();
        }
    }

    public void run(String[] args) throws IOException {
        // TODO: Adjustable Timeouts
        long startTime = System.currentTimeMillis();
        String mode = args[0].trim();
        Path submissionDir = FileSystems.getDefault().getPath(args[1]);
        problemDir = FileSystems.getDefault().getPath(args[2]);
        
        Path homeDir = Util.getHomeDir();
        workDir = new File(".").getAbsoluteFile().toPath().normalize();
        System.setProperty("java.security.policy", homeDir.resolve("codecheck.policy").toString());
        System.setProperty("com.horstmann.codecheck.home", homeDir.toString());
        
        if (System.getProperty("com.horstmann.codecheck.debug") == null) 
            System.setSecurityManager(new SecurityManager()); 
    	   	
        if (System.getProperty("com.horstmann.codecheck.textreport") != null)
            report = new TextReport("Report", submissionDir);
        else if (System.getProperty("com.horstmann.codecheck.jsonreport") != null)
            report = new JSONReport("Report", submissionDir);
        else if (System.getProperty("com.horstmann.codecheck.njsreport") != null)
            report = new NJSReport("Report", submissionDir);
        else
            report = new HTMLReport("Report", submissionDir);
        int level = 0;
        try {
            level = Integer.parseInt(mode);
        } catch (NumberFormatException ex) {
        }

        if (mode.equals("check"))
            level = 1;
        else if (mode.equals("grade"))
            level = 9; // to pick up all
        if (Files.exists(problemDir.resolve("student")))
            studentDirectories.add("student");
        for (int n = 1; n <= level; n++)
            if (Files.exists(problemDir.resolve("student" + n)))
                studentDirectories.add("student" + n);

        if (mode.equals("grade")) { // mode grade will be rejected by web app
            if (Files.exists(problemDir.resolve("grader"))) {
                studentDirectories.add("grader");
            }
        }

        List<String> solutionDirectories = new ArrayList<>();
        if (Files.exists(problemDir.resolve("solution")))
            solutionDirectories.add("solution");
        for (int n = 1; n <= level; n++)
            if (Files.exists(problemDir.resolve("solution" + n)))
                solutionDirectories.add("solution" + n);

        if (studentDirectories.size() + solutionDirectories.size() == 0) {
            // new-style packaging with no student or solution directories
            studentDirectories.add(".");
        }
        
        
        String problemId = null;
        Annotations annotations = null;
                
        try {
            // Read check.properties in level dirs
            for (String levelDir : studentDirectories) {
                File modeCheckProperties = problemDir.resolve(levelDir).resolve("check.properties").toFile();
                if (modeCheckProperties.exists()) {
                    try (InputStream in = new FileInputStream(modeCheckProperties)) {
                        checkProperties.load(in);
                    }
                }
            }

            studentFiles = filterNot(Util.getDescendantFiles(problemDir, studentDirectories), "check.properties", ".*", "problem.ch", "problem.html");
            solutionFiles = filterNot(Util.getDescendantFiles(problemDir, solutionDirectories), "*.txt", ".*", "*.class");
            // TODO: Filtering out rubric
            // TODO: Unify with server/src/Problem.java
            
            // Determine language
            
            String languageName = System.getProperty("com.horstmann.codecheck.language");
            if (languageName != null) {
                try {
                    language = (Language) Class.forName(
                            languageName + "Language").newInstance();
                } catch (InstantiationException | IllegalAccessException
                        | ClassNotFoundException e) {
                    report.error("Cannot process language " + languageName);
                }
            } else {
                // Guess from solution and student files
                List<Path> files = new ArrayList<>();
                files.addAll(solutionFiles);
                files.addAll(studentFiles);
                for (int k = 0; language == null && k < languages.length; k++) {
                    if (languages[k].isLanguage(files)) 
                        language = languages[k];
                }
                if (language == null) throw new RuntimeException("Cannot find language from " + files);
            }

            annotations = new Annotations(language);
            annotations.read(problemDir, studentFiles, false);
            annotations.read(problemDir, solutionFiles, true);
            // Any student files with //SOLUTION must be moved to solution files
            Set<Path> annotatedSolutions = annotations.findSolutions();
            studentFiles.removeAll(annotatedSolutions);
            solutionFiles.addAll(annotatedSolutions);
            if (solutionFiles.size() == 0) 
                throw new RuntimeException("No solution file.");
          
            String uid = problemDir.getFileName().toString();
            report.comment("Submission", submissionDir.getFileName().toString());
            report.comment("Problem", uid);
            report.comment("Level", "" + level);
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
            df.setTimeZone(TimeZone.getTimeZone("UTC"));
            String currentTime = df.format(new Date());
            report.comment("Time", currentTime);
            report.footnote(currentTime);
            problemId = annotations.findUniqueKey("ID");
            if (problemId == null) {
                problemId = Util.removeExtension(solutionFiles.iterator().next().getFileName());            	
            }
            else {
            	problemId = problemId.replaceAll("[^A-Za-z0-9]", "").toLowerCase();
            }
        	
            report.comment("ID", problemId);
            
            // Used to pass in machine instance, git url into report 
            for (int iarg = 3; iarg < args.length; iarg++) {
            	String arg = args[iarg];
            	int keyEnd = arg.indexOf("=");
            	if (keyEnd >= 0) {
            	    report.comment(arg.substring(0, keyEnd), arg.substring(keyEnd + 1));
            	}
            	else {
            	    report.comment(arg, "");
            	}
            }
            
            
            
            timeoutMillis = DEFAULT_TIMEOUT_MILLIS;
            String timeoutProperty = System.getProperty("com.horstmann.codecheck.timeout");
            if (timeoutProperty != null)
            	timeoutMillis = Integer.parseInt(timeoutProperty);
            timeoutMillis = (int) annotations.findUniqueDoubleKey("TIMEOUT", timeoutMillis);
            
            maxOutputLen = DEFAULT_MAX_OUTPUT_LEN;
            String maxOutputLenProperty = System.getProperty("com.horstmann.codecheck.maxoutputlen");
            if (maxOutputLenProperty != null)
                maxOutputLen = Integer.parseInt(maxOutputLenProperty);
            maxOutputLen = (int) annotations.findUniqueDoubleKey("MAXOUTPUTLEN", maxOutputLen);
            
            double tolerance = annotations.findUniqueDoubleKey("TOLERANCE", DEFAULT_TOLERANCE);
            boolean ignoreCase = !"false".equals(annotations.findUniqueKey("IGNORECASE"));
            boolean ignoreSpace = !"false".equals(annotations.findUniqueKey("IGNORESPACE"));
            comp.setTolerance(tolerance);
            comp.setIgnoreCase(ignoreCase);
            comp.setIgnoreSpace(ignoreSpace);            
            
            getRequiredModules();
            getMainAndDependentModules();            
            
            annotations.check(report, requiredFiles);
                        
            for (String dir : studentDirectories) {
                for (Path p : Util.getDescendantFiles(problemDir.resolve(dir))) {
                    if (!requiredFiles.contains(p)) 
                        printFiles.add(p);
                }
            }

            copySuppliedFiles();
                
            Set<Path> missingModules = new TreeSet<>();
            for (Path file : requiredFiles) {
                Path source = submissionDir.resolve(file);
                if (Files.exists(source))
                    Files.copy(source, workDir.resolve(file), StandardCopyOption.REPLACE_EXISTING);
                else {
                    report.error("Missing file " + file);
                    missingModules.add(file);
                }
            }

            if (!annotations.checkConditions(workDir, report)) {
                // Do nothing
            } else if (getStringProperty("test.method") != null) { // Legacy
                callMethod(tolerance, ignoreCase, ignoreSpace);
            } else if (annotations.has("CALL"))
                doCalls(submissionDir, annotations.findCalls());
            else if (annotations.has("SUB"))
                doSubstitutions(submissionDir, annotations.findSubstitution());
            else {
                Map<String, String> inputs = new TreeMap<>();
                for (String i : new String[] { "", "1", "2", "3", "4", "5", "6", "7", "8", "9" }) {
                    String key = "test" + i + ".in";
                    String in = getStringProperty(key);
                    if (in == null)
                        in = Util.read(workDir.resolve(key));
                    else
                        in += "\n";
                    if (in != null)
                        inputs.put("test" + i, in);
                }
                int inIndex = inputs.size();
                for (String s : annotations.findKeys("IN")) {
                    if (!s.endsWith("\n")) s += "\n";
                    inputs.put("test" + ++inIndex, Util.unescapeJava(s));
                }

                runUnitTests();

                List<Path> testerModules = new ArrayList<>();
                List<Path> runModules = new ArrayList<>();
                for (Path mainmodule : mainModules) {
                    if (language.isTester(mainmodule)
                             && !annotations.isSample(mainmodule)
                             && !(containsModule(solutionFiles, mainmodule) && inputs.size() > 0)) // TODO: Legacy
                        testerModules.add(mainmodule);
                    else
                        runModules.add(mainmodule);
                }
                
                if (testerModules.size() > 0) {
                    report.header("tester", "Testers");
                    for (Path mainmodule : testerModules)
                        if (missingModules.contains(mainmodule)) {
                            report.error("Missing " + mainmodule);
                            score.setInvalid();
                        }
                        else
                            runTester(mainmodule, timeoutMillis / testerModules.size(), maxOutputLen / testerModules.size());
                }

                if (runModules.size() > 0) {
                    for (Path mainmodule : runModules)
                        if (missingModules.contains(mainmodule)) {
                            report.error("Missing " + mainmodule);
                            score.setInvalid();
                        }
                        else
                            testInputs(inputs, mainmodule, annotations);
                }
            }
            
            // Process checkstyle.xml etc.
            for (Path p : studentFiles) {
                if (language.accept(p, submissionDir, requiredFiles, report, score)) {
                    printFiles = filterNot(printFiles, p.getFileName().toString());
                }
            }

            if (System.getProperty("com.horstmann.codecheck.textreport") == null)
            {
	            report.header("studentFiles", "Student files");
	            for (Path file : requiredFiles)
	                report.file(submissionDir, file);
		
	            printFiles = filterNot(printFiles, "test*.in", "test*.out", "check.properties", "q.properties", "*.png", "*.PNG",
	                    "*.gif", "*.GIF", "*.jpg", "*.jpeg", "*.JPG", ".DS_Store", "*.jar", "*.class", "problem.ch", "problem.html");      

	            printFiles.removeAll(annotations.findHidden());
	
	            if (printFiles.size() > 0) {
	                copySuppliedFiles(); // Might have been mutated
	                report.header("providedFiles", "Provided files");
	                for (Path file : printFiles)
	                    report.file(workDir, file);
	            }
            }
        } catch (Throwable t) {
            report.systemError(t);
        } finally {
            if (annotations != null && !annotations.has("NOSCORE")) report.add(score);
            long endTime = System.currentTimeMillis();
            report.comment("Elapsed", (endTime - startTime) + " ms");
            report.save(problemId, "report");
        }
        System.exit(0);
    }

    private void callMethod(double tolerance, boolean ignoreCase,
            boolean ignoreSpace) throws IOException, Exception {
        String mainclass = getStringProperty("mainclass");
        Path mainModule = null;
        if (mainclass == null) {
            if (mainModules.size() == 1)
                mainModule = mainModules.iterator().next();
            else if (solutionFiles.size() == 1)
                mainModule = Util.tail(solutionFiles.iterator().next());
            else
                report.systemError("Can't identify main module");
            mainclass = Util.removeExtension(mainModule);
        } else mainModule = Paths.get(mainclass + ".java");

        CallMethod call = new CallMethod(mainclass, checkProperties, timeoutMillis);
        call.setTolerance(tolerance);
        call.setIgnoreCase(ignoreCase);
        call.setIgnoreSpace(ignoreSpace);
        if (compile(mainModule)) {
            report.header("callMethod", "Calling method");
            Path tempDir = compileSolution(mainModule, null, 0); 
            call.prepare(tempDir);
            call.run(workDir, report, score);
            Util.deleteDirectory(tempDir);
        } else
            score.setInvalid();
    }
}
