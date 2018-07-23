package com.horstmann.codecheck;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
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
    public static final int DEFAULT_TIMEOUT_MILLIS = 15000;
    public static final int DEFAULT_MAX_OUTPUT_LEN = 100_000;
    public static final String DEFAULT_TOKEN = "line";
    public static final int MUCH_LONGER = 1000; // if longer than the expected by this amount, truncate 
    public static boolean DEBUG = System.getProperty("com.horstmann.codecheck.debug") != null;
    
    private int timeoutMillis;
    private int maxOutputLen;
    private Path workDir;
    private Properties checkProperties;
    private Report report;
    private Path studentDir;
    private Path solutionDir;
    private Set<Path> useFiles = new TreeSet<>(); // relative to studentDir
        // the files (sources and inputs) that must be copied to the directory 
        // in which the program is run (workDir/tempDir)
    private Set<Path> solutionFiles = new TreeSet<>(); // relative to solutionDir/submissionDir
        // the source files that make up the solution
    private Set<Path> mainSourceFiles = new TreeSet<>(); // relative to workDir/tempDir
        // the source files that contain main
    private Set<Path> dependentSourceFiles = new TreeSet<>();
        // all other source files

    private boolean inputMode = false;
    
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
       new CSharpLanguage(),
       new HaskellLanguage()
    };

    /**
     * Entry point to program.
     *
     * @param args
     *            command-line arguments. args[0] = submission dir,
     *            args[1] = problem dir args[2] etc = metadata key=value pairs (optional)
     * @throws IOException
     * @throws ReflectiveOperationException 
     */
    public static void main(String[] args) throws IOException, ReflectiveOperationException {
        new Main().run(args);
    }

    public void copyUseFiles(Path targetDir) throws IOException {
        for (Path p : useFiles) {
            Path source = studentDir.resolve(p);
            Path target = targetDir.resolve(p);
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        }
    }

    public boolean compile(Path mainModule) {
        return compile(Collections.singletonList(mainModule));
    }

    /**
     * Runs the compiler
     * @param sourceFiles a list of paths, starting with the path to the main source file
     * @return true if compilation succeeds
     */
    public boolean compile(List<Path> sourceFiles) {
        List<Path> allSourceFiles = new ArrayList<>();
        allSourceFiles.addAll(sourceFiles);
        allSourceFiles.addAll(dependentSourceFiles);
        String errorReport = language.compile(allSourceFiles, workDir);
        if (errorReport == null) return true;        
        if (errorReport.trim().equals(""))
            report.error("Error compiling " + sourceFiles.get(0));
        else {
            report.error(errorReport);
            report.errors(language.errors(errorReport, true));
        }
        return false;
    }

    public Path compileSolution(Path mainSourceFile, Substitution sub, int n) throws IOException {
        // Delete any class files in the work directory
        // Otherwise they won't be generated in the solution directory
        for (Path p : Files.list(workDir).filter(p -> p.getFileName().toString().endsWith(".class")).toArray(Path[]::new))
            Files.deleteIfExists(p);
        
        Path tempDir = Util.createTempDirectory();
        copyUseFiles(tempDir);
        for (Path p : solutionFiles) {
            Path source = solutionDir.resolve(p);
            Path target = tempDir.resolve(p);
            if (sub != null && sub.getFile().equals(p))
                sub.substitute(source, target, n);
            else
                Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        }
        List<Path> sourceFiles = new ArrayList<>();
        sourceFiles.add(mainSourceFile);
        sourceFiles.addAll(dependentSourceFiles);        
        String errorReport = language.compile(sourceFiles, tempDir);
        if (errorReport == null) return tempDir;
        else {
            report.systemError("Error compiling " + sourceFiles + "\n" + errorReport);
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
            String outerr = language.run(mainmodule, dependentSourceFiles, workDir, "", null, timeout, maxOutputLen);
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
        report.header("run", inputMode ? "Output" : "Testing " + mainmodule);
        Path solutionDir = null;

        if (!inputMode && !annotations.isSample(mainmodule)) {
            solutionDir = compileSolution(mainmodule, null, 0);
            if (solutionDir == null) return;
        }        
        
        if (compile(mainmodule)) {
            for (String test : inputs.keySet()) {
                String input = inputs.get(test);
                if (!input.endsWith("\n")) input += "\n";

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
        report.run(!test.equals("Input") && runNumber.length() > 0 ? "Test " + runNumber : null);
        
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
        copyUseFiles(workDir); 

        report.args(runargs);

        if (!language.echoesStdin() && !test.equals("Input")) report.input(input);

        // Run student program and capture stdout/err and output files

        String outerr = language.run(mainmodule, dependentSourceFiles, workDir, runargs, input, timeout, maxOutput);
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
            copyUseFiles(workDir); // Might have been deleted or mutated
        
            String expectedOuterr = language.run(mainmodule, dependentSourceFiles, solutionDir, runargs, input, timeout, maxOutput);
        
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
        for (Path p : solutionFiles) {
            Path fullPath = solutionDir.resolve(p);
            if (language.isMain(fullPath))
                mainSourceFiles.add(p);
            else if (language.isSource(fullPath) && !language.isUnitTest(fullPath))
                dependentSourceFiles.add(p);
        }
        
        for (Path p : useFiles) {
            Path fullPath = studentDir.resolve(p);
            if (language.isMain(fullPath))
                mainSourceFiles.add(p);
            else if (language.isSource(fullPath) && !language.isUnitTest(fullPath))
                dependentSourceFiles.add(p);
        }
    }

    private void runUnitTests() {
        List<Path> unitTests = new ArrayList<>();
        for (Path p : useFiles) {
            if (language.isUnitTest(p)) 
                unitTests.add(p);
        }
        if (unitTests.size() > 0) {
            report.header("unitTest", "Unit Tests");
            for (Path p: unitTests) {
                language.runUnitTest(p, dependentSourceFiles, workDir, report, score, timeoutMillis / unitTests.size(), maxOutputLen / unitTests.size());            
            }
        }
    }

    private void doSubstitutions(Path submissionDir, Substitution sub) throws Exception {
        report.header("sub", "Running program with substitutions");
        Path mainmodule = sub.getFile();
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
                    actual[i] = language.run(mainmodule, dependentSourceFiles, workDir, null, null, timeout, maxOutput);
                    Path tempDir = compileSolution(mainmodule, sub, i);
                    expected[i] = language.run(mainmodule, dependentSourceFiles, tempDir, null, null, timeout, maxOutput);                    
                    Util.deleteDirectory(tempDir);
                    int j = 0;
                    for (String v : sub.values(i)) { args[i][j] = v; j++; }
                    outcomes[i] = comp.compare(actual[i], expected[i]).matches;
                    actual[i] = Util.truncate(actual[i], expected[i].length() + MUCH_LONGER);
                    score.pass(outcomes[i], null); // Pass/fail shown in run table
                }
            }
            report.runTable(null, argNames, args, actual, expected, outcomes);
        } else {
            score.setInvalid();
        }
    }

    private void doCalls(Path submissionDir, Calls calls) throws Exception {        
        report.header("call", "Calling with Arguments");
        List<Path> testModules = calls.writeTester(solutionDir, workDir);
        
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
                Set<Path> otherModules = new TreeSet<>(dependentSourceFiles);
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
            	score.pass(outcomes[i], null /* no report--it's in the table */);
            }
            report.runTable(names, new String[] { "Arguments" }, args, actual, expected, outcomes);
        } else {
            score.setInvalid();
        }
    }

    private void callMethod(double tolerance, boolean ignoreCase,
            boolean ignoreSpace) throws IOException, Exception {
        String mainclass = getStringProperty("mainclass");
        Path mainModule = null;
        if (mainclass == null) {
            if (mainSourceFiles.size() == 1)
                mainModule = mainSourceFiles.iterator().next();
            else if (solutionFiles.size() == 1)
                mainModule = solutionFiles.iterator().next();
            else
                report.systemError("Can't identify main module");
            mainclass = Util.removeExtension(mainModule);
        } else mainModule = Paths.get(mainclass + ".java");

        CallMethod call = new CallMethod(mainclass, checkProperties, timeoutMillis);
        call.setTolerance(tolerance);
        call.setIgnoreCase(ignoreCase);
        call.setIgnoreSpace(ignoreSpace);
        Path tempDir = compileSolution(mainModule, null, 0); 
        if (compile(mainModule)) {
            report.header("callMethod", "Calling method");
            call.prepare(tempDir);
            call.run(workDir, report, score);
        } else
            score.setInvalid();
        Util.deleteDirectory(tempDir);
    }

    public void run(String[] args) throws IOException, ReflectiveOperationException {
        Annotations annotations = null;
        String problemId = "";
        // TODO: Adjustable Timeouts
        long startTime = System.currentTimeMillis();
        try {
            Path submissionDir = FileSystems.getDefault().getPath(args[0]);
            Path problemDir = FileSystems.getDefault().getPath(args[1]);
            Path homeDir = Util.getHomeDir();
            workDir = Paths.get(".").toAbsolutePath().normalize();
            System.setProperty("java.security.policy", homeDir.resolve("codecheck.policy").toString());
            System.setProperty("com.horstmann.codecheck.home", homeDir.toString());
            
            // if (!DEBUG) // TODO: Why not in debug mode? 
                System.setSecurityManager(new SecurityManager());

            String reportType = System.getProperty("com.horstmann.codecheck.report");
            if (reportType != null) {
                Class<?> reportClass = Class.forName("com.horstmann.codecheck." + reportType + "Report");
                report = (Report) reportClass.getConstructor(String.class, Path.class).newInstance("Report", submissionDir);            
            }
            else if (System.getProperty("com.horstmann.codecheck.textreport") != null) // TODO: Legacy
                report = new TextReport("Report", submissionDir);
            else if (System.getProperty("com.horstmann.codecheck.jsonreport") != null)
                report = new JSONReport("Report", submissionDir);
            else if (System.getProperty("com.horstmann.codecheck.njsreport") != null)
                report = new NJSReport("Report", submissionDir);
            else if (System.getProperty("com.horstmann.codecheck.codioreport") != null)
                report = new CodioReport("Report", submissionDir);
            else
                report = new HTMLReport("Report", submissionDir);
                    
            // Determine language
            
            String languageName = System.getProperty("com.horstmann.codecheck.language");
            if (languageName != null) {
                try {
                    language = (Language) Class.forName(
                            languageName + "Language").getConstructor().newInstance();
                } catch (InstantiationException | IllegalAccessException
                        | ClassNotFoundException e) {
                    report.error("Cannot process language " + languageName);
                }
            } else {
                // Guess from solution and student files
                Set<Path> files = Util.getDescendantFiles(problemDir);
                for (int k = 0; language == null && k < languages.length; k++) {
                    if (languages[k].isLanguage(files)) 
                        language = languages[k];
                }
                if (language == null) throw new CodeCheckException("Cannot find language from " + files);
            }

            studentDir = problemDir.resolve("student"); 
                    
            checkProperties = new Properties();
            annotations = new Annotations(language);
            
            if (Files.exists(studentDir)) { // old style with student and solution directories
                Path checkPropertiesPath = studentDir.resolve("check.properties");
                if (Files.exists(checkPropertiesPath)) {
                    try (InputStream in = Files.newInputStream(checkPropertiesPath)) {
                        checkProperties.load(in);
                    }
                }

                useFiles = Util.filterNot(Util.getDescendantFiles(studentDir), 
                        ".*", "*~", "check.properties");
                solutionDir = problemDir.resolve("solution");
                solutionFiles = Util.filterNot(Util.getDescendantFiles(solutionDir), 
                        ".*", "*~");
                annotations.read(studentDir, useFiles, solutionDir, solutionFiles, report);
                useFiles.removeAll(solutionFiles);
            } else {
                studentDir = problemDir;
                solutionDir = problemDir;
                useFiles = Util.filterNot(Util.getDescendantFiles(studentDir), 
                        ".*", "*~", "*.class", "a.out", 
                        "index.html", "index.ch", "problem.html", 
                        "*.in", "q.properties", "check.properties", "param.js");
                solutionFiles = new TreeSet<Path>();
                annotations.read(studentDir, useFiles, solutionDir, solutionFiles, report);
                // Move any files annotated with SOLUTION, SHOW or EDIT to solution 
                Set<Path> annotatedSolutions = annotations.getSolutions();
                useFiles.removeAll(annotatedSolutions);
                solutionFiles.addAll(annotatedSolutions);

                inputMode = Files.exists(submissionDir.resolve("Input"));
                if (inputMode) {
                    Iterator<Path> iter = useFiles.iterator();
                    while (iter.hasNext()) {
                        Path p = iter.next();
                        if (language.isSource(studentDir.resolve(p)) && !annotations.getHidden().contains(p)) {
                            solutionFiles.add(p);
                            iter.remove();                            
                        }
                    }
                    useFiles.remove(Paths.get("Input"));
                }                 
            }

            if (solutionFiles.isEmpty()) {                                   
                throw new CodeCheckException("No solution files found");
            }
            
            report.comment("Submission", submissionDir.toString());
               // This is just a unique ID, can be used to check against cheating
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
            for (int iarg = 2; iarg < args.length; iarg++) {
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
            boolean ignoreCase = !"false".equalsIgnoreCase(annotations.findUniqueKey("IGNORECASE"));
            boolean ignoreSpace = !"false".equalsIgnoreCase(annotations.findUniqueKey("IGNORESPACE"));
            comp.setTolerance(tolerance);
            comp.setIgnoreCase(ignoreCase);
            comp.setIgnoreSpace(ignoreSpace);            
            
            getMainAndDependentModules();            
                        
            copyUseFiles(workDir);
                
            Set<Path> missingFiles = new TreeSet<>();
            for (Path file : solutionFiles) {
                Path source = submissionDir.resolve(file);
                if (Files.exists(source))
                    Files.copy(source, workDir.resolve(file), StandardCopyOption.REPLACE_EXISTING);
                else {
                    report.error("Missing file " + file);
                    missingFiles.add(file); // TODO: How can this happen? Why not abort?
                }
            }
            
            // the supplied files that the students are entitled to see
            Set<Path> printFiles = Util.filterNot(useFiles, "test*.in", "test*.out", "Input", 
                    "*.png", "*.PNG", "*.gif", "*.GIF", "*.jpg", "*.jpeg", "*.JPG", 
                    "*.jar");      

            printFiles.removeAll(annotations.getHidden()); 
            
            if (annotations.checkConditions(submissionDir, report)) {
                if (getStringProperty("test.method") != null) // TODO: Legacy
                    callMethod(tolerance, ignoreCase, ignoreSpace);
                if (annotations.has("CALL"))
                    doCalls(submissionDir, annotations.findCalls());
                if (annotations.has("SUB")) {
                    Substitution sub = annotations.findSubstitution();
                    doSubstitutions(submissionDir, sub);
                    mainSourceFiles.remove(sub.getFile());
                }
                
                Map<String, String> inputs = new TreeMap<>(); // TODO: Legacy
                for (String i : new String[] { "", "1", "2", "3", "4", "5", "6", "7", "8", "9" }) {
                    String key = "test" + i + ".in";
                    String in = getStringProperty(key);
                    if (in == null)
                        in = Util.read(studentDir.resolve(key));
                    else
                        in += "\n";
                    if (in != null)
                        inputs.put("test" + i, in);
                }
                int inIndex = inputs.size();
                for (String s : annotations.findKeys("IN")) {
                    inputs.put("test" + ++inIndex, Util.unescapeJava(s));
                }
                if (inputMode) { 
                    Path runInput = submissionDir.resolve("Input");
                    inputs.put("Input", Util.read(runInput));
                }

                runUnitTests();
    
                List<Path> testerModules = new ArrayList<>();
                List<Path> runModules = new ArrayList<>();
                for (Path mainmodule : mainSourceFiles) {
                    if (language.isTester(mainmodule)
                             && !annotations.isSample(mainmodule) && !inputMode)
                        testerModules.add(mainmodule);
                    else
                        runModules.add(mainmodule);
                }
                
                if (testerModules.size() > 0) {
                    report.header("tester", "Testers");
                    for (Path mainmodule : testerModules)
                        if (missingFiles.contains(mainmodule)) {
                            report.error("Missing " + mainmodule);
                            score.setInvalid();
                        }
                        else
                            runTester(mainmodule, timeoutMillis / testerModules.size(), maxOutputLen / testerModules.size());
                }

                if (runModules.size() > 0) {
                    for (Path mainmodule : runModules)
                        if (missingFiles.contains(mainmodule)) {
                            report.error("Missing " + mainmodule);
                            score.setInvalid();
                        }
                        else
                            testInputs(inputs, mainmodule, annotations);
                }
                // Process checkstyle.xml etc.
                for (Path p : useFiles) {
                    if (language.accept(p, submissionDir, report, score)) {
                        printFiles.remove(p);
                    }
                }
            }
            
            if (!inputMode) { // Don't print student or provided files for run-only mode
                report.header("studentFiles", "Submitted files");
                
                for (Path file : solutionFiles) 
                    report.file(submissionDir, file);
    		
                if (printFiles.size() > 0) {
                    report.header("providedFiles", "Provided files");
                    for (Path file : printFiles)
                        report.file(studentDir, file);
                }
            }
        } catch (Throwable t) {
            report.systemError(t);
        } finally {
            if (annotations != null && !annotations.has("NOSCORE") 
                    && !inputMode) 
                report.add(score);
            long endTime = System.currentTimeMillis();
            report.comment("Elapsed", (endTime - startTime) + " ms");
            report.save(problemId, "report");
        }
        System.exit(0);
    }
}
