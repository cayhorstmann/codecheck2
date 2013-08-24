package com.horstmann.codecheck;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

/*
 * Run this program from the "work directory", the directory in which all compilation and
 * execution happens. The submission directory and problem directory are left untouched.
 *
 */

public class Main {
    public static final double DEFAULT_TOLERANCE = 1.0E-6;
    public static final int DEFAULT_TIMEOUT_MILLIS = 30000;
    public static final String DEFAULT_TOKEN = "line";
    
    private int timeoutMillis;
    private Path workDir;
    private Properties checkProperties = new Properties();
    private Report report;
    private Path problemDir;
    private Set<Path> studentFiles;
    private Set<Path> requiredFiles = new TreeSet<>(); // tails only
    private Set<Path> solutionFiles;
    private Set<Path> printFiles = new TreeSet<>();
    private Set<String> mainclasses = new TreeSet<String>(); // class names
    private SecurityManager securityManager = new StudentSecurityManager();
    private Score score = new Score();
    private Comparison comp = new Comparison();

    /**
     * Entry point to program.
     *
     * @param args
     *            command-line arguments. args[0] = level (1, 2, 3, ..., or
     *            check/grade for compatibility) args[1] = submission dir,
     *            args[2] = problem dir args[3] = report title (optional)
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        new Main().run(args);
    }

    public static boolean matches(Path path, String glob) {
        PathMatcher matcher = FileSystems.getDefault().getPathMatcher(
                                  "glob:" + glob.replace("/", FileSystems.getDefault().getSeparator()));
        return matcher.matches(path);
    }

    public static Set<Path> filter(Set<Path> paths, String glob) {
        Set<Path> result = new TreeSet<>();
        for (Path p : paths)
            if (matches(p.getFileName(), glob))
                result.add(p);
        return result;
    }

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

    public void getGradingFiles(Path dir) throws IOException {
        if (dir == null)
            return;
        for (Path p : Util.getDescendantFiles(dir)) {
            Path source = dir.resolve(p);
            Path target = workDir.resolve(p);
            // Copy if it is not required to be submitted
            if (!requiredFiles.contains(p) && !"check.properties".equals(p.toString())) {
                Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                printFiles.add(p);
            }
        }
    }

    public boolean containsClass(Set<Path> files, String classname) {
        for (Path file : files)
            if (Util.javaClass(Util.tail(file)).equals(classname))
                return true;
        return false;
    }

    public boolean compile(String classname) {
        return compile(classname, workDir);
    }

    public Path compileSolution(String classname, Substitution sub, int n) throws IOException {
        Path tempDir = Files.createTempDirectory("labrat");
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
        boolean result = compile(classname, tempDir);
        return result ? tempDir : null;
    }

    public boolean compile(String classname, Path dir) {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        OutputStream outStream = new ByteArrayOutputStream();
        OutputStream errStream = new ByteArrayOutputStream();
        int result = compiler.run(null, outStream, errStream, "-sourcepath", dir.toString(),
                                  "-d", dir.toString(), dir.resolve(Util.javaPath(classname)).toString());
        if (result != 0) {
            String errorReport = errStream.toString();
            if (errorReport.trim().equals(""))
                report.output(null, "Error compiling " + classname);
            else
                report.error("Compiler error", errorReport);
        }
        return result == 0;
    }

    /**
     * Runs a Java program
     *
     * @param mainclass
     * @param classpathDir
     * @param input
     * @return an array of two strings, holding the program output and errors
     * @throws IOException
     * @throws ReflectiveOperationException
     */
    @SuppressWarnings("deprecation")
    public String runJavaProgram(final String mainclass, final Path classpathDir, String args, String input, int timeoutMillis)
    throws IOException, ReflectiveOperationException {
        InputStream oldIn = System.in;
        PrintStream oldOut = System.out;
        PrintStream oldErr = System.err;
        if (input == null)
            input = "";
        final ByteArrayOutputStream newOut = new ByteArrayOutputStream();
        final PrintStream newOutPrint = new PrintStream(newOut);
        System.setIn(new ByteArrayInputStream(input.getBytes("UTF-8")) {
            public int available() 
            {
               return 0;
            }
            public int read()  
            {
               int c = super.read();
               if (c != -1) 
               { 
                  newOut.write((char) c); 
               }
               return c;
            }
            public int read(byte[] b)
            {
               return read(b, 0, b.length);
            }
            public int read(byte[] b, int off, int len)
            {
               // int r = super.read(b, off, len);
               if (len == 0 || off >= b.length) return 0;
               int r = 0;            
               int c = super.read();
               if (c == -1) return -1;
               boolean done = false;
               while (!done)
               {
                  b[off + r] = (byte) c;  
                  r++;                  
                  if (c == '\n') done = true;
                  else 
                  {  
                     c = super.read();
                     if (c == -1) done = true;
                  }
               }            
               if (r != -1) 
               { 
                  newOut.write(b, off, r);
               }
               return r;
            }        	
        });
        
        String result = "";
        System.setOut(newOutPrint);
        System.setErr(newOutPrint);
        System.setSecurityManager(securityManager);
        final URLClassLoader loader = new URLClassLoader(new URL[] { classpathDir.toFile().toURI().toURL() });
        try {

            final AtomicBoolean done = new AtomicBoolean(false);

            final String[] argsArray;
            if (args == null || args.trim().equals(""))
                argsArray = new String[0];
            else
                argsArray = args.trim().split("\\s+");
            loader.setDefaultAssertionStatus(true);

            final Thread mainmethodThread = new Thread() {
                public void run() {
                    try {
                        Class<?> klass = loader.loadClass(mainclass);
                        final Method mainmethod = klass.getMethod("main", String[].class);
                        mainmethod.invoke(null, (Object) argsArray);
                    } catch (InvocationTargetException ex) {
                        Throwable cause = ex.getCause();
                        if (cause instanceof StudentSecurityManager.ExitException) {
                            // do nothing
                        } else if (cause == null)
                            ex.printStackTrace(newOutPrint);
                        else
                            cause.printStackTrace(newOutPrint);
                    } catch (Throwable t) {
                        t.printStackTrace(newOutPrint);
                    }
                    done.set(true);
                }
            };

            mainmethodThread.start();

            try {
                mainmethodThread.join(timeoutMillis);
            } catch (InterruptedException e) {
            }
            result = newOut.toString("UTF-8");
            if (!done.get()) {
            	if (!result.endsWith("\n")) result += "\n";
                result += "Timed out after " +
                		(timeoutMillis >= 2000 ? timeoutMillis / 1000 + " seconds" 
                				: timeoutMillis + " milliseconds");
                mainmethodThread.stop();
            }
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
            System.setErr(oldErr);
            System.setSecurityManager(null);
            loader.close();
        }
        return result;
    }

    boolean getBooleanProperty(String key, boolean defaultValue) {
        String value = checkProperties.getProperty(key);
        if (value == null)
            return defaultValue;
        return value.toLowerCase().equals("true");
    }

    double getDoubleProperty(String key, double defaultValue) {
        String value = checkProperties.getProperty(key);
        if (value != null)
            try {
                return Double.parseDouble(value.trim());
            } catch (NumberFormatException ex) {
            }
        return defaultValue;
    }

    double getDoubleProperty(String key, String key2, double defaultValue) {
        String value = checkProperties.getProperty(key);
        if (value != null)
            try {
                return Double.parseDouble(value.trim());
            } catch (NumberFormatException ex) {
            }
        return getDoubleProperty(key2, defaultValue);
    }

    double getDoubleProperty(String key, String key2, String key3, double defaultValue) {
        String value = checkProperties.getProperty(key);
        if (value != null)
            try {
                return Double.parseDouble(value.trim());
            } catch (NumberFormatException ex) {
            }
        return getDoubleProperty(key2, key3, defaultValue);
    }

    String getStringProperty(String key, String... fallbackKeysAndDefaultValue) {
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

    private void snap(String mainclass) throws Exception {
        report.header("Screen capture of " + mainclass);
        if (compile(mainclass)) {
            String[] progArgs = getStringProperty("args", "test.snap.args", "test.args", "").split("\\s+");
            System.setSecurityManager(securityManager);
            String result = "";
            try {
                try {
                    result = SnapFrame.run(mainclass, mainclass + ".png",
                                           getStringProperty("test.snap.keys", "test.keys", ""), progArgs, workDir, false);
                } finally {
                    System.setSecurityManager(null);
                }
            } catch (Exception ex) {
                if (!(ex instanceof StudentSecurityManager.ExitException))
                    throw ex;
            }
            if (result.length() > 0)
                report.output(result);
            report.image(workDir.resolve(mainclass + ".png"));
        }
    }

    private boolean isTester(String classname) {
    	return classname != null && classname.matches(".*Tester[0-9]*");
    }
    
    private void runTester(String mainclass) throws IOException, UnsupportedEncodingException,
        ReflectiveOperationException {
        report.header("Running " + mainclass);
        // TODO: Assume testers always in default package?
        
        // TODO: Scoring doesn't work when outerr contains an exception report because we don't know how many
        // test cases have not occurred. 
        // May need to count the number of expected cases in the 
        
        if (compile(mainclass)) {
            String outerr = runJavaProgram(mainclass, workDir, "", null, timeoutMillis);
            AsExpected cond = new AsExpected(comp);
            cond.eval(outerr, report, score, workDir.resolve(Util.javaPath(mainclass)));
        }
    }

    private void testInputs(Map<String, String> inputs, String mainclass, Annotations annotations) throws UnsupportedEncodingException,
        IOException, ReflectiveOperationException {
        Path tempDir = null;
        /*
         * If there are no inputs, we feed in one empty input, or the legacy test.run.inputstring
         */
        if (inputs.size() == 0)
            inputs.put("", getStringProperty("test.run.inputstring")); // Legacy
        report.header("Testing " + mainclass);
        if (compile(mainclass)) {
        	int timeout = timeoutMillis / inputs.size();
            for (String test : inputs.keySet()) {
                String input = inputs.get(test);
                // TODO: Might be useful to have more than one set of ARGS
                String runargs = annotations.findUniqueKey("ARGS");
                if (runargs == null)
                    runargs = getStringProperty(test + ".args", "args", "test.args", "test.run.args",
                                                "test.test-inputs.args", "");
                String outerr = runJavaProgram(mainclass, workDir, runargs, input, timeout);
                /*               
                if (input != null && input.length() > 1)
                    report.output("Input " + test, input);
                */
                String testExpectedFile = annotations.findUniqueKey("OUT");
                if (testExpectedFile == null)
                    testExpectedFile = getStringProperty(test + ".outputfile", (test.length() > 0 ? test
                                                         : "test.run") + ".expectedfile", "outputfile", "test.expectedfile", null); // Legacy
                String contents = null;
                String expectedContents = null;
                String expectedOuterr = null;
                CompareImages imageComp = null;
                Path testExpectedPath = testExpectedFile == null ? null : workDir.resolve(testExpectedFile);

                if (testExpectedFile != null) {
                    if (CompareImages.isImage(testExpectedFile)) {
                        imageComp = new CompareImages(testExpectedPath);
                        report.image(testExpectedPath);
                    } else
                        contents = Util.read(testExpectedPath);
                }

            	String title = "Program run";
            	if (test != null) title = (title + " " + test.replace("test", "")).trim(); 
                if (annotations.isSample(mainclass) || "true".equals(getStringProperty("test.run"))) { // Run without testing
                    report.output(title, outerr);
                }
                else {
                    // Make output from solution
                    if (tempDir == null)
                        tempDir = compileSolution(mainclass, null, 0);
                    if (tempDir != null) {
                        if (testExpectedFile != null)
                            Files.delete(workDir.resolve(testExpectedFile));
                        expectedOuterr = runJavaProgram(mainclass, tempDir, runargs, input, timeout);
                        if (testExpectedFile != null) {
                            if (imageComp == null)
                                expectedContents = Util.read(testExpectedPath);
                        }
                    }

                    if (imageComp != null) {
                        imageComp.setOtherImage(testExpectedPath);
                        boolean outcome = imageComp.getOutcome();
                        if (!outcome) {
                            report.image("Expected", testExpectedPath);
                            report.image("Mismatched pixels", imageComp.diff());
                        }
                        score.pass(outcome, report);
                    } else if (testExpectedFile != null) {
                        report.header(testExpectedFile);
                        boolean outcome = comp.execute(contents, expectedContents, report, title);
                        score.pass(outcome, report);
                    }

                    if (expectedOuterr != null && expectedOuterr.length() > 0) {
                        boolean outcome = comp.execute(outerr, expectedOuterr, report, title);
                        score.pass(outcome, report);
                    }
                }
        }
        }
    }

    private void getMainClasses() {
        Set<Path> files = new TreeSet<>();
        files.addAll(studentFiles);
        if (solutionFiles.size() > 0) {
            files.addAll(solutionFiles);
            for (Path p : files) {
                String c = Util.javaClass(Util.tail(p));
                if (c != null && Util.isMain(problemDir, p))
                    mainclasses.add(c);
            }

        } else { // Legacy
            String mainclassProperty = getStringProperty("mainclass");

            if (mainclassProperty != null) {
                mainclasses.add(mainclassProperty);
                if (!getBooleanProperty("mainclass.required", true))
                    requiredFiles.remove(Util.javaPath(mainclassProperty));
            }

            for (Path p : files) {
                String c = Util.javaClass(Util.tail(p));
                if (isTester(c))
                    mainclasses.add(c);
            }
        }
    }

    private void getRequiredClasses() {
        if (solutionFiles.size() > 0) {
            for (Path p : solutionFiles)
                if (p.toString().endsWith(".java"))
                    requiredFiles.add(Util.tail(p));
        } else { // Legacy
            String requiredclasses = getStringProperty("requiredclasses");
            if (requiredclasses != null)
                for (String f : requiredclasses.trim().split("\\s*,\\s*"))
                    requiredFiles.add(Util.javaPath(f));
        }
    }

    private void doSubstitutions(Path submissionDir, Substitution sub) throws IOException, ReflectiveOperationException {
        report.header("Running program with substitutions");
        Path p = Util.tail(sub.getFile());
        String mainclass = Util.javaClass(p);
        if (compile(mainclass)) {
        	int n = sub.getSize();
        	String[] argNames = sub.names().toArray(new String[0]);
            String[][] args = new String[n][argNames.length];
            String[] actual = new String[n];
            String[] expected = new String[n];
            boolean[] outcomes = new boolean[n];

            int timeout = timeoutMillis / Math.max(1, sub.getSize()); 
            for (int i = 0; i < sub.getSize(); i++) {
                sub.substitute(submissionDir.resolve(p),
                               workDir.resolve(p), i);
                if (compile(mainclass)) {
                    actual[i] = runJavaProgram(mainclass, workDir, null, null, timeout);
                    Path tempDir = compileSolution(mainclass, sub, i);
                    expected[i] = runJavaProgram(mainclass, tempDir, null, null, timeout);                    
                    
                    int j = 0;
                    for (String v : sub.values(i)) { args[i][j] = v; j++; }                      
                    outcomes[i] = comp.compare(actual[i], expected[i]);
                    score.pass(outcomes[i]);
                }
            }
            report.runTable(argNames, args, actual, expected, outcomes);
        }
    }

    private void doCalls(Path submissionDir, Calls calls) throws IOException, ReflectiveOperationException {
        report.header("Testing method " + calls.getName());
        Path p = Util.tail(calls.getFile());
        String mainclass = Util.javaClass(p) + "CodeCheck";
        calls.writeTester(problemDir, workDir);
        
        String[][] args = new String[calls.getSize()][1];
        String[] actual = new String[calls.getSize()];
        String[] expected = new String[calls.getSize()];
        boolean[] outcomes = new boolean[calls.getSize()];

        int timeout = timeoutMillis / calls.getSize();
        
        if (compile(mainclass)) {
            for (int i = 0; i < calls.getSize(); i++) {
            	String result = runJavaProgram(mainclass, workDir, "" + (i + 1), null, timeout);
            	System.out.println("result=" + result);
                Scanner in = new Scanner(result);
                List<String> lines = new ArrayList<>();
                while (in.hasNextLine()) lines.add(in.nextLine());
                in.close();
            	args[i][0] = calls.getArgs(i);
                if (lines.size() == 3 && Arrays.asList("true", "false").contains(lines.get(2))) {
                	expected[i] = lines.get(0);
                	actual[i] = lines.get(1);
                	outcomes[i] = lines.get(2).equals("true");                
                } else {
                	expected[i] = lines.size() > 0 ? lines.get(0) : "???";  
                	actual[i] = lines.size() > 1 ? lines.get(1) : "???";
                	outcomes[i] = false;
                }
            	score.pass(outcomes[i]);
            }
            report.runTable(new String[] { "Arguments" }, args, actual, expected, outcomes);
        }
    }

    public void run(String[] args) throws IOException {
        // TODO: Adjustable Timeouts


        // TODO: What if args[0], args[1] don't exist?

        String mode = args[0].trim();
        Path submissionDir = FileSystems.getDefault().getPath(args[1]);
        problemDir = FileSystems.getDefault().getPath(args[2]);
        if (System.getProperty("com.horstmann.codecheck.textreport") != null)
        	report = new TextReport(args.length >= 4 ? args[3] : "Report", submissionDir);
        else
        	report = new HTMLReport(args.length >= 4 ? args[3] : "Report", submissionDir);
        int level = 0;
        try {
            level = Integer.parseInt(mode);
        } catch (NumberFormatException ex) {
        }

        if (mode.equals("check"))
            level = 1;
        else if (mode.equals("grade"))
            level = 9; // to pick up all
        List<String> studentDirectories = new ArrayList<>();
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

        workDir = new File(".").getAbsoluteFile().toPath().normalize();
        
        String problemId = null;
                
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

            studentFiles = filterNot(Util.getDescendantFiles(problemDir, studentDirectories), "check.properties", ".DS_Store");
            solutionFiles = filterNot(Util.getDescendantFiles(problemDir, solutionDirectories), "*.txt", ".DS_Store");
            // Filtering out rubric

            Annotations annotations = new Annotations();
            annotations.read(problemDir, studentFiles);
            annotations.read(problemDir, solutionFiles);

            String uid = problemDir.getFileName().toString();
            report.comment("UID: " + uid);
            problemId = annotations.findUniqueKey("ID").replaceAll("[^A-Za-z0-9]", "").toLowerCase();
            if (problemId == null) problemId = uid;
            else report.comment("ID: " + problemId);
            timeoutMillis = DEFAULT_TIMEOUT_MILLIS;
            String timeoutProperty = System.getProperty("com.horstmann.codecheck.timeout");
            if (timeoutProperty != null)
            	timeoutMillis = Integer.parseInt(timeoutProperty);
            timeoutMillis = (int) annotations.findUniqueDoubleKey("TIMEOUT", timeoutMillis);
            
            double tolerance = annotations.findUniqueDoubleKey("TOLERANCE", DEFAULT_TOLERANCE);
            tolerance = getDoubleProperty("test.tolerance", tolerance); // Legacy
            boolean ignoreCase = !"false".equals(annotations.findUniqueKey("IGNORECASE"));
            boolean ignoreSpace = !"false".equals(annotations.findUniqueKey("IGNORESPACE"));
            comp.setTolerance(tolerance);
            comp.setIgnoreCase(ignoreCase);
            comp.setIgnoreSpace(ignoreSpace);            
            
            getRequiredClasses();
            getMainClasses();

            for (String modeDir : studentDirectories)
                getGradingFiles(problemDir.resolve(modeDir));

            Set<String> missingClasses = new TreeSet<>();
            for (Path file : requiredFiles) {
                Path source = submissionDir.resolve(file);
                if (Files.exists(source))
                    Files.copy(source, workDir.resolve(file), StandardCopyOption.REPLACE_EXISTING);
                else {
                    report.error("Missing file " + file);
                    missingClasses.add(Util.javaClass(file));
                }
            }

            if (!annotations.checkConditions(workDir, report)) {
                // Do nothing
            } else if (getStringProperty("test.method") != null) {
                String mainclass = getStringProperty("mainclass");
                // Must be able to override since sometimes the mainclass
                // contains the
                // check method, and the student is expected to supply a
                // different class
                if (mainclass == null) {
                    if (mainclasses.size() == 1)
                        mainclass = mainclasses.iterator().next();
                    else if (solutionFiles.size() == 1)
                        mainclass = Util.javaClass(Util.tail(solutionFiles.iterator().next()));
                    else
                        report.systemError("Can't identify mainclass");
                    // TODO: It ought to be able to locate the class that has a
                    // method with the given name
                }

                // Call method
                CallMethod call = new CallMethod(mainclass, checkProperties, timeoutMillis);
                call.setTolerance(tolerance);
                call.setIgnoreCase(ignoreCase);
                call.setIgnoreSpace(ignoreSpace);
                if (compile(mainclass)) {
                    report.header("Calling method");
                    call.prepare(compileSolution(mainclass, null, 0));
                    call.run(workDir, report, score);
                }

                // TODO: If their program runs out of memory, it takes labrat
                // with it. Better to run in new process after all?
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

                for (String mainclass : mainclasses) {
                    if (missingClasses.contains(mainclass))
                        report.error("Missing " + mainclass);
                    else if (isTester(mainclass)
                             && !annotations.isSample(mainclass)
                             && !(containsClass(solutionFiles, mainclass) && inputs.size() > 0))
                        runTester(mainclass);
                    else if (getBooleanProperty("test.snap", false))
                        // TODO: Remove legacy
                        snap(mainclass);
                    else
                        testInputs(inputs, mainclass, annotations);
                }
            }

            if (System.getProperty("com.horstmann.codecheck.textreport") == null)
            {
	            report.header("Student files");
	            for (Path file : requiredFiles)
	                report.file(submissionDir, file);
	
	            String nodoc = checkProperties.getProperty("nodoc");
	            Set<String> nodocCl = new HashSet<String>();
	            if (nodoc != null)
	                nodocCl.addAll(Arrays.asList(nodoc.split(",")));
	
	            Set<Path> hidden = annotations.findHidden();
	            Iterator<Path> iter = printFiles.iterator();
	            while (iter.hasNext()) {
	                Path p = iter.next();
	                if (hidden.contains(p) || nodocCl.contains(Util.javaClass(p)))
	                    iter.remove();
	            }
	
	            printFiles = filterNot(printFiles, "test*.in", "test*.out", "*.expected", "check.properties", "*.png",
	                                   "*.gif", "*.jpg", "*.jpeg", ".DS_Store");
	            if (printFiles.size() > 0) {
	                report.header("Other files");
	                for (Path file : printFiles)
	                    report.file(workDir, file);
	            }
            }
        } catch (Throwable t) {
            report.systemError(t);
        } finally {
            report.add(score);
            report.save(problemId, "report");
        }
        System.exit(0);
    }
}
