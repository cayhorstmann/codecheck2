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

// TODO: How can student supply optional files?

// TODO: Recognize Printer to automatically capture output?

public class Main {
    public static final double DEFAULT_TOLERANCE = 1.0E-6;
    public static final int DEFAULT_TIMEOUT_MILLIS = 30000;
    public static final String DEFAULT_TOKEN = "line";
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
    public String runJavaProgram(final String mainclass, final Path classpathDir, String args, String input)
    throws IOException, ReflectiveOperationException {
        InputStream oldIn = System.in;
        PrintStream oldOut = System.out;
        PrintStream oldErr = System.err;
        if (input == null)
            input = "";
        System.setIn(new ByteArrayInputStream(input.getBytes("UTF-8")));
        ByteArrayOutputStream newOut = new ByteArrayOutputStream();
        final PrintStream newOutPrint = new PrintStream(newOut);
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

            long startTime = System.currentTimeMillis();
            mainmethodThread.start();

            final int delayMillis = DEFAULT_TIMEOUT_MILLIS; // TODO:
            // Parameterize

            try {
                mainmethodThread.join(delayMillis);
            } catch (InterruptedException e) {
            }
            long endTime = System.currentTimeMillis();
            if (endTime - startTime > delayMillis)
                newOutPrint.println("Timed out after " + delayMillis / 1000 + " seconds");

            if (!done.get())
                mainmethodThread.stop();
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
            System.setErr(oldErr);
            System.setSecurityManager(null);
            loader.close();
        }
        return newOut.toString("UTF-8");
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

    private void runTester(String mainclass) throws IOException, UnsupportedEncodingException,
        ReflectiveOperationException {
        report.header("Running " + mainclass);
        // TODO: Assume testers always in default package?
        if (compile(mainclass)) {
            String outerr = runJavaProgram(mainclass, workDir, "", null);
            AsExpected cond = new AsExpected();
            cond.setTolerance(getDoubleProperty("test.tolerance", DEFAULT_TOLERANCE));
            String evaluation = cond.eval(outerr, checkProperties, score);
            report.html(evaluation);
        }
    }

    private void testInputs(Map<String, String> inputs, String mainclass, Annotations annotations) throws UnsupportedEncodingException,
        IOException, ReflectiveOperationException {
        Path tempDir = null;
        if (inputs.size() == 0)
            inputs.put("", getStringProperty("test.run.inputstring")); // Legacy
        report.header("Testing " + mainclass);
        if (compile(mainclass)) {
            for (String test : inputs.keySet()) {
                String input = inputs.get(test);
                // TODO: Might be useful to have more than one set of ARGS
                String runargs = annotations.findUniqueKey("ARGS");
                if (runargs == null)
                    runargs = getStringProperty(test + ".args", "args", "test.args", "test.run.args",
                                                "test.test-inputs.args", "");
                String outerr = runJavaProgram(mainclass, workDir, runargs, input);
                if (input != null && input.length() > 1)
                    report.output("Input " + test, input);
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
                        if (outerr.length() > 0) report.output("Output", outerr);
                        imageComp = new CompareImages(testExpectedPath);
                        report.image("Actual", testExpectedPath);
                    } else
                        contents = Util.read(testExpectedPath);
                }

                if (!test.equals("") || testExpectedFile != null) {
                    // Make output from solution
                    if (tempDir == null)
                        tempDir = compileSolution(mainclass, null, 0);
                    if (tempDir != null) {
                        if (testExpectedFile != null)
                            Files.delete(workDir.resolve(testExpectedFile));
                        String result = runJavaProgram(mainclass, tempDir, runargs, input);
                        if (!test.equals(""))
                            expectedOuterr = result;
                        if (testExpectedFile != null) {
                            if (imageComp == null)
                                expectedContents = Util.read(testExpectedPath);
                        }
                    }
                }

                double tolerance = annotations.findUniqueDoubleKey("TOLERANCE", DEFAULT_TOLERANCE);
                if (tolerance == DEFAULT_TOLERANCE)
                    tolerance = getDoubleProperty(test + ".tolerance", "tolerance", "test.tolerance",
                                                  DEFAULT_TOLERANCE);
                String token =  annotations.findUniqueKey("TOKEN");
                if (token == null) token = getStringProperty(test + ".token", "token", "test.token", DEFAULT_TOKEN);

                if (annotations.isSample(mainclass) || test.equals("") && imageComp == null) // Run without testing
                    report.output("Output", outerr);
                else {
                    if (expectedOuterr != null && expectedOuterr.length() > 0) {
                        Comparison comp = new Comparison();
                        comp.setToken(token);
                        comp.setTolerance(tolerance);
                        report.html("Program output", comp.execute(outerr, expectedOuterr, checkProperties));
                        score.pass(comp.getOutcome(), report);
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
                        Comparison comp = new Comparison();
                        comp.setToken(token);
                        comp.setTolerance(tolerance);
                        report.html(testExpectedFile, comp.execute(contents, expectedContents, checkProperties));
                        score.pass(comp.getOutcome(), report);
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
                if (c != null && c.contains("Tester"))
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
            StringBuilder builder = new StringBuilder();
            builder.append(checkProperties.getProperty("sub.start"));
            builder.append(checkProperties.getProperty("sub.headerStart"));
            for (String name : sub.names()) {
                builder.append(checkProperties.getProperty("sub.headerCellStart"));
                builder.append(name);
                builder.append(checkProperties.getProperty("sub.headerCellEnd"));
            }
            builder.append(checkProperties.getProperty("sub.headerEnd"));

            for (int i = 0; i < sub.getSize(); i++) {
                sub.substitute(submissionDir.resolve(p),
                               workDir.resolve(p), i);
                if (compile(mainclass)) {
                    String actual = runJavaProgram(mainclass, workDir, null, null);
                    Path tempDir = compileSolution(mainclass, sub, i);
                    String expected = runJavaProgram(mainclass, tempDir, null, null);
                    Comparison comp = new Comparison();
                    comp.setToken(getStringProperty("token", "test.token", DEFAULT_TOKEN));
                    comp.setTolerance(getDoubleProperty("tolerance", "test.tolerance",
                                                        DEFAULT_TOLERANCE));
                    builder.append(checkProperties.getProperty("sub.rowStart"));
                    for (String v : sub.values(i)) {
                        builder.append(checkProperties.getProperty("sub.cellStart"));
                        builder.append(Util.htmlEscape(v));
                        builder.append(checkProperties.getProperty("sub.cellEnd"));
                    }
                    builder.append(checkProperties.getProperty("sub.cellStart"));
                    builder.append(Util.htmlEscape(actual));
                    builder.append(checkProperties.getProperty("sub.cellEnd"));
                    builder.append(checkProperties.getProperty("sub.cellStart"));
                    builder.append(Util.htmlEscape(expected));
                    builder.append(checkProperties.getProperty("sub.cellEnd"));
                    comp.execute(actual, expected, checkProperties);

                    builder.append(checkProperties.getProperty("sub.cellStart"));
                    score.pass(comp.getOutcome(), builder);
                    builder.append(checkProperties.getProperty("sub.cellEnd"));
                    builder.append(checkProperties.getProperty("sub.rowEnd"));
                }
            }
            builder.append(checkProperties.getProperty("sub.end"));
            report.html(builder);
        }
    }

    private void doCalls(Path submissionDir, Calls calls) throws IOException, ReflectiveOperationException {
        report.header("Testing method calls");
        Path p = Util.tail(calls.getFile());
        String mainclass = Util.javaClass(p) + "CodeCheck";
        calls.writeTester(problemDir, workDir);

        if (compile(mainclass)) {
            StringBuilder builder = new StringBuilder();
            builder.append(checkProperties.getProperty("callmethod.start"));

            for (int i = 1; i <= calls.getSize(); i++) {
                String[] result = runJavaProgram(mainclass, workDir, "" + i, null).split("\n");
                builder.append(checkProperties.getProperty("callmethod.rowStart"));
                builder.append(checkProperties.getProperty("callmethod.cellStart"));
                // TODO: Add method to header
                builder.append(i == 1 ? calls.getName() : "&nbsp;");
                builder.append(checkProperties.getProperty("callmethod.cellEnd"));
                builder.append(checkProperties.getProperty("callmethod.cellStart"));
                builder.append(Util.htmlEscape(calls.getArgs(i - 1)));
                builder.append(checkProperties.getProperty("callmethod.cellEnd"));
                for (int j = 0; j < 2; j++) {
                    builder.append(checkProperties.getProperty("callmethod.cellStart"));
                    builder.append(Util.htmlEscape(result[j]));
                    builder.append(checkProperties.getProperty("callmethod.cellEnd"));
                }
                builder.append(checkProperties.getProperty("callmethod.cellStart"));
                score.pass(result[2].equals("true"), builder);
                builder.append(checkProperties.getProperty("callmethod.cellEnd"));
                builder.append(checkProperties.getProperty("callmethod.rowEnd"));
            }
            builder.append(checkProperties.getProperty("callmethod.end"));
            report.html(builder);
        }
    }

    public void run(String[] args) throws IOException {
        // TODO: Adjustable Timeouts

        // TODO: Maybe put these style issues into a separate properties file
        // like in the olden days?
        checkProperties
        .put("callmethod.start",
             "<table style=\"font-size: 0.9em;\"><tr><th>Method</th><th>Arguments</th><th>Actual</th><th>Expected</th><th>&nbsp;</th></tr>\n");
        checkProperties.put("callmethod.end", "</table>\n");
        checkProperties.put("callmethod.rowStart", "<tr>");
        checkProperties.put("callmethod.rowEnd", "</tr>\n");
        checkProperties.put("callmethod.cellStart", "<td style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">");
        checkProperties.put("callmethod.cellEnd", "</td>");

        checkProperties.put("sub.start",
                            "<table style=\"font-size: 0.9em;\">\n");
        checkProperties.put("sub.end", "</table>\n");
        checkProperties.put("sub.headerStart", "<tr>");
        checkProperties.put("sub.headerEnd", "<th style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">Actual</th><th style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">Expected</th><th style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">&nbsp;</th></tr>\n");
        checkProperties.put("sub.headerCellStart", "<td style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">");
        checkProperties.put("sub.headerCellEnd", "</td>");
        checkProperties.put("sub.rowStart", "<tr>");
        checkProperties.put("sub.rowEnd", "</tr>\n");
        checkProperties.put("sub.cellStart", "<td style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">");
        checkProperties.put("sub.cellEnd", "</td>");

        checkProperties.put("comparison.start",
                            "<table style=\"font-size: 0.9em;\"><tr><th>Actual</th><th>Expected</th></tr>\n");
        checkProperties.put("comparison.mismatchStart", "<font color=\"red\">");
        checkProperties.put("comparison.mismatchEnd", "</font>");
        checkProperties.put("comparison.questionableStart", "<font color=\"blue\">");
        checkProperties.put("comparison.questionableEnd", "</font>");
        checkProperties.put("comparison.expectedStart", "<font color=\"lightgray\">");
        checkProperties.put("comparison.expectedEnd", "</font>");
        checkProperties.put("expected.unexpectedStart", "<font color=\"red\">");
        checkProperties.put("expected.unexpectedEnd", "</font>");
        checkProperties.put("expected.matchedStart", "<font color=\"green\">");
        checkProperties.put("expected.matchedEnd", "</font>");

        // TODO: What if args[0], args[1] don't exist?

        String mode = args[0].trim();
        Path submissionDir = FileSystems.getDefault().getPath(args[1]);
        problemDir = FileSystems.getDefault().getPath(args[2]);
        report = new Report(args.length >= 4 ? args[3] : "Report");
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
                CallMethod call = new CallMethod(mainclass, checkProperties);
                if (compile(mainclass)) {
                    report.header("Calling method");
                    call.prepare(compileSolution(mainclass, null, 0));
                    report.html(call.run(workDir, score));
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
                    else if (mainclass.contains("Tester")
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
        } catch (Throwable t) {
            report.systemError(t);
        } finally {
            report.add(score);
            report.save(submissionDir.resolve("report.html"));
        }
        System.exit(0);
    }
}
