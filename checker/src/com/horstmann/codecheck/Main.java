package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
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
    private Set<String> mainmodules = new TreeSet<String>(); // module names
    private Score score = new Score();
    private Comparison comp = new Comparison();
    private Language language = new JavaLanguage();

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

    public boolean containsModule(Set<Path> files, String modulename) {
        for (Path file : files)
            if (language.moduleOf(Util.tail(file)).equals(modulename))
                return true;
        return false;
    }

    public boolean compile(String modulename) {
        return language.compile(modulename, workDir, report);
    }

    public Path compileSolution(String modulename, Substitution sub, int n) throws IOException {
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
        boolean result = language.compile(modulename, tempDir, report);
        return result ? tempDir : null;
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

    private void snap(String mainclass) throws Exception { // Legacy
        SecurityManager securityManager = new StudentSecurityManager();

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
    
    private void runTester(String mainmodule) throws IOException, UnsupportedEncodingException,
        ReflectiveOperationException {
        report.header("Running " + mainmodule);
        // TODO: Assume testers always in default package?
        
        // TODO: Scoring doesn't work when outerr contains an exception report because we don't know how many
        // test cases have not occurred. 
        // May need to count the number of expected cases in the 
        
        if (compile(mainmodule)) {
            String outerr = language.run(mainmodule, workDir, "", null, timeoutMillis);
            AsExpected cond = new AsExpected(comp);
            cond.eval(outerr, report, score, workDir.resolve(language.pathOf(mainmodule)));
        }
    }

    private void testInputs(Map<String, String> inputs, String mainmodule, Annotations annotations) throws UnsupportedEncodingException,
        IOException, ReflectiveOperationException {
        /*
         * If there are no inputs, we feed in one empty input, or the legacy test.run.inputstring
         */
        if (inputs.size() == 0)
            inputs.put("", getStringProperty("test.run.inputstring")); // Legacy
        report.header("Testing " + mainmodule);
        Path solutionDir = null;
        if (!(annotations.isSample(mainmodule) || "true".equals(getStringProperty("test.run")))) {
            solutionDir = compileSolution(mainmodule, null, 0);
            if (solutionDir == null) return;
        }        
        
        if (compile(mainmodule)) {
            for (String test : inputs.keySet()) {
                String input = inputs.get(test);
                testInput(mainmodule, annotations, solutionDir, test, input);
            }
        }
    }

    private void testInput(String mainmodule, Annotations annotations,
            Path solutionDir, String test, String input)
            throws IOException, ReflectiveOperationException {
        // TODO: Might be useful to have more than one set of ARGS        
        String runargs = annotations.findUniqueKey("ARGS");
        if (runargs == null)
            runargs = getStringProperty(test + ".args", "args", "test.args", "test.run.args",
                                        "test.test-inputs.args", "");
        String outFile = annotations.findUniqueKey("OUT");
        if (outFile == null)
            outFile = getStringProperty(test + ".outputfile", (test.length() > 0 ? test
                                                 : "test.run") + ".expectedfile", "outputfile", "test.expectedfile", null); // Legacy
        Path outPath = outFile == null ? null : workDir.resolve(outFile);

        if (outPath != null)
            Files.deleteIfExists(outPath);

        String outerr = language.run(mainmodule, workDir, runargs, input, timeoutMillis);
        /*               
        if (input != null && input.length() > 1)
            report.output("Input " + test, input);
        */
        String contents = null;
        String expectedContents = null;
        String expectedOuterr = null;
        CompareImages imageComp = null;
        if (outPath != null) {
            if (CompareImages.isImage(outFile)) {
                try {
                    imageComp = new CompareImages(outPath);
                    report.image(outPath);
                } catch (IOException ex) {
                    report.error(ex.getMessage());
                    return; 
                }
            } else
                contents = Util.read(outPath);
        }

        String title = "Program run";
        if (test != null) title = (title + " " + test.replace("test", "")).trim(); 
        if (solutionDir == null) { // Run without testing
            report.output(title, outerr);
            // TODO: Score?
            return;
        }
        if (outPath != null) Files.delete(outPath); // TODO: This is to deal with the flaky graphics that may never write the file
        expectedOuterr = language.run(mainmodule, solutionDir, runargs, input, timeoutMillis);
        if (outPath != null) {
            if (imageComp == null)
                expectedContents = Util.read(outPath);
        }

        if (outFile != null) {
            if (imageComp != null) {
                try {
                    imageComp.setOtherImage(outPath);
                    boolean outcome = imageComp.getOutcome();
                    if (!outcome) {
                        report.image("Expected", outPath);
                        report.image("Mismatched pixels", imageComp.diff());
                    }
                    score.pass(outcome, report);
                } catch (IOException ex) {
                    report.systemError(ex.getMessage());
                }
            } else {
                report.header(outFile);
                boolean outcome = comp.execute(contents,
                        expectedContents, report, title);
                score.pass(outcome, report);
            }
        }

        if (expectedOuterr != null && expectedOuterr.length() > 0) {
            boolean outcome = comp.execute(outerr, expectedOuterr, report, title);
            score.pass(outcome, report);
        }
    }

    private void getMainModules() {
        Set<Path> files = new TreeSet<>();
        files.addAll(studentFiles);
        if (solutionFiles.size() > 0) {
            files.addAll(solutionFiles);
            for (Path p : files) {
                String c = language.moduleOf(Util.tail(p));
                if (c != null && language.isMain(problemDir, p))
                    mainmodules.add(c);
            }

        } else { // Legacy
            String mainclassProperty = getStringProperty("mainclass");

            if (mainclassProperty != null) {
                mainmodules.add(mainclassProperty);
                if (!getBooleanProperty("mainclass.required", true))
                    requiredFiles.remove(language.pathOf(mainclassProperty));
            }

            for (Path p : files) {
                String c = language.moduleOf(Util.tail(p));
                if (language.isTester(c))
                    mainmodules.add(c);
            }
        }
    }

    private void getRequiredModules() {
        if (solutionFiles.size() > 0) {
            for (Path p : solutionFiles)
                if (language.isSource(p))
                    requiredFiles.add(Util.tail(p));
        } else { // Legacy
            String requiredclasses = getStringProperty("requiredclasses");
            if (requiredclasses != null)
                for (String f : requiredclasses.trim().split("\\s*,\\s*"))
                    requiredFiles.add(language.pathOf(f));
        }
    }

    private void doSubstitutions(Path submissionDir, Substitution sub) throws IOException, ReflectiveOperationException {
        report.header("Running program with substitutions");
        Path p = Util.tail(sub.getFile());
        String mainmodule = language.moduleOf(p);
        if (compile(mainmodule)) {
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
                if (compile(mainmodule)) {
                    actual[i] = language.run(mainmodule, workDir, null, null, timeout);
                    Path tempDir = compileSolution(mainmodule, sub, i);
                    expected[i] = language.run(mainmodule, tempDir, null, null, timeout);                    
                    
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
        String mainmodule = language.moduleOf(p) + "CodeCheck";
        calls.writeTester(problemDir, workDir);
        
        String[][] args = new String[calls.getSize()][1];
        String[] actual = new String[calls.getSize()];
        String[] expected = new String[calls.getSize()];
        boolean[] outcomes = new boolean[calls.getSize()];

        int timeout = timeoutMillis / calls.getSize();
        
        if (compile(mainmodule)) {
            for (int i = 0; i < calls.getSize(); i++) {
            	String result = language.run(mainmodule, workDir, "" + (i + 1), null, timeout);
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

        for (URL url : ((URLClassLoader) getClass().getClassLoader()).getURLs()) {
            String urlString = url.toString();
            if (urlString.endsWith("codecheck.jar")) {
                String path = urlString.substring(urlString.indexOf('/'),
                        urlString.lastIndexOf('/'));
                System.setProperty("java.security.policy", path
                        + "/codecheck.policy");
            }
        }
        //System.setSecurityManager(new SecurityManager());
    	
    	// TODO: Discover from file extension
    	String languageName = System.getProperty("com.horstmann.codecheck.language");
    	if (languageName != null) {
    		try {
				language = (Language) Class.forName(languageName + "Language").newInstance();
			} catch (InstantiationException | IllegalAccessException
					| ClassNotFoundException e) {
				report.error("Cannot process language " + languageName);
			}
    	}
    	
        String mode = args[0].trim();
        Path submissionDir = FileSystems.getDefault().getPath(args[1]);
        problemDir = FileSystems.getDefault().getPath(args[2]);
        if (System.getProperty("com.horstmann.codecheck.textreport") != null)
        	report = new TextReport("Report", submissionDir);
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

            Annotations annotations = new Annotations(language);
            annotations.read(problemDir, studentFiles);
            annotations.read(problemDir, solutionFiles);

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
            	problemId = language.moduleOf(Util.tail(solutionFiles.iterator().next()));            	
            }
            else {
            	problemId = problemId.replaceAll("[^A-Za-z0-9]", "").toLowerCase();
            }
        	
            report.comment("ID", problemId);
            
            // Used to pass in machine instance into report 
            for (int iarg = 3; iarg < args.length; iarg++) {
            	String arg = args[iarg];
            	int keyEnd = arg.indexOf("=");
            	if (keyEnd >= 0) {
            		report.comment(arg.substring(0, keyEnd), arg.substring(keyEnd + 1));
            	}
            }
            
            
            
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
            
            getRequiredModules();
            getMainModules();            
            
            for (String modeDir : studentDirectories)
                getGradingFiles(problemDir.resolve(modeDir));

            Set<String> missingModules = new TreeSet<>();
            for (Path file : requiredFiles) {
                Path source = submissionDir.resolve(file);
                if (Files.exists(source))
                    Files.copy(source, workDir.resolve(file), StandardCopyOption.REPLACE_EXISTING);
                else {
                    report.error("Missing file " + file);
                    missingModules.add(language.moduleOf(file));
                }
            }

            if (!annotations.checkConditions(workDir, report)) {
                // Do nothing
            } else if (getStringProperty("test.method") != null) {
                String mainmodule = getStringProperty("mainclass");
                // Must be able to override since sometimes the mainclass
                // contains the
                // check method, and the student is expected to supply a
                // different class
                if (mainmodule == null) {
                    if (mainmodules.size() == 1)
                        mainmodule = mainmodules.iterator().next();
                    else if (solutionFiles.size() == 1)
                        mainmodule = language.moduleOf(Util.tail(solutionFiles.iterator().next()));
                    else
                        report.systemError("Can't identify main module");
                    // TODO: It ought to be able to locate the class that has a
                    // method with the given name
                }

                // Call method
                CallMethod call = new CallMethod(mainmodule, checkProperties, timeoutMillis);
                call.setTolerance(tolerance);
                call.setIgnoreCase(ignoreCase);
                call.setIgnoreSpace(ignoreSpace);
                if (compile(mainmodule)) {
                    report.header("Calling method");
                    call.prepare(compileSolution(mainmodule, null, 0));
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

                for (String mainmodule : mainmodules) {
                    if (missingModules.contains(mainmodule))
                        report.error("Missing " + mainmodule);
                    else if (language.isTester(mainmodule)
                             && !annotations.isSample(mainmodule)
                             && !(containsModule(solutionFiles, mainmodule) && inputs.size() > 0))
                        runTester(mainmodule);
                    else if (getBooleanProperty("test.snap", false))
                        // TODO: Remove legacy
                        snap(mainmodule);
                    else
                        testInputs(inputs, mainmodule, annotations);
                }
            }

            if (System.getProperty("com.horstmann.codecheck.textreport") == null)
            {
	            report.header("Student files");
	            for (Path file : requiredFiles)
	                report.file(submissionDir, file);
	
	            String nodoc = checkProperties.getProperty("nodoc"); // Legacy
	            Set<String> nodocCl = new TreeSet<String>();
	            if (nodoc != null)
	                nodocCl.addAll(Arrays.asList(nodoc.split(",")));
	
	            printFiles = filterNot(printFiles, "test*.in", "test*.out", "*.expected", "check.properties", "*.png",
                        "*.gif", "*.jpg", "*.jpeg", ".DS_Store");

	            Set<Path> hidden = annotations.findHidden();
	            Iterator<Path> iter = printFiles.iterator();
	            while (iter.hasNext()) {
	                Path p = iter.next();
	                if (hidden.contains(p)) iter.remove();
	                else {
	                	String cl = language.moduleOf(p);
	                	if (cl != null && nodocCl.contains(cl)) iter.remove();
	                }	                    
	            }
	
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
