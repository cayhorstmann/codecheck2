package com.horstmann.codecheck;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.TreeSet;


public class Main { 
    public static final double DEFAULT_TOLERANCE = 1.0E-6;
    public static final int DEFAULT_TIMEOUT_MILLIS = 15000;
    public static final int DEFAULT_MAX_OUTPUT_LEN = 100_000;
    public static final String DEFAULT_TOKEN = "line";
    public static final int MUCH_LONGER = 1000; // if longer than the expected by this amount, truncate 
    
    private int timeoutMillis;
    private int maxOutputLen;
    private Properties checkProperties;
    private Report report;
    private Map<Path, byte[]> useFiles = new Util.FileMap(); 
        // the files (sources and inputs) from problemFiles that must be copied to the directory 
        // in which the submission/solution program is run
    private Map<Path, byte[]> solutionFiles = new Util.FileMap(); 
        // the source files from problemFiles that make up the solution, same keys as submissionFiles
    private Set<Path> mainSourcePaths = new TreeSet<>(); 
        // the source files that contain main
    private Set<Path> dependentSourcePaths = new TreeSet<>();
        // all other source files

    private boolean inputMode = false;
    
    private Score score = new Score();
    private Comparison comp = new Comparison();
    private Language language = null;
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
       new HaskellLanguage(),
       new SMLLanguage()
    };
    private Annotations annotations;
    private Plan plan;

    /**
     * Entry point to command line program.
     *
     * @param args command-line arguments. args[0] = submission dir,
     * args[1] = problem dir args[2] etc = metadata key=value pairs (optional)
     */
    public static void main(String[] args) throws IOException, ReflectiveOperationException {
        Path submissionDir = FileSystems.getDefault().getPath(args[0]);
        Path problemDir = FileSystems.getDefault().getPath(args[1]);
        Map<Path, String> submissionFiles = Util.descendantTextFiles(submissionDir);
        Map<Path, byte[]> problemFiles = Util.descendantFiles(problemDir);
        Properties metadata = new Properties(); 
        // Used to pass in machine instance, git url into report 
        for (int iarg = 2; iarg < args.length; iarg++) {
            String arg = args[iarg];
            int keyEnd = arg.indexOf("=");
            if (keyEnd >= 0) {
                metadata.put(arg.substring(0, keyEnd), arg.substring(keyEnd + 1));
            }
            else {
                metadata.put(arg, "");
            }
        }
        
        Report report = new Main().run(submissionFiles, problemFiles, System.getProperties(), metadata);
        report.save(submissionDir, "report");        
    }

    private void doSubstitutions(Map<Path, String> submissionFiles, Substitution sub) throws Exception {
        Path mainFile = sub.getFile();
        int n = sub.getSize();
        int timeout = timeoutMillis / n;
        int maxOutput = maxOutputLen / n;
        for (int i = 0; i < n; i++) {
            // TODO: No sense compiling if first (unsubstituted) student program not working
            // This can be a useful optimization elsewhere
        	String contents = submissionFiles.get(mainFile);
            String subtituted = sub.substitute(contents, i); 
            String fileID = "submissionsubfiles" + i;
            String compileID = "submissionsub" + i;            
            plan.addFile(Paths.get(fileID).resolve(mainFile), subtituted);
            plan.compile(compileID, "submission " + fileID, mainFile, dependentSourcePaths);
            plan.run(compileID, compileID, mainFile, null, null, timeout, maxOutput, false);
            contents = new String(solutionFiles.get(mainFile), StandardCharsets.UTF_8);
            subtituted = sub.substitute(contents, i); 
            fileID = "solutionsubfiles" + i;
            compileID = "solutionsub" + i;            
            plan.addFile(Paths.get(fileID).resolve(mainFile), subtituted);
            plan.compile(compileID, "solution " + fileID, mainFile, dependentSourcePaths);
            plan.run(compileID, compileID, mainFile, null, null, timeout, maxOutput, false); 
        }
        plan.addTask(() -> {
            report.header("sub", "Running program with substitutions");
            String[] argNames = sub.names().toArray(new String[0]);
            String[][] args = new String[n][argNames.length];
            String[] actual = new String[n];
            String[] expected = new String[n];
            boolean[] outcomes = new boolean[n];
            for (int i = 0; i < n; i++) {
                if (!plan.checkCompiled("submissionsub" + i, report, score)) return;    
                if (!plan.checkSolutionCompiled("solutionsub" + i, report, score)) return;    
                actual[i] = plan.outerr("submissionsub" + i); 
                expected[i] = plan.outerr("solutionsub" + i);                    
                int j = 0;
                for (String v : sub.values(i)) { args[i][j] = v; j++; }
                outcomes[i] = comp.compare(actual[i], expected[i]).matches;
                actual[i] = Util.truncate(actual[i], expected[i].length() + MUCH_LONGER);
                score.pass(outcomes[i], null); // Pass/fail shown in run table
            }
            report.runTable(null, argNames, args, actual, expected, outcomes);
        });
    }

    private void doCalls(Calls calls) throws Exception {        
        Map<Path, String> testerFiles = calls.writeTester(solutionFiles);
        Path base = Paths.get("callfiles");
        for (Map.Entry<Path, String> entry : testerFiles.entrySet()) 
            plan.addFile(base.resolve(entry.getKey()), entry.getValue());
        
        String[] names = new String[calls.getSize()];
        String[][] args = new String[calls.getSize()][1];
        String[] actual = new String[calls.getSize()];
        String[] expected = new String[calls.getSize()];
        boolean[] outcomes = new boolean[calls.getSize()];

        int timeout = timeoutMillis / calls.getSize();
        int maxOutput = maxOutputLen  / calls.getSize();
        List<Path> sources = new ArrayList<Path>(testerFiles.keySet()); 
        plan.compile("call", "submission callfiles", sources, dependentSourcePaths); // TODO: This is the only place with a list of sources. Why???
        for (int i = 0; i < calls.getSize(); i++) {
            Path mainFile = sources.get(0);
            // TODO: Solution code not isolated from student. It would be more secure to
            // change call strategy to generate output. 
            plan.run("call", "call", "call" + i, mainFile, "", "" + (i + 1), timeout, maxOutput, false);
        }
        plan.addTask(() -> {
            report.header("call", "Calling with Arguments");
            if (!plan.checkCompiled("call", report, score)) return;    
            
            for (int i = 0; i < calls.getSize(); i++) {
                String output = plan.outerr("call" + i);                
                List<String> lines = Util.lines(output);
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
                                    
                    expected[i] = "";  
                    actual[i] = message;
                    outcomes[i] = false;
                }
                score.pass(outcomes[i], null /* no report--it's in the table */);
            }
            report.runTable(names, new String[] { "Arguments" }, args, actual, expected, outcomes);
        });
    }     

    private void runUnitTests() {
        List<Path> unitTests = new ArrayList<>();
        for (Path p : useFiles.keySet()) {
            if (language.isUnitTest(p)) 
                unitTests.add(p);
        }
        if (unitTests.size() > 0) {
            report.header("unitTest", "Unit Tests");
            
            for (Path p: unitTests) {
                String id = plan.nextID("test");
                plan.unitTest(id, p, dependentSourcePaths, timeoutMillis / unitTests.size(), maxOutputLen / unitTests.size());
                plan.addTask(() -> {
                    report.run(p.toString());
                    if (!plan.checkCompiled(id, report, score)) return; 
                    String outerr = plan.outerr(id);                    
                    language.reportUnitTest(outerr, report, score);                
                });
                            
            }
        }
    }
    
    private void runTester(Path mainFile, int timeout, int maxOutputLen) throws Exception {
        String compileID = plan.nextID("tester");
        plan.compile(compileID, "submission", mainFile, dependentSourcePaths);
        plan.run(compileID, compileID, mainFile, "", null, timeout, maxOutputLen, false);
        plan.addTask(() -> {
            report.run("Running " + mainFile);
            if (!plan.checkCompiled(compileID, report, score)) return; 
            String outerr = plan.outerr(compileID);
            AsExpected cond = new AsExpected(comp);
            String tester = plan.getFileString("use", mainFile);
            if (tester == null)
                tester = plan.getFileString("solution", mainFile);  // In case the student was asked to do it
            cond.eval(outerr, report, score, tester);                 
        });
    }

    private void testInputs(Map<String, String> inputs, Path mainFile, Annotations annotations) throws Exception {
        /*
         * If there are no inputs, we feed in one empty input to execute the program.
         */
        if (inputs.size() == 0)
            inputs.put("", ""); 
        
        plan.compile("submissionrun", "submission", mainFile, dependentSourcePaths);
        boolean runSolution = !inputMode && !annotations.isSample(mainFile);
        if (runSolution) 
            plan.compile("solutionrun", "solution", mainFile, dependentSourcePaths);

        plan.addTask(() -> {
        	report.header("run", inputMode ? "Output" : "Testing " + mainFile);
        	plan.checkCompiled("submissionrun", report, score);
        	if (runSolution)
        		plan.checkSolutionCompiled("solutionrun", report, score); 
    	});
        for (String test : inputs.keySet()) {
            String input = inputs.get(test);
            testInput(mainFile, annotations, runSolution, test, input, timeoutMillis / inputs.size(), maxOutputLen / inputs.size());
        }
    }

    private void testInput(Path mainFile, Annotations annotations,
            boolean runSolution, String test, String input, int timeout, int maxOutput)
            throws Exception {
        List<String> runargs = annotations.findKeys("ARGS");
        if (runargs.size() == 0) runargs.add("");
        String out = annotations.findUniqueKey("OUT");
        List<String> outFiles = out == null ? Collections.emptyList() : Arrays.asList(out.trim().split("\\s+"));
        
        String runNumber = test.replace("test", "").trim();
        plan.addTask(() -> { 
            if (!plan.compiled("submissionrun")) return;
            report.run(!test.equals("Input") && runNumber.length() > 0 ? "Test " + runNumber : null);
        });
        boolean interleaveio = language.echoesStdin() == Language.Interleave.ALWAYS ||
            language.echoesStdin() == Language.Interleave.UNGRADED && test.equals("Input");
        if (input == null || input.equals("")) interleaveio = false;
        else if (!input.endsWith("\n")) input += "\n";
        
        // TODO: Language settings
        for (String args : runargs) {
            testInput(mainFile, runSolution, test, input, args, outFiles, timeout / runargs.size(), maxOutput / runargs.size(), interleaveio);
        }
    }
    
    private void testInput(Path mainFile,
            boolean runSolution, String test, String input, String runargs, List<String> outFiles, int timeout, int maxOutput, boolean interleaveio)
            throws Exception {
        String submissionRunID = plan.nextID("submissionrun");
        plan.run("submissionrun", submissionRunID, mainFile, runargs, input, outFiles, timeout, maxOutput, interleaveio);
        String solutionRunID = plan.nextID("solutionrun");        
        if (runSolution) {
            plan.run("solutionrun", solutionRunID, mainFile, runargs, input, outFiles, timeout, maxOutput, interleaveio);                
        }
        plan.addTask(() -> {
            if (!plan.compiled("submissionrun")) return;
            report.args(runargs);
    
            if (!interleaveio && !test.equals("Input")) report.input(input);
    
            List<String> contents = new ArrayList<>();
            List<CompareImages> imageComp = new ArrayList<>();
            String outerr = plan.outerr(submissionRunID);
            for (String f : outFiles) {
                if (CompareImages.isImage(f)) {
                    try {
                        imageComp.add(new CompareImages(plan.getOutputBytes(submissionRunID, f)));
                    } catch (IOException ex) {
                        report.output(outerr);
                        report.error(ex.getMessage());
                    }
                }
                else
                    contents.add(plan.getOutputString(submissionRunID, f));            
            }
                    
            if (!runSolution) { 
                report.output(outerr);
                for (String f : outFiles) {
                    if (CompareImages.isImage(f)) {
                        try {
                            report.image("Image", imageComp.remove(0).first());
                        } catch (IOException ex) {
                            report.error(ex.getMessage());    
                        }
                    }
                    else
                        report.file(f, contents.remove(0));
                }
                // No score
                return;
            } 

            String expectedOuterr = plan.outerr(solutionRunID);
                
            if (expectedOuterr != null && expectedOuterr.trim().length() > 0) {                
                if (outFiles.size() > 0) {
                    // Report output but don't grade it
                    report.output(outerr);
                } else {
                    boolean outcome = comp.execute(input, outerr, expectedOuterr, report, null);
                    score.pass(outcome, report);
                }
            }        
        
            for (String f : outFiles) {
                if (CompareImages.isImage(f)) {
                    CompareImages ic = imageComp.remove(0);
                    try {
                        ic.setOtherImage(plan.getOutputBytes(solutionRunID, f));
                        boolean outcome = ic.getOutcome();
                        report.image("Image", ic.first());
                        if (!outcome) {
                            report.image("Expected", ic.other());
                            report.image("Mismatched pixels", ic.diff());
                        }
                        score.pass(outcome, report);
                    } catch (IOException ex) {
                        report.error(ex.getMessage());                        
                    }
                } else {
                    String expectedContents = plan.getOutputString(solutionRunID, f);                
                    boolean outcome = comp.execute(input, contents.remove(0),
                            expectedContents, report, f);
                    score.pass(outcome, report);
                }
            }
        });
    }
    
    private void getSolutionAndUseFiles(Map<Path, String> submissionFiles, Map<Path, byte[]> problemFiles)  throws IOException {
    	boolean oldStyle = problemFiles.keySet().stream().anyMatch(p -> p.getName(0).toString().equals("student")); // TODO: -> Problem
    	if (oldStyle) { // old style with student and solution directories
            Path checkPropertiesPath = Paths.get("student/check.properties");
            if (problemFiles.containsKey(checkPropertiesPath)) {
            	try (InputStream in = new ByteArrayInputStream(problemFiles.get(checkPropertiesPath))) {
                    checkProperties.load(in);
                }
            }
            for (Path p : problemFiles.keySet()) {
            	if (!Util.matches(Util.tail(p), ".*", "*~", "check.properties", "*.zy")) {
            		String initial = p.getName(0).toString(); 
            		if (initial.equals("student")) {
            			useFiles.put(Util.tail(p), problemFiles.get(p));
            		}
            		else if (initial.equals("solution")) {
            			solutionFiles.put(Util.tail(p), problemFiles.get(p));            			
            		}            		
            	}
            }

            annotations.read(useFiles, solutionFiles, /* inputMode = */ false, report);
        } else {
            for (Path p : problemFiles.keySet()) {
            	if (!Util.matches(p, ".*", "*~", "*.class", "a.out", "*.pyc",  
                    "index.html", "index.ch", "problem.html", 
                    "Input", "*.in", "q.properties", "check.properties", "param.js", "edit.key", "*.zy")) {
            		useFiles.put(p, problemFiles.get(p));
            	}
            }
            inputMode = problemFiles.containsKey(Paths.get("Input"));
            annotations.read(useFiles, Collections.emptyMap(), inputMode, report);
            // Move any files annotated with SOLUTION, SHOW or EDIT to solution 
            for (Path p : annotations.getSolutions()) {
            	solutionFiles.put(p, useFiles.get(p));
            	useFiles.remove(p);
            }

            if (inputMode) {
                Set<Path> useFilePaths = new HashSet<Path>(useFiles.keySet());
                for (Path p : useFilePaths) {
                    if (language.isSource(p) && !annotations.getHidden().contains(p)) {
                    	solutionFiles.put(p, useFiles.get(p));
                    	useFiles.remove(p);
                    }
                }
            }                 
        }

        if (solutionFiles.isEmpty()) {                                   
            throw new CodeCheckException("No solution files found");
        }
    }
    
    private void getMainAndDependentSourceFiles() {
    	for (Map.Entry<Path, byte[]> entry : solutionFiles.entrySet()) {
            Path p = entry.getKey();
            String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
            if (language.isMain(p, contents))
                mainSourcePaths.add(p);
            else if (!language.isUnitTest(p))
                dependentSourcePaths.add(p);
        }
        
        for (Map.Entry<Path, byte[]> entry : useFiles.entrySet()) {
            Path p = entry.getKey();
            if (language.isSource(p)) {
            	String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
            	if (language.isMain(p, contents))
            		mainSourcePaths.add(p);
            	else if (language.isSource(p) && !language.isUnitTest(p))
            		dependentSourcePaths.add(p);
            }
        }
    }

    public Set<Path> copyFilesToPlan(Map<Path, String> submissionFiles) {
        Path use = Paths.get("use");
        Path solution = Paths.get("solution");
        Path submission = Paths.get("submission");
        for (Map.Entry<Path, byte[]> entry : useFiles.entrySet()) {
            Path p = entry.getKey();            
        	if (submissionFiles.containsKey(p)) // This happens in Input mode with submitted inputs TODO: Really?
                plan.addFile(use.resolve(p), submissionFiles.get(p));
        	else        		
        		plan.addFile(use.resolve(entry.getKey()), entry.getValue());
        }
        
        Set<Path> missingFiles = new TreeSet<>();
        for (Map.Entry<Path, byte[]> entry : solutionFiles.entrySet()) {
            Path p = entry.getKey();            
            plan.addFile(solution.resolve(p), entry.getValue());
            if (submissionFiles.containsKey(p))
                plan.addFile(submission.resolve(p), submissionFiles.get(p));
            else {
                report.error("Missing file " + p);
                missingFiles.add(p); // TODO: How can this happen? Why not abort?
            }
        }        
        return missingFiles;
    }
    
    public String reportComments(Properties metadata) {
        report.comment("Submission", plan.getWorkDir().getFileName().toString());
        // This is just a unique ID, can be used to check against cheating
         DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
         df.setTimeZone(TimeZone.getTimeZone("UTC"));
         String currentTime = df.format(new Date());
         report.comment("Time", currentTime);
         report.footnote(currentTime);
         String problemId = annotations.findUniqueKey("ID");
         if (problemId == null) {
             problemId = Util.removeExtension(solutionFiles.keySet().iterator().next().getFileName());                
         }
         else {
             problemId = problemId.replaceAll("[^A-Za-z0-9]", "").toLowerCase();
         }
             
         report.comment("ID", problemId);

         for (Map.Entry<Object, Object> entries : metadata.entrySet())
        	 report.comment(entries.getKey().toString(), entries.getValue().toString());
         return problemId;
    }

    public Report run(Map<Path, String> submissionFiles, Map<Path, byte[]> problemFiles, Properties runProps, Properties metadata) throws IOException {
        long startTime = System.currentTimeMillis();
        String problemID = ""; // TODO: What for?
        try {

            // Set up report first in case anything else throws an exception 
            
            String reportType = runProps.getProperty("com.horstmann.codecheck.report");
            if ("Text".equals(reportType))
                report = new TextReport("Report");
            else if ("JSON".equals(reportType))
                report = new JSONReport("Report");
            else if ("NJS".equals(reportType))
                report = new NJSReport("Report");
            else
                report = new HTMLReport("Report");
            
            // Guess language from problem files
            Set<Path> files = problemFiles.keySet();
            for (int k = 0; language == null && k < languages.length; k++) {
                if (languages[k].isLanguage(files)) 
                    language = languages[k];
            }
            if (language == null) throw new CodeCheckException("Cannot find language from " + files);

            plan = new Plan(language, runProps.getProperty("com.horstmann.codecheck.debug") != null);
            
            checkProperties = new Properties();
            annotations = new Annotations(language);
            getSolutionAndUseFiles(submissionFiles, problemFiles);
            
            
            timeoutMillis = DEFAULT_TIMEOUT_MILLIS;
            String timeoutProperty = runProps.getProperty("com.horstmann.codecheck.timeout");
            if (timeoutProperty != null)
            	timeoutMillis = Integer.parseInt(timeoutProperty);
            timeoutMillis = (int) annotations.findUniqueDoubleKey("TIMEOUT", timeoutMillis);
            
            maxOutputLen = DEFAULT_MAX_OUTPUT_LEN;
            String maxOutputLenProperty = runProps.getProperty("com.horstmann.codecheck.maxoutputlen");
            if (maxOutputLenProperty != null)
                maxOutputLen = Integer.parseInt(maxOutputLenProperty);
            maxOutputLen = (int) annotations.findUniqueDoubleKey("MAXOUTPUTLEN", maxOutputLen);
            
            double tolerance = annotations.findUniqueDoubleKey("TOLERANCE", DEFAULT_TOLERANCE);
            boolean ignoreCase = !"false".equalsIgnoreCase(annotations.findUniqueKey("IGNORECASE"));
            boolean ignoreSpace = !"false".equalsIgnoreCase(annotations.findUniqueKey("IGNORESPACE"));
            comp.setTolerance(tolerance);
            comp.setIgnoreCase(ignoreCase);
            comp.setIgnoreSpace(ignoreSpace);            

            getMainAndDependentSourceFiles();            

            problemID = reportComments(metadata);
            // TODO: Used for name of report?
            
            Set<Path> missingFiles = copyFilesToPlan(submissionFiles);
            
            // the use files that the students are entitled to see
            Set<Path> printFiles = Util.filterNot(useFiles.keySet(),  
                    "*.png", "*.PNG", "*.gif", "*.GIF", "*.jpg", "*.jpeg", "*.JPG", "*.bmp", "*.BMP",
                    "*.jar", "*.pdf");      

            printFiles.removeAll(annotations.getHidden());
            printFiles.removeAll(solutionFiles.keySet());
            
            if (annotations.checkConditions(submissionFiles, report)) {
                if (annotations.has("CALL")) {
                    Calls calls = annotations.findCalls();
                    mainSourcePaths.remove(calls.getFile());
                    dependentSourcePaths.add(calls.getFile());
                    doCalls(calls);
                }
                if (annotations.has("SUB")) {
                    Substitution sub = annotations.findSubstitution();
                    mainSourcePaths.remove(sub.getFile());
                    //dependentSourceFiles.add(sub.getFile());
                    doSubstitutions(submissionFiles, sub);
                }
                
                Map<String, String> inputs = new TreeMap<>(); // TODO: Legacy
                for (String i : new String[] { "", "1", "2", "3", "4", "5", "6", "7", "8", "9" }) {
                    String key = "test" + i + ".in";
                    
                    String in = checkProperties.getProperty(key);
                    if (in != null)
                        in += "\n";
                    else {
                    	Path p = Paths.get(key);
                    	byte[] contents = null;
                    	if (problemFiles.containsKey(p))
                    		contents = problemFiles.get(p);
                    	else {
                    		p = Paths.get("student").resolve(key);
                        	if (problemFiles.containsKey(p))
                        		contents = problemFiles.get(p);                    				
                    	}
                    	if (contents != null)
                    		in = new String(contents, StandardCharsets.UTF_8);
                    }
                    if (in != null)
                        inputs.put("test" + i, in);
                }
                int inIndex = inputs.size();
                for (String s : annotations.findKeys("IN")) {
                    inputs.put("test" + ++inIndex, Util.unescapeJava(s));
                }
                if (inputMode) { 
                    Path p = Paths.get("Input");
                    inputs.put("Input", submissionFiles.get(p));
                }

                runUnitTests(); 
    
                List<Path> testerFiles = new ArrayList<>();
                List<Path> runFiles = new ArrayList<>();
                for (Path mainSourceFile : mainSourcePaths) {
                    if (language.isTester(mainSourceFile) && !solutionFiles.keySet().contains(mainSourceFile)
                             && !annotations.isSample(mainSourceFile) && !inputMode)
                        testerFiles.add(mainSourceFile);
                    else
                        runFiles.add(mainSourceFile);
                } 
                
                if (testerFiles.size() > 0) {
                    report.header("tester", "Testers");
                    for (Path testerFile : testerFiles)
                        if (missingFiles.contains(testerFile)) {
                            report.error("Missing " + testerFile);
                            score.setInvalid();
                        }
                        else
                            runTester(testerFile, timeoutMillis / testerFiles.size(), maxOutputLen / testerFiles.size());
                }

                if (runFiles.size() > 0) {
                    for (Path runFile : runFiles)
                        if (missingFiles.contains(runFile)) {
                            report.error("Missing " + runFile);
                            score.setInvalid();
                        }
                        else
                            testInputs(inputs, runFile, annotations);
                }
                // Process checkstyle.xml etc.
                for (Path p : useFiles.keySet()) {
                    String cmd = language.process(p, submissionFiles);
                    if (cmd != null) {
                        printFiles.remove(p);
                        String id = plan.nextID("process");
                        plan.process(id, cmd);
                        plan.addTask(() -> {
                            String result = plan.outerr(id);
                            language.reportProcessResult(result, report, score);
                        });
                    }
                }
            }
            String remoteURL = runProps.getProperty("com.horstmann.codecheck.remote");            
            plan.execute(report, remoteURL);
            
            if (!inputMode) { // Don't print submitted or provided files for run-only mode
                report.header("studentFiles", "Submitted files");
                /*
                 * Iterate over solutionFiles because submissionFiles may have additional files
                 * when running the codecheck script
                 */
                for (Path p : solutionFiles.keySet())   
                    report.file(p.toString(), submissionFiles.get(p));
    		
                if (printFiles.size() > 0) {
                    report.header("providedFiles", "Provided files");
                    for (Path p : printFiles)
                        report.file(p.toString(), new String(useFiles.get(p), StandardCharsets.UTF_8));
                }
            }
        } catch (Throwable t) {
            if (report != null) report.systemError(t);
            else t.printStackTrace();
        } finally {
            if (report != null) {
                if (annotations != null && !annotations.has("NOSCORE") 
                        && !inputMode) 
                    report.add(score);
                long endTime = System.currentTimeMillis();
                report.comment("Elapsed", (endTime - startTime) + " ms");
                report.close();
            }
            else System.err.println("report is null");
        }
        return report;
    }
}
