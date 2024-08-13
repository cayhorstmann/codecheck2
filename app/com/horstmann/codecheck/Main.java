package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;


public class Main { 
    public static final double DEFAULT_TOLERANCE = 1.0E-6;
    public static final int DEFAULT_TIMEOUT_MILLIS = 15000;
    public static final int DEFAULT_MAX_OUTPUT_LEN = 100_000;
    public static final String DEFAULT_TOKEN = "line";
    public static final int MUCH_LONGER = 1000; // if longer than the expected by this amount, truncate 
    
    private int timeoutMillis;
    private int maxOutputLen;
    private Report report;
    private Problem problem;
    // TODO: Move these into Problem instance
    private Set<Path> mainSourcePaths = new TreeSet<>(); 
        // the source files that contain main
    private Set<Path> dependentSourcePaths = new TreeSet<>();
        // all other source files

    private Score score = new Score();
    private Comparison comp = new Comparison();
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
        
        Report report = new Main().run(submissionFiles, 
            problemFiles, 
            System.getProperty("com.horstmann.codecheck.report"), 
            metadata, 
            new CommandLineResourceLoader()).getReport();
        Files.write(submissionDir.resolve("report." + report.extension()), report.getText().getBytes());
    }

    private void doSubstitutions(Map<Path, String> submissionFiles, Substitution sub) throws Exception {
        Path mainFile = sub.getFile();
        int n = sub.getSize();
        int timeout = timeoutMillis / n;
        int maxOutput = maxOutputLen / n;
        for (int i = 0; i < n; i++) {
            String contents = submissionFiles.get(mainFile);
            String subtituted = sub.substitute(contents, i); 
            String fileID = "submissionsubfiles" + i;
            String compileID = "submissionsub" + i;            
            plan.addFile(Paths.get(fileID).resolve(mainFile), subtituted);
            plan.compile(compileID, "submission " + fileID, mainFile, dependentSourcePaths);
            plan.run(compileID, compileID, mainFile, null, null, timeout, maxOutput, false);
            contents = Util.getString(problem.getSolutionFiles(), mainFile);
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
       
    private void doCalls(Map<Path, String> submissionFiles, Calls calls, ResourceLoader resourceLoader) throws Exception {
    	Path file = calls.getFile();
    	
        String submissionContents = submissionFiles.get(file);
        if (submissionContents.isEmpty()) submissionContents = Util.getString(problem.getUseFiles(), file);
        Map<Path, String> submissionTesterFiles = problem.getLanguage().writeTester(file, submissionContents, calls.getCalls(), resourceLoader);
        Path submissionBase = Paths.get("submissioncallfiles");
        for (Map.Entry<Path, String> entry : submissionTesterFiles.entrySet()) 
            plan.addFile(submissionBase.resolve(entry.getKey()), entry.getValue());
        List<Path> submissionSources = new ArrayList<Path>(submissionTesterFiles.keySet()); 

        String solutionContents = Util.getString(problem.getSolutionFiles(), file);
        if (solutionContents.isEmpty()) solutionContents = Util.getString(problem.getUseFiles(), file);
        Map<Path, String> solutionTesterFiles = problem.getLanguage().writeTester(file, solutionContents, calls.getCalls(), resourceLoader);    	
        Path solutionBase = Paths.get("solutioncallfiles");
        for (Map.Entry<Path, String> entry : solutionTesterFiles.entrySet()) 
            plan.addFile(solutionBase.resolve(entry.getKey()), entry.getValue());
        List<Path> solutionSources = new ArrayList<Path>(solutionTesterFiles.keySet());
        
        // TODO: This is the only place with a list of sources. 
        // If the multiple sources in the C++ tester are removed, do we still need them?
        plan.compile("submissioncall", "submission submissioncallfiles", submissionSources, dependentSourcePaths); 
        plan.compile("solutioncall", "solution solutioncallfiles", solutionSources, dependentSourcePaths);
        
        int timeout = timeoutMillis / calls.getSize();
        int maxOutput = maxOutputLen  / calls.getSize();
        for (int i = 0; i < calls.getSize(); i++) {
            plan.run("submissioncall", "submissioncall", "submissioncall" + i, submissionSources.get(0), "", "" + (i + 1), timeout, maxOutput, false);
            plan.run("solutioncall", "solutioncall", "solutioncall" + i, solutionSources.get(0), "", "" + (i + 1), timeout, maxOutput, false);
        }
        plan.addTask(() -> {
            report.header("call", "Calling with Arguments");
            if (!plan.checkCompiled("submissioncall", report, score)) return;  
            if (!plan.checkCompiled("solutioncall", report, score)) return;   
            
            String[] names = new String[calls.getSize()];
            String[][] args = new String[calls.getSize()][1];
            String[] actual = new String[calls.getSize()];
            String[] expected = new String[calls.getSize()];
            boolean[] outcomes = new boolean[calls.getSize()];

            for (int i = 0; i < calls.getSize(); i++) {
                actual[i] = plan.outerr("submissioncall" + i);     
                expected[i] = plan.outerr("solutioncall" + i);     
                outcomes[i] = actual[i].equals(expected[i]);     
                Calls.Call call = calls.getCall(i);
                names[i] = call.name;
                args[i][0] = call.args;
                if (call.isHidden()) {
                    actual[i] = "[hidden]";
                    expected[i] = "[hidden]"; 
                    args[i][0] = "[hidden]"; 
                }
                score.pass(outcomes[i], null /* no report--it's in the table */);
            }
            report.runTable(names, new String[] { "Arguments" }, args, actual, expected, outcomes);
        });
    }     

    private void runUnitTests() {
        List<Path> unitTests = new ArrayList<>();
        for (Path p : problem.getUseFiles().keySet()) {
            if (problem.getLanguage().isUnitTest(p)) 
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
                    if (problem.getAnnotations().getHiddenTestFiles().contains(p))
                        problem.getLanguage().reportUnitTest(outerr, report, score, true);   
                    else 
                        problem.getLanguage().reportUnitTest(outerr, report, score, false);

                });
                            
            }
        }
    }
    
    private void runTester(Path mainFile, int timeout, int maxOutputLen) throws Exception {
        String compileID = plan.nextID("tester");
        plan.compile(compileID, "submission", mainFile, dependentSourcePaths);
        plan.run(compileID, compileID, mainFile, "", null, timeout, maxOutputLen, false);
        plan.addTask(() -> {
            report.run(mainFile.toString());
            if (!plan.checkCompiled(compileID, report, score)) return; 
            String outerr = plan.outerr(compileID);
            AsExpected cond = new AsExpected(comp);
            String tester = plan.getFileString("use", mainFile);
            if (problem.getAnnotations().getHiddenTestFiles().contains(mainFile))
                cond.setHidden(true);
            if (tester == null)
                tester = plan.getFileString("solution", mainFile);  // In case the student was asked to do it
            cond.eval(outerr, report, score, tester);                 
        });
    }

    private void testInputs(List<Input> inputs, Path mainFile, boolean okToInterleave) throws Exception {
        /*
         * If there are no inputs, we feed in one empty input to execute the program.
         */
        if (inputs.size() == 0)
            inputs.add(new Input("", "", false));
        
        plan.compile("submissionrun", "submission", mainFile, dependentSourcePaths);
        boolean runSolution = !problem.getInputMode() && !problem.getAnnotations().isSample(mainFile);
        if (runSolution) {
            plan.compile("solutionrun", "solution", mainFile, dependentSourcePaths);
        }

        plan.addTask(() -> {
            report.header("run", problem.getInputMode() ? "Output" : "Running " + mainFile);
            if (runSolution)
                plan.checkSolutionCompiled("solutionrun", report, score); 
            plan.checkCompiled("submissionrun", report, score);
        });
        for (int i = 0; i < inputs.size(); i++) {   
            String test = inputs.get(i).getKey(); 
            String input = inputs.get(i).getValue(); 
            boolean hidden = inputs.get(i).getHidden(); 
            testInput(mainFile, runSolution, test, input, timeoutMillis / inputs.size(), maxOutputLen / inputs.size(), okToInterleave, hidden);
        }
    }

    private void testInput(Path mainFile, 
            boolean runSolution, String test, String input, int timeout, int maxOutput, boolean okToInterleave, boolean hidden)
            throws Exception {
        List<String> runargs = problem.getAnnotations().findAll("ARGS");
        if (runargs.size() == 0) runargs.add("");
        String out = problem.getAnnotations().findUnique("OUT");
        List<String> outFiles = out == null ? Collections.emptyList() : Arrays.asList(out.trim().split("\\s+"));
        
        String runNumber = test.replace("test", "").trim();
        plan.addTask(() -> { 
            if (!plan.compiled("submissionrun")) return;
            report.run(!test.equals("Input") && runNumber.length() > 0 ? "Test " + runNumber : null);
        });
        boolean interleaveio = okToInterleave && (problem.getLanguage().echoesStdin() == Language.Interleave.ALWAYS ||
            problem.getLanguage().echoesStdin() == Language.Interleave.UNGRADED && test.equals("Input"));
        if (input == null || input.isBlank()) interleaveio = false;
        else if (!input.endsWith("\n")) input += "\n";
        
        // TODO: Language settings
        for (String args : runargs) {
            testInput(mainFile, runSolution, test, input, args, outFiles, timeout / runargs.size(), maxOutput / runargs.size(), interleaveio, hidden) ;
        }
    }
    
    private void testInput(Path mainFile,
            boolean runSolution, String test, String input, String runargs, List<String> outFiles, int timeout, int maxOutput, boolean interleaveio, boolean hidden)
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

            String outerr = plan.outerr(submissionRunID);
            String expectedOuterr = plan.outerr(solutionRunID);                
            if (expectedOuterr != null && expectedOuterr.trim().length() > 0 && outFiles.size() == 0) {                
            	boolean outcome = comp.execute(input, outerr, expectedOuterr, report, null, hidden);
            	score.pass(outcome, report);
            } else {        
                // Not scoring output if there are outFiles, but showing in case there is an exception
                report.output(outerr);
            }
            
            Map<String, String> contents = new HashMap<>();
            Map<String, CompareImages> imageComp = new HashMap<>();
            for (String f : outFiles) {
                if (CompareImages.isImage(f)) {
                    imageComp.put(f, new CompareImages(plan.getOutputBytes(submissionRunID, f)));
                }
                else
                    contents.put(f, plan.getOutputString(submissionRunID, f));            
            }
                    
            if (!runSolution) { 
                for (String f : outFiles) {
                    if (CompareImages.isImage(f)) {
                        CompareImages ci = imageComp.get(f);
                        report.image("Image", ci.first());
                    }
                    else
                        report.file(f, contents.get(f));
                }
                // No score
                return;
            } 

            for (String f : outFiles) {
                if (CompareImages.isImage(f)) {
                    CompareImages ic = imageComp.get(f);
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
                    boolean outcome = comp.execute(input, contents.get(f),
                            expectedContents, report, f, hidden);
                    score.pass(outcome, report);
                }
            }
        });
    }
    
    private void getMainAndDependentSourceFiles() {
        for (Map.Entry<Path, byte[]> entry : problem.getSolutionFiles().entrySet()) {
            Path p = entry.getKey();
            String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
            if (problem.getLanguage().isMain(p, contents))
                mainSourcePaths.add(p);
            else if (!problem.getLanguage().isUnitTest(p))
                dependentSourcePaths.add(p);
        }
        
        for (Map.Entry<Path, byte[]> entry : problem.getUseFiles().entrySet()) {
            Path p = entry.getKey();
            if (problem.getLanguage().isSource(p)) {
                String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
                if (problem.getLanguage().isMain(p, contents))
                    mainSourcePaths.add(p);
                else if (!problem.getLanguage().isUnitTest(p))
                    dependentSourcePaths.add(p);
            }
        }
    }

    public void copyFilesToPlan(Map<Path, String> submissionFiles) {
        Path use = Paths.get("use");
        Path solution = Paths.get("solution");
        Path submission = Paths.get("submission");
        for (Map.Entry<Path, byte[]> entry : problem.getUseFiles().entrySet()) 
            plan.addFile(use.resolve(entry.getKey()), entry.getValue());
        for (Map.Entry<Path, byte[]> entry : problem.getInputFiles().entrySet()) {
            Path p = entry.getKey();
            if (!submissionFiles.containsKey(p)) throw new CodeCheckException("Missing file " + p);
            plan.addFile(use.resolve(p), submissionFiles.get(p));
        }
        
        for (Map.Entry<Path, byte[]> entry : problem.getSolutionFiles().entrySet()) {
            Path p = entry.getKey();            
            plan.addFile(solution.resolve(p), entry.getValue());
            if (!submissionFiles.containsKey(p)) throw new CodeCheckException("Missing file " + p);
            plan.addFile(submission.resolve(p), submissionFiles.get(p));
        }        
    }
    
    public void reportComments(Properties metadata) {
        report.comment("Submission", Util.createPrivateUID());
        // This is just a unique ID, can be used to check against cheating
         DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
         df.setTimeZone(TimeZone.getTimeZone("UTC"));
         String currentTime = df.format(new Date());
         report.comment("Time", currentTime);
         report.footnote(currentTime);
             
         report.comment("ID", problem.getId());

         for (Map.Entry<Object, Object> entries : metadata.entrySet())
             report.comment(entries.getKey().toString(), entries.getValue().toString());
    }

    public Plan run(Map<Path, String> submissionFiles, Map<Path, byte[]> problemFiles, 
            String reportType, Properties metadata, ResourceLoader resourceLoader) throws IOException {
        long startTime = System.currentTimeMillis();
        boolean okToInterleave = true;
        boolean scoring = true;
        try {
            // Set up report first in case anything else throws an exception 
            
            if ("Text".equals(reportType))
                report = new TextReport("Report");
            else if ("JSON".equals(reportType))
                report = new JSONReport("Report");
            else if ("NJS".equals(reportType))
                report = new NJSReport("Report");
            else if ("Setup".equals(reportType)) {
            	report = new SetupReport("Report");
            	okToInterleave = false;
            }
            else
                report = new HTMLReport("Report");
            
            plan = new Plan(resourceLoader.getProperty("com.horstmann.codecheck.debug") != null);
            plan.setReport(report);
            plan.readSolutionOutputs(problemFiles);

            problem = new Problem(problemFiles);
            if (report instanceof SetupReport) ((SetupReport) report).setProblem(problem); 
            plan.setLanguage(problem.getLanguage());

            // TODO: This would be nice to have in Problem, except that one might later need to remove checkstyle.xml
            // the use files that the students are entitled to see
            Set<Path> printFiles = Util.filterNot(problem.getUseFiles().keySet(),  
            "*.png", "*.PNG", "*.gif", "*.GIF", "*.jpg", "*.jpeg", "*.JPG", "*.bmp", "*.BMP",
            "*.jar", "*.pdf");      

            printFiles.removeAll(problem.getAnnotations().getHidden());
            printFiles.removeAll(problem.getAnnotations().getHiddenTestFiles());
            printFiles.removeAll(problem.getSolutionFiles().keySet());
            
            timeoutMillis = (int) problem.getAnnotations().findUniqueDoubleKey("TIMEOUT", DEFAULT_TIMEOUT_MILLIS);
            maxOutputLen = (int) problem.getAnnotations().findUniqueDoubleKey("MAXOUTPUTLEN", DEFAULT_MAX_OUTPUT_LEN);        
            double tolerance = problem.getAnnotations().findUniqueDoubleKey("TOLERANCE", DEFAULT_TOLERANCE);
            boolean ignoreCase = !"false".equalsIgnoreCase(problem.getAnnotations().findUnique("IGNORECASE"));
            boolean ignoreSpace = !"false".equalsIgnoreCase(problem.getAnnotations().findUnique("IGNORESPACE"));
            if ("false".equalsIgnoreCase(problem.getAnnotations().findUnique("SCORING"))) scoring = false;
            if ("false".equalsIgnoreCase(problem.getAnnotations().findUnique("INTERLEAVE"))) okToInterleave = false;
            comp.setTolerance(tolerance);
            comp.setIgnoreCase(ignoreCase);
            comp.setIgnoreSpace(ignoreSpace);            

            getMainAndDependentSourceFiles();            

            reportComments(metadata);
            
            copyFilesToPlan(submissionFiles);

            if (problem.getAnnotations().checkConditions(submissionFiles, report)) {
                if (problem.getAnnotations().has("CALL") || problem.getAnnotations().has("CALL HIDDEN")) {
                    Calls calls = problem.getAnnotations().findCalls();
                    mainSourcePaths.remove(calls.getFile());
                    dependentSourcePaths.add(calls.getFile());
                    doCalls(submissionFiles, calls, resourceLoader);
                }
                if (problem.getAnnotations().has("SUB")) {
                    Substitution sub = problem.getAnnotations().findSubstitution();
                    mainSourcePaths.remove(sub.getFile());
                    //dependentSourceFiles.add(sub.getFile());
                    doSubstitutions(submissionFiles, sub);
                }
                
                List<Input> inputs = new ArrayList<Input>();
                for (String i : new String[] { "", "1", "2", "3", "4", "5", "6", "7", "8", "9" }) {
                    String key = "test" + i + ".in";

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
                        inputs.add(new Input("test" + i, new String(contents, StandardCharsets.UTF_8), false));
                }
                int inIndex = inputs.size();
                for (String s : problem.getAnnotations().findAll("IN")) {
                    inputs.add(new Input("test" + ++inIndex, Util.unescapeJava(s), false)); 
                }
                for (String s : problem.getAnnotations().findAll("IN HIDDEN")) {
                    inputs.add(new Input("test" + ++inIndex, Util.unescapeJava(s), true)); 
                }
                if (problem.getInputMode()) { 
                    Path p = Paths.get("Input");
                    inputs.add(new Input("Input", submissionFiles.get(p), false));
                }

                runUnitTests(); 
    
                List<Path> testerFiles = new ArrayList<>();
                List<Path> runFiles = new ArrayList<>();
                for (Path mainSourceFile : mainSourcePaths) {
                    if (problem.getLanguage().isTester(mainSourceFile) && !problem.getSolutionFiles().keySet().contains(mainSourceFile)
                             && !problem.getAnnotations().isSample(mainSourceFile) && !problem.getInputMode())
                        testerFiles.add(mainSourceFile);
                    else
                        runFiles.add(mainSourceFile);
                } 
                
                if (testerFiles.size() > 0) {
                    report.header("tester", "Testers");
                    for (Path testerFile : testerFiles)
                        runTester(testerFile, timeoutMillis / testerFiles.size(), maxOutputLen / testerFiles.size());
                }

                if (runFiles.size() > 0) {
                    for (Path runFile : runFiles)
                        testInputs(inputs, runFile, okToInterleave);
                }
                // Process checkstyle.xml etc.
                for (Path p : problem.getUseFiles().keySet()) {
                    String cmd = problem.getLanguage().process(p, submissionFiles);
                    if (cmd != null) {
                        printFiles.remove(p);
                        String id = plan.nextID("process");
                        plan.process(id, cmd);
                        plan.addTask(() -> {
                            String result = plan.outerr(id);
                            problem.getLanguage().reportProcessResult(result, report, score);
                        });
                    }
                }
                String remoteURL = resourceLoader.getProperty("com.horstmann.codecheck.comrun.remote");            
                String scriptCommand = resourceLoader.getProperty("com.horstmann.codecheck.comrun.local");  
                if (remoteURL == null && scriptCommand == null) scriptCommand = "/opt/codecheck/comrun";
                plan.execute(report, remoteURL, scriptCommand);
            }
            
            if (!problem.getInputMode()) { // Don't print submitted or provided files for run-only mode
                report.header("studentFiles", "Submitted files");
                /*
                 * Iterate over solutionFiles because submissionFiles may have additional files
                 * when running the codecheck script
                 */
                for (Path p : problem.getSolutionFiles().keySet())   
                    report.file(p.toString(), submissionFiles.get(p));
            
                if (printFiles.size() > 0) {
                    report.header("providedFiles", "Provided files");
                    for (Path p : printFiles)
                        report.file(p.toString(), new String(problem.getUseFiles().get(p), StandardCharsets.UTF_8));
                }
            }
        } catch (Throwable t) {
            if (report != null) report.systemError(t);
            else t.printStackTrace();
        } finally {
            if (report != null) {
                if (scoring && problem != null && !problem.getInputMode()) 
                    report.add(score);
                long endTime = System.currentTimeMillis();
                report.comment("Elapsed", (endTime - startTime) + " ms");
                report.close();
            }
            else System.err.println("report is null");
        }
        return plan;
    }
}
