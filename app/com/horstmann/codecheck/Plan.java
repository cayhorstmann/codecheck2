package com.horstmann.codecheck;

import java.io.IOException;
import java.net.SocketTimeoutException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

public class Plan {
    private Language language;
    private List<Runnable> tasks = new ArrayList<>();
    private Map<Path, byte[]> files = new Util.FileMap();
    private Map<Path, byte[]> outputs = new Util.FileMap();
    private StringBuilder scriptBuilder = new StringBuilder();
    private Path workDir;
    private int nextID = 0;
    private static int MIN_TIMEOUT = 3; // TODO: Maybe better to switch interleaveio and timeout? 
    
    public Plan(Language language) throws IOException {
        this.language = language;
        workDir = Util.createTempDirectory();
        if (Main.DEBUG) addScript("debug");
    }
    
    public Path getWorkDir() {
        return workDir;
    }
    
    public String nextID(String prefix) {
        nextID++;
        return prefix + nextID;
    }

    public void addFile(Path path, String contents) {
        files.put(path, contents.getBytes(StandardCharsets.UTF_8));
    }
    public void addFile(Path path, byte[] contents) {
        files.put(path, contents);
    }
    public void addTask(Runnable task) { tasks.add(task); }
    
    private void addScript(CharSequence command) {
        scriptBuilder.append(command);
        scriptBuilder.append("\n");
    }

    public boolean checkCompiled(String compileDir, Report report, Score score) {
        String errorReport = getOutputString(compileDir + "/_errors");
        if (errorReport == null) return true;         
        if (errorReport.trim().equals(""))
        report.error("Compilation Failed");
        else {
            report.error(errorReport);
            report.errors(language.errors(errorReport, true));
        }
        score.setInvalid();
        return false;
    }

    public boolean compiled(String compileDir) {
        return !outputs.containsKey(Paths.get(compileDir).resolve("_errors"));
    }
    
    public String outerr(String runID) {
        byte[] output = outputs.get(Paths.get(runID).resolve("_run"));
        if (output == null) throw new NoSuchElementException(runID + "/_run");
        else return new String(output, StandardCharsets.UTF_8);
    }
    
    public String getFileString(String key, Path file) {
        byte[] output = files.get(Paths.get(key).resolve(file));
        if (output == null) return null;
        else return new String(output, StandardCharsets.UTF_8);
    }

    public String getOutputString(String key, String...keys) {
        byte[] output = outputs.get(Paths.get(key, keys));
        if (output == null) return null;
        else return new String(output, StandardCharsets.UTF_8);
    }
    
    public String getOutputString(String key, Path path) {
        byte[] output = outputs.get(Paths.get(key).resolve(path));
        if (output == null) return null;
        else return new String(output, StandardCharsets.UTF_8);
    }

    public byte[] getOutputBytes(String key, String...keys) {
        return outputs.get(Paths.get(key, keys));
    }
    
    public void compile(String compileDir, String sourceDirs, Path mainFile, Collection<Path> dependentSourceFiles) {
        compile(compileDir, sourceDirs, Collections.singletonList(mainFile), dependentSourceFiles);
    }
    
    /**
     * Runs the compiler
     * @param compileDir the directory in which to compile
     * @param sourceDirs space-separated list of directories to copy into the compile directory (use is always copied)
     * @param sourceFiles a list of paths, starting with the path to the main source file
     * @return true if compilation succeeds
     */
    public void compile(String compileDir, String sourceDirs, List<Path> sourceFiles, Collection<Path> dependentSourceFiles) {
        List<Path> allSourceFiles = new ArrayList<>();
        allSourceFiles.addAll(sourceFiles);
        allSourceFiles.addAll(dependentSourceFiles);
        addScript("prepare " + compileDir + " use " + sourceDirs);
        addScript("compile " + compileDir + " " + language.getLanguage() + " " + Util.join(allSourceFiles, " "));
    }

    // TODO maxOutputLen
    public void run(String compileDir, String runDir, Path mainFile, String input, String args, int timeout, int maxOutputLen, boolean interleaveIO) {
        run(compileDir, runDir, runDir, mainFile, input, args, timeout, maxOutputLen, interleaveIO);        
    }
    
    // for multiple runs in the same directory
    public void run(String compileDir, String runDir, String runID, Path mainFile, String input, String args, int timeout, int maxOutputLen, boolean interleaveIO) {
        if (!compileDir.equals(runDir)) 
            addScript("prepare " + runDir + " " + compileDir);
        addFile(Paths.get("in").resolve(runID), input == null ? "" : input);
        addScript("run " + runDir + " " + runID + " " + Math.max(MIN_TIMEOUT, (timeout + 500) / 1000) + " " + maxOutputLen + " " + interleaveIO + " " + language.getLanguage() + " " + mainFile + (args == null ? "" : " " + args));
    }

    public void run(String compileDir, String runDir, Path mainFile, String args, String input, Collection<String> outfiles, int timeout, int maxOutputLen, boolean interleaveIO) {
        run(compileDir, runDir, mainFile, input, args, timeout, maxOutputLen, interleaveIO);
        if (outfiles.size() > 0)
            addScript("collect " + runDir + " " + Util.join(outfiles, " "));
    }
    
    public void unitTest(String dir, Path mainFile, Collection<Path> dependentSourceFiles, int timeout, int maxOutputLen) {
        addScript("prepare " + dir + " use submission");
        addScript("unittest " + dir + " " + Math.max(MIN_TIMEOUT, (timeout + 500) / 1000) + " " + " " + language.getLanguage() + " " + mainFile + " " + Util.join(dependentSourceFiles, " "));
    }    
    
    public void process(String dir, String cmd) {
        addScript("prepare " + dir + " use submission");
        addScript("process " + dir + cmd);
    }
        
    public void execute(Report report) throws IOException, InterruptedException {
        files.put(Paths.get("script"), scriptBuilder.toString().getBytes(StandardCharsets.UTF_8));
        String remoteURL = System.getProperty("com.horstmann.codecheck.remote");
        if (remoteURL == null)
            executeLocally(report);
        else
            executeRemotely(remoteURL, report);
        for (Runnable task : tasks) 
            task.run(); 
    }

    private void executeLocally(Report report)
            throws IOException, InterruptedException {
        try {
            for (Map.Entry<Path, byte[]> entry : files.entrySet()) {
                Path target = workDir.resolve(entry.getKey());
                Files.createDirectories(target.getParent());
                Files.write(target, entry.getValue());
            }
            Path scriptFile = workDir.resolve("script");
            Set<PosixFilePermission> perms = Files.getPosixFilePermissions(scriptFile);
            perms.add(PosixFilePermission.OWNER_EXECUTE); // TODO Which permission? 
            Files.setPosixFilePermissions(scriptFile, perms);
            ProcessBuilder builder = new ProcessBuilder("./script").directory(workDir.toFile());            
            Process process = builder.start();
            int millis = 30000; // TODO
            boolean completed = process.waitFor(millis, TimeUnit.MILLISECONDS);
            if (!completed) {
                process.destroyForcibly();
                report.systemError("\nTimeout after " + millis + " milliseconds\n");
            }            
            Path outDir = workDir.resolve("out");            
            try (Stream<Path> entries = Files.walk(outDir)) 
            {
                entries.forEach(entry -> {
                    if (Files.isRegularFile(entry))
                        outputs.put(outDir.relativize(entry), Util.readBytes(entry));
                });
            }
        } finally {
            if (!Main.DEBUG) Util.deleteDirectory(workDir);
        }
    }
    
    private void executeRemotely(String remoteURL, Report report)
            throws IOException {
        byte[] requestZip = Util.zip(files);
        int retries = 2;
        while (retries > 0) {
            try {
                byte[] responseZip = Util.fileUpload(remoteURL, "job", "job.zip", requestZip);
                outputs = Util.unzip(responseZip);
                if (Main.DEBUG) {
                    Path temp = Files.createTempFile("codecheck", ".zip");
                    System.out.println("Remote result at " + temp);
                    Files.write(temp, responseZip);
                }
                return;
            } catch (IOException ex) {
                retries--;
                if (retries == 0 || !(ex.getMessage().startsWith("Status: 5") || ex instanceof SocketTimeoutException)) // TODO: More elegant
                    throw ex;
            }
        }
    }
}
