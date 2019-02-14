package models;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.typesafe.config.Config;

import play.api.Environment;

@Singleton
public class CodeCheck {
	@Inject private Config config;
	@Inject private S3Connection s3conn;
	public static final int TIMEOUT = (2 * 60 + 15) * 1000; // 2 minutes 15 seconds 

	public Path createSubmissionDirectory()
			throws IOException {
		String dirName = config.getString("com.horstmann.codecheck.submissions");
		Path dir = Paths.get(dirName);
		if (!Files.exists(dir))
			Files.createDirectory(dir);
		return Util.createTempDirectory(dir);
	}	
	
	public Path loadProblem(String repo, String problemName, String studentId) throws IOException, ScriptException, NoSuchMethodException {
		Path problemPath = loadProblem(repo, problemName);
		
		replaceParametersInDirectory(studentId, problemPath);
		return problemPath;
	}

	@Inject Environment playEnv;
	
	public void replaceParametersInDirectory(String studentId, Path problemPath)
			throws ScriptException, NoSuchMethodException, IOException {
		Path paramPath = problemPath.resolve("param.js"); 
		if (Files.exists(paramPath)) {
			ScriptEngineManager engineManager = new ScriptEngineManager();
			ScriptEngine engine = engineManager.getEngineByName("nashorn");
			String preload = "public/preload.js";
			InputStream in = playEnv.classLoader().getResourceAsStream(preload);
			engine.eval(new InputStreamReader(in, StandardCharsets.UTF_8));
			//seeding unique student id
			((Invocable) engine).invokeMethod(engine.get("Math"), "seedrandom", studentId);
			engine.eval(Files.newBufferedReader(paramPath));
			Files.delete(paramPath);
			for (Path p : Util.filterNot(Util.getDescendantFiles(problemPath), "*.jar", "*.gif", "*.png", "*.jpg", "*.wav"))
				replaceParametersInFile(problemPath.resolve(p), engine);
		}
	}
	
	private void replaceParametersInFile(Path p, ScriptEngine engine) throws ScriptException, IOException {
		String contents = Util.read(p);
		if (contents == null) return; // Happens if not UTF-8
		String leftDelimiter = (String) engine.eval("delimiters[0]");
		int leftLength = leftDelimiter.length();
		String rightDelimiter = (String) engine.eval("delimiters[1]");
		int rightLength = rightDelimiter.length();
		StringBuilder result = new StringBuilder();
		// TODO: Use length of delimiters
		int from = 0;
		int to = -rightLength;
		boolean done = false;
		while (!done) {
			from = contents.indexOf(leftDelimiter, to + rightLength);
			if (from == -1) {
				if (to == -1) return; // No delimiter in file
				else {
					result.append(contents.substring(to + rightLength));
					done = true;
				}
			} else {
				int nextTo = contents.indexOf(rightDelimiter, from + leftLength);
				if (nextTo == -1) return; // Delimiters don't match--might be binary file
				else {
					result.append(contents.substring(to + rightLength, from));
					to = nextTo;
					String toEval = contents.substring(from + leftLength, to);
					if (toEval.contains(leftDelimiter)) return; // Nested
					result.append("" + engine.eval(toEval));
				}
			}
		}
		Files.write(p, result.toString().getBytes(StandardCharsets.UTF_8));
	}
	
	public Path loadProblem(String repo, String problemName) throws IOException {
		if (s3conn.isOnS3(repo)) {
			return s3conn.unzipFromS3(repo, problemName);
		} else {
			Path repoPath = Paths.get(config.getString("com.horstmann.codecheck.repo."
							+ repo));
			// TODO: That comes from Problems.java--fix it there
			if (problemName.startsWith("/"))
				problemName = problemName.substring(1);
			Path problemDir = java.nio.file.Files.createTempDirectory("problem");
			Util.copyDirectory(repoPath.resolve(problemName), problemDir);			
			return problemDir;
		}
	}
	
	public void run(String reportType, String repo,
			String problem, String ccu, Path submissionDir)
			throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
		Path problemDir = loadProblem(repo, problem, ccu);
		run(reportType, repo, problem, problemDir, submissionDir,
				"User=" + ccu, "Problem=" + (repo + "/" + problem).replaceAll("[^\\pL\\pN_/-]", ""));
		Util.deleteDirectory(problemDir);
	}

	private void run(String reportType, String repo,
			String problem, Path problemDir,
			Path submissionDir, String... metaData) throws IOException, InterruptedException {
		String command = config.getString("com.horstmann.codecheck." + reportType);
		StringBuilder metas = new StringBuilder();
		for (String meta : metaData) { if (metas.length() > 0) metas.append(" "); metas.append(meta); }
		
		String script = MessageFormat.format(command, submissionDir.toAbsolutePath(),
				problemDir.toAbsolutePath(), metas); 
		
		Files.write(submissionDir.resolve("codecheck.log"), script.getBytes("UTF-8"));
		ProcessBuilder builder = new ProcessBuilder(script.split(" "));
        builder.redirectErrorStream(true);        
        builder.redirectOutput(submissionDir.resolve("codecheck.out").toFile());
        try {
        	Process process = builder.start();
        	boolean completed = process.waitFor(TIMEOUT, TimeUnit.MILLISECONDS);
        	if (!completed) process.destroyForcibly();
        } catch (Exception ex) {
        	ex.printStackTrace();
        	throw ex;
        }
	}	
}