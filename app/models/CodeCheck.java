package models;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.horstmann.codecheck.Main;
import com.horstmann.codecheck.Report;
import com.typesafe.config.Config;

import play.api.Environment;

@Singleton
public class CodeCheck {
	@Inject private Config config;
	@Inject private S3Connection s3conn;

	public Map<Path, byte[]> loadProblem(String repo, String problemName, String studentId) throws IOException, ScriptException, NoSuchMethodException {
		Map<Path, byte[]> problemFiles = loadProblem(repo, problemName);
		
		replaceParametersInDirectory(studentId, problemFiles);
		return problemFiles;
	}

	@Inject Environment playEnv;
	
	public void replaceParametersInDirectory(String studentId, Map<Path, byte[]> problemFiles)
			throws ScriptException, NoSuchMethodException, IOException {
		Path paramPath = Paths.get("param.js"); 
		if (problemFiles.containsKey(paramPath)) {
			ScriptEngineManager engineManager = new ScriptEngineManager();
			ScriptEngine engine = engineManager.getEngineByName("nashorn");
			String preload = "public/preload.js";
			InputStream in = playEnv.classLoader().getResourceAsStream(preload);
			engine.eval(new InputStreamReader(in, StandardCharsets.UTF_8));
			//seeding unique student id
			((Invocable) engine).invokeMethod(engine.get("Math"), "seedrandom", studentId);
			engine.eval(Files.newBufferedReader(paramPath));
			Files.delete(paramPath);
			for (Path p : Util.filterNot(problemFiles.keySet(), "*.jar", "*.gif", "*.png", "*.jpg", "*.wav")) {
				String contents = new String(problemFiles.get(p), StandardCharsets.UTF_8);
				String result = replaceParametersInFile(contents, engine);
				if (result != null)
					problemFiles.put(p, result.getBytes(StandardCharsets.UTF_8));				
			}
		}
	}
	
	private String replaceParametersInFile(String contents, ScriptEngine engine) throws ScriptException, IOException {
		if (contents == null) return null; // Happens if not UTF-8
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
				if (to == -1) return null; // No delimiter in file
				else {
					result.append(contents.substring(to + rightLength));
					done = true;
				}
			} else {
				int nextTo = contents.indexOf(rightDelimiter, from + leftLength);
				if (nextTo == -1) return null; // Delimiters don't match--might be binary file
				else {
					result.append(contents.substring(to + rightLength, from));
					to = nextTo;
					String toEval = contents.substring(from + leftLength, to);
					if (toEval.contains(leftDelimiter)) return null; // Nested
					result.append("" + engine.eval(toEval));
				}
			}
		}
		return result.toString();
	}
	
	public Map<Path, byte[]> loadProblem(String repo, String problemName) throws IOException {
		if (s3conn.isOnS3(repo)) {
			return com.horstmann.codecheck.Util.unzip(s3conn.readFromS3(repo, problemName));
		} else {
			Path repoPath = Paths.get(config.getString("com.horstmann.codecheck.repo."
							+ repo));
			// TODO: That comes from Problems.java--fix it there
			if (problemName.startsWith("/"))
				problemName = problemName.substring(1);
			return com.horstmann.codecheck.Util.descendantFiles(repoPath.resolve(problemName));			
		}
	}
	
	public Report run(String reportType, String repo,
			String problem, String ccid, Map<Path, String> submissionFiles)
			throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
		Map<Path, byte[]> problemFiles = loadProblem(repo, problem, ccid);
		Properties metaData = new Properties();
		metaData.put("User", ccid);
		metaData.put("Problem", (repo + "/" + problem).replaceAll("[^\\pL\\pN_/-]", ""));
		
		return run(reportType, repo, problem, problemFiles, submissionFiles, metaData);
	}

	private Report run(String reportType, String repo,
			String problem, Map<Path, byte[]> problemFiles,
			Map<Path, String> submissionFiles, Properties metaData) throws IOException, InterruptedException {
		Properties properties = new Properties();
		properties.put("com.horstmann.codecheck.report", reportType);
		properties.put("com.horstmann.codecheck.remote", config.getString("com.horstmann.codecheck.remote"));
		properties.put("com.horstmann.codecheck.debug", config.getString("com.horstmann.codecheck.debug"));
		
		Report report = new Main().run(submissionFiles, problemFiles, properties, metaData);
		return report;
	}	
}