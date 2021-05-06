package models;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStore;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.horstmann.codecheck.Main;
import com.horstmann.codecheck.Report;
import com.horstmann.codecheck.ResourceLoader;
import com.horstmann.codecheck.Util;
import com.typesafe.config.Config;

import jdk.security.jarsigner.JarSigner;
import play.Logger;
import play.api.Environment;

@Singleton
public class CodeCheck {
	private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");	
	private Config config;
	private S3Connection s3conn;
	private JarSigner signer;
	private ResourceLoader resourceLoader;
	
	@Inject public CodeCheck(Config config, S3Connection s3conn, Environment playEnv) {
		this.config = config;
		this.s3conn = s3conn;
		resourceLoader = new ResourceLoader() {
			@Override
			public InputStream loadResource(String path) throws IOException {
				return playEnv.classLoader().getResourceAsStream("public/resources/" + path);
			}
			@Override
			public String getProperty(String key) {
				return config.hasPath(key) ? config.getString(key) : null;
			}
		};
		try {
			String keyStorePath = config.getString("com.horstmann.codecheck.storeLocation");
			char[] password = config.getString("com.horstmann.codecheck.storePassword").toCharArray();
	        KeyStore ks = KeyStore.getInstance(new File(keyStorePath), password);
	        KeyStore.ProtectionParameter protParam = new KeyStore.PasswordProtection(password);	
	        KeyStore.PrivateKeyEntry pkEntry = (KeyStore.PrivateKeyEntry) ks.getEntry("codecheck", protParam);
		    signer = new JarSigner.Builder(pkEntry).build();			
		} catch (Exception e) {
			logger.warn("Cannot load keystore", e);
		}
	}
	

	public Map<Path, byte[]> loadProblem(String repo, String problemName, String studentId) throws IOException, ScriptException, NoSuchMethodException {
		Map<Path, byte[]> problemFiles = loadProblem(repo, problemName);
		
		replaceParametersInDirectory(studentId, problemFiles);
		return problemFiles;
	}

	public void replaceParametersInDirectory(String studentId, Map<Path, byte[]> problemFiles)
			throws ScriptException, NoSuchMethodException, IOException {
		Path paramPath = Paths.get("param.js"); 
		if (problemFiles.containsKey(paramPath)) {
			ScriptEngineManager engineManager = new ScriptEngineManager();
			ScriptEngine engine = engineManager.getEngineByName("nashorn");
			InputStream in = resourceLoader.loadResource("preload.js");
			engine.eval(new InputStreamReader(in, StandardCharsets.UTF_8));
			//seeding unique student id
			((Invocable) engine).invokeMethod(engine.get("Math"), "seedrandom", studentId);
			engine.eval(Util.getString(problemFiles, paramPath));
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
			return Util.unzip(s3conn.readFromS3(repo, problemName));
		} else {
			Path repoPath = Paths.get(config.getString("com.horstmann.codecheck.repo."
							+ repo));
			// TODO: That comes from Problems.java--fix it there
			if (problemName.startsWith("/"))
				problemName = problemName.substring(1);
			return Util.descendantFiles(repoPath.resolve(problemName));			
		}
	}
	
	public Report run(String reportType, String repo,
			String problem, String ccid, Map<Path, String> submissionFiles)
			throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
		Map<Path, byte[]> problemFiles = loadProblem(repo, problem, ccid);
		Properties metaData = new Properties();
		metaData.put("User", ccid);
		metaData.put("Problem", (repo + "/" + problem).replaceAll("[^\\pL\\pN_/-]", ""));
		
		return new Main().run(submissionFiles, problemFiles, reportType, metaData, resourceLoader);
	}
	
	
    public byte[] signZip(Map<Path, byte[]> contents) throws IOException {  
    	if (signer == null) return Util.zip(contents);
    	Path tempFile = Files.createTempFile(null, ".zip");
    	OutputStream fout = Files.newOutputStream(tempFile);
        ZipOutputStream zout = new ZipOutputStream(fout);
        for (Map.Entry<Path, byte[]> entry : contents.entrySet()) {
           ZipEntry ze = new ZipEntry(entry.getKey().toString());
           zout.putNextEntry(ze);
           zout.write(entry.getValue());
           zout.closeEntry();
        }
        zout.close();	    	
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try (ZipFile in = new ZipFile(tempFile.toFile())) {
           signer.sign(in, out);
        }
        Files.delete(tempFile);
        return out.toByteArray(); 
	}	
}