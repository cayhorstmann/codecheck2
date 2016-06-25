package controllers;

import models.Config;
import models.PlayConfig;
import models.Util;
import play.Logger;
import play.mvc.Controller;
import play.mvc.Result;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Upload  extends Controller {
	private Path problemDir;
	private String reason;
	private String repo = "ext";
	private Map<String, String> runs = new LinkedHashMap<>();
	
	private static Config config = PlayConfig.INSTANCE;
	
	final Logger.ALogger logger = Logger.of("com.horstmann");

	public Result uploadProblem() {
		try {
			play.mvc.Http.MultipartFormData<File> body = request().body().asMultipartFormData();	
			String problem = Util.createUID();
			Path unzipDir;
			boolean isOnS3 = Util.isOnS3(config, "ext"); 
			if (isOnS3) {
				unzipDir = Files.createTempDirectory("problem");				
			} else {
				unzipDir = java.nio.file.Paths.get(config
						.get("com.horstmann.codecheck.repo.ext"));
				if (!Files.exists(unzipDir)) Files.createDirectory(unzipDir);
			}
			Path problemZip = unzipDir.resolve(problem + ".zip");
			try {
				problemDir = unzipDir.resolve(problem);			
				Files.createDirectory(problemDir);
				File file = body.getFile("file").getFile();
				logger.debug("file=" + file);
				Path savedPath = file.toPath();
				Files.copy(savedPath, problemZip);
				
				fixZip(problemZip);
				
				InputStream in = Files.newInputStream(problemZip);
				Util.unzip(in, problemDir);
				in.close();
	
				if (isOnS3) Util.putToS3(problemZip, repo + ".code-check.org", problem);
				if (check()) {
					boolean grade = runs.keySet().contains("grade");
					boolean multipleLevels = runs.keySet().size() > (grade ? 2
							: 1);
					String url =  "files/" + problem; 
					
					StringBuilder response = new StringBuilder();
					response.append("<html><body style=\"font-family: sans\"><ul style=\"list-style: square\">");
					for (String k : runs.keySet()) {
						response.append("<li>");
						String reportUrl = "fetch/" + runs.get(k);
						if (k.equals("grade")) {
							response.append("<a href=\"");
							response.append(reportUrl);
							response.append("\" target=\"_blank\">Grader report</a>");
						} else {
							String problemUrl = (request().secure() ? "https://" : "http://" ) + request().host() + "/" + url;
							if (multipleLevels) {
								response.append("Level " + k + " ");
								problemUrl += "/" + k;
							}
							response.append("URL: <code>");
							response.append(problemUrl); // TODO: Fix
							response.append("</code> | <a href=\"");
							response.append(problemUrl);
							response.append("\" target=\"_blank\">Preview</a>");
							response.append(" | <a href=\"");
							response.append(reportUrl);
							response.append("\" target=\"_blank\">Report</a>");
						}
						response.append("</li>\n");
					}
					
					response.append("</ul></body></html>\n");
					return ok(response.toString()).as("text/html");
				} else
					if (isOnS3) Util.deleteFromS3(repo + ".code-check.org", problem);
					return badRequest(reason + "\n");
			} finally {
				if (isOnS3) Util.deleteDirectory(unzipDir); else Files.delete(problemZip);
			}
		}
	    catch (Exception ex) {
		    return internalServerError(Util.getStackTrace(ex));
	    }
	}
	
	// In case the zip file contains an initial directory
	// TODO: Crazily wasteful--unzip and then fix
	 private static void fixZip(Path zipPath) throws IOException {
	      try (FileSystem zipfs = FileSystems.newFileSystem(zipPath, null)) {
	          Path root = zipfs.getPath("/");
	          try (Stream<Path> rootEntries = Files.list(root)) {
	              Set<Path> result = rootEntries.filter(Files::isDirectory).collect(Collectors.toSet());
	              if (result.size() == 1) {
	                 Path child = result.iterator().next();
	                 if (!child.getFileName().equals("student")) { // One subdirectory, not named student 
	                	 try (Stream<Path> entries = Files.list(child)) {
	                		result = entries.collect(Collectors.toSet());	                     
	                        Files.walkFileTree(child, new SimpleFileVisitor<Path>() {
	                              public FileVisitResult preVisitDirectory(Path dir,
	                                  BasicFileAttributes attrs) throws IOException {
	                                 int n = dir.getNameCount();
	                                 if (n > 1) {              
	                                    Path q = root.resolve(dir.subpath(1, n));
	                                    Files.createDirectory(q);
	                                 }
	                                 return FileVisitResult.CONTINUE;
	                              }
	                              public FileVisitResult visitFile(Path p,
	                                 BasicFileAttributes attrs) throws IOException {
	                                 int n = p.getNameCount();
	                                 if (n > 1) {
	                                    Path q = root.resolve(p.subpath(1, n));
	                                    Files.move(child.resolve(p), q);
	                                 }
	                                 return FileVisitResult.CONTINUE;
	                              }
	                              public FileVisitResult postVisitDirectory(Path dir,
	                                 IOException e) throws IOException {
	                                 if (e != null) throw e;
	                                 Files.delete(dir);
	                                 return FileVisitResult.CONTINUE;
	                              }
	                           });
	                     }
	                    }
	              }
	             }
	         }
	   }
	 
	 private static boolean isSolution(Path p) throws IOException {
		 try (Scanner in = new Scanner(p)) {
			 return in.hasNextLine() && in.nextLine().contains("SOLUTION"); // TODO: Check delimiters
		 }
	 }
	 
	private boolean check() throws IOException {

		int maxLevel = 1;
		for (int i = 9; i >= 2 && maxLevel == 1; i--)
			// Find highest level
			if (Files.exists(problemDir.resolve("student" + i))
					|| Files.exists(problemDir.resolve("solution" + i)))
				maxLevel = i;

		boolean grade = Files.exists(problemDir.resolve("grader"));
		Path submissionDir = Util.getDir(config, "submissions");
		List<String> solutionSubdirs = new ArrayList<>();
		List<String> studentSubdirs = new ArrayList<>();
		for (int i = 1; i <= (grade ? maxLevel + 1 : maxLevel); i++) {
			Path tempDir = Util.createTempDirectory(submissionDir);
			// Copy solution files up to the current level
			if (i <= maxLevel) 
				solutionSubdirs.add(i == 1 ? "solution" : "solution" + i);
			if (i == 1) studentSubdirs.add("student");
			else if (i == maxLevel + 1) studentSubdirs.add("grader");
			else studentSubdirs.add("student" + i);
			for (Path p : Util.getDescendantFiles(problemDir, solutionSubdirs))
				Files.copy(problemDir.resolve(p), tempDir.resolve(Util.tail(p)));
			Util.forEachFile(problemDir, p -> { 
				if (isSolution(p))
					Files.copy(p, tempDir.resolve(p.getFileName()));
			});
			for (Path p : Util.getDescendantFiles(problemDir, studentSubdirs)) 
				if (isSolution(problemDir.resolve(p)))
					Files.copy(problemDir.resolve(p), tempDir.resolve(Util.tail(p)));

			String problem = problemDir.getFileName().toString();
			String levelString = grade && i == maxLevel + 1 ? "grade" : "" + i;
			Util.runLabrat(config, "html", repo, problem, levelString,
					tempDir.toAbsolutePath(), "");
			// Path reportDir = Util.getDir(context,
			// "reports").resolve(tempDir.getFileName());
			// Files.createDirectory(reportDir);
			// Files.copy(tempDir.resolve("report.html"),
			// reportDir.resolve("report.html"));
			// TODO: Remove temp dir?
			runs.put(levelString, tempDir.getFileName().toString()
					+ "/report.html");
		}
		return true;
	}
	
    public Result fetch(String dir, String file) throws IOException {
        Path submissionDir = Util.getDir(config, "submissions");
        File data = submissionDir.resolve(dir).resolve(file).toFile();
        if (file.endsWith(".html")) { 
        return ok(data).as("text/html");
        }
        else { // JAR
        	return ok(data).as("application/octet-stream");
        }
    }	
}
	
