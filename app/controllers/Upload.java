package controllers;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.script.ScriptException;

import models.CodeCheck;
import models.S3Connection;
import models.Util;
import models.Problem;

import com.typesafe.config.Config;

import play.Logger;
import play.mvc.Controller;
import play.mvc.Result;
import views.html.*;

public class Upload  extends Controller {	
	final Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
	final String repo = "ext";
	@Inject private S3Connection s3conn;
	@Inject private Config config;
	@Inject private CodeCheck codeCheck;
	
	public Result uploadFiles() {
		return uploadFiles(Util.createUID(), Util.randomString(16));
	}
	
	public Result editedFiles(String problem, String editKey) {
		try {
			Path problemDir = codeCheck.loadProblem(repo, problem);
			String editKeyCorrect = Util.read(problemDir, ".editKey");
			if (!editKeyCorrect.equals(editKey)) {
				return badRequest("Wrong Edit Key: " + editKey + "\nProblem found: " + problem);
			}
			
    		Util.deleteDirectory(problemDir);
			return uploadFiles(problem, editKeyCorrect);
			
		} catch (IOException ex) {
		    return badRequest("Problem not found: " + problem);
	    } catch (Exception ex) {
		    return internalServerError(Util.getStackTrace(ex));
	    }	
	}

	public Result uploadFiles(String problem, String editKey) {
		try {
			Path problemDir = null;			
			if (problem == null) badRequest("No problem id");
			int n = 1;
			boolean isOnS3 = s3conn.isOnS3("ext");
			if (isOnS3) {
				problemDir = Files.createTempDirectory("problem");						
			} else {
				Path extDir = java.nio.file.Paths.get(config
						.getString("com.horstmann.codecheck.repo.ext"));
				problemDir = extDir.resolve(problem);
				Util.deleteDirectory(problemDir);
				Files.createDirectories(problemDir);
			}
			Map<String, String[]> params = request().body().asFormUrlEncoded();
			while (params.containsKey("filename" + n)) {
				String filename = params.get("filename" + n)[0];
				if (filename.trim().length() > 0) {
					String contents = params.get("contents" + n)[0];
					Util.write(problemDir, filename, contents);
				}
				n++;
			}

			Util.write(problemDir, ".editKey", editKey);
			if (isOnS3) {
				Path problemZip = Files.createTempFile("problem", "zip");
				Util.zip(problemDir, problemZip);
				s3conn.putToS3(problemZip, repo + "." + config.getString("com.horstmann.codecheck.s3bucketsuffix"), problem);
				Files.delete(problemZip);
			}
			String response = checkProblem(problem, problemDir);
			if (isOnS3) Util.deleteDirectory(problemDir);
			return ok(response).as("text/html");
		}
	    catch (Exception ex) {
		    return internalServerError(Util.getStackTrace(ex));
	    }
	}
	
	public Result uploadProblem() {
		return uploadProblem(Util.createUID(), Util.randomString(16));
	}
	
	public Result editedProblem(String problem, String editKey) {
		try {
			Path problemDir = codeCheck.loadProblem(repo, problem);
			String editKeyCorrect = Util.read(problemDir, ".editKey");
			if (!editKeyCorrect.equals(editKey)) {
				return badRequest("Wrong Edit Key: " + editKey + "\nProblem found: " + problem);
			}
			
    		Util.deleteDirectory(problemDir);
    		return uploadProblem(problem, editKey);
			
		} catch (IOException ex) {
		    return badRequest("Problem not found: " + problem);
	    } catch (Exception ex) {
		    return internalServerError(Util.getStackTrace(ex));
	    }
	}
	
	public Result uploadProblem(String problem, String editKey) {		
		try {
			play.mvc.Http.MultipartFormData<File> body = request().body().asMultipartFormData();	
			if (problem == null) badRequest("No problem id");
			Path unzipDir;
			boolean isOnS3 = s3conn.isOnS3("ext"); 
			if (isOnS3) {
				unzipDir = Files.createTempDirectory("problem");				
			} else {
				unzipDir = java.nio.file.Paths.get(config
						.getString("com.horstmann.codecheck.repo.ext"));
				if (!Files.exists(unzipDir)) Files.createDirectory(unzipDir);
			}
			Path problemZip = unzipDir.resolve(problem + ".zip");
			try {
				Path problemDir = unzipDir.resolve(problem);			
				Files.createDirectories(problemDir);
				File file = body.getFile("file").getFile();
				logger.debug("file=" + file);
				Path savedPath = file.toPath();
				Files.copy(savedPath, problemZip);
				
				fixZip(problemZip);
				
				InputStream in = Files.newInputStream(problemZip);
				Util.unzip(in, problemDir);
				in.close();
				
				Util.write(problemDir, ".editKey", editKey);
	
				if (isOnS3) {
					Path problemEditKeyZip = Files.createTempFile("problem", "zip");
					Util.zip(problemDir, problemEditKeyZip);
					s3conn.putToS3(problemEditKeyZip, repo + "." + config.getString("com.horstmann.codecheck.s3bucketsuffix"), problem);
					Files.delete(problemEditKeyZip);
				}
				String response = checkProblem(problem, problemDir);
				return ok(response).as("text/html");
			} finally {
				if (isOnS3) Util.deleteDirectory(unzipDir); else Files.delete(problemZip);
			}
		}
	    catch (Exception ex) {
		    return internalServerError(Util.getStackTrace(ex));
	    }
	}

	private String checkProblem(String problem, Path problemDir)
			throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
		Path newProblemDir = Files.createTempDirectory("problem");
		Util.copyDirectory(problemDir, newProblemDir);
		String studentId = Util.createUID();
		codeCheck.replaceParametersInDirectory(studentId, newProblemDir);
		String run = check(problem, newProblemDir, studentId);
		Util.deleteDirectory(newProblemDir);
		String url =  "files/" + problem; 
		
		StringBuilder response = new StringBuilder();
		response.append("<html><head><title></title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>");
		response.append("<body style=\"font-family: sans\">");
		String problemUrl = (request().secure() ? "https://" : "http://" ) + request().host() + "/" + url;
		String editURL = (request().secure() ? "https://" : "http://" ) + request().host() + "/edit/" + problem + "/" + Util.read(problemDir, ".editKey");
		response.append("URL: ");
		response.append("<a href=\"" + problemUrl + "\" target=\"_blank\">" + problemUrl + "</a>");
		response.append("<br/>Edit URL: ");
		response.append("<a href=\"" + editURL + "\" target=\"_blank\">" + editURL + "</a>");
		if (run == null)
			response.append("<p>Fatal error: No report could be generated.</p>");
		else
			response.append("<br/><iframe height=\"400\" style=\"width: 90%; margin: 2em;\" src=\"data:text/html;base64," + run + "\"></iframe>");			
		response.append("</li>\n");
		session().put("pid", problem);
		response.append("</ul><p></body></html>\n");
		return response.toString();
	}
	
	public Result editKeySubmit(String problem, String editKeySubmit) {
		if (problem.equals("")) return badRequest("No Problem ID entered");
		
		try {
			Path problemDir = codeCheck.loadProblem(repo, problem);
			String editKey = Util.read(problemDir, ".editKey");
			if (!editKeySubmit.equals(editKey)) {
				return badRequest("Wrong Edit Key: " + editKeySubmit + "\nProblem found: " + problem);
			}
			
			Map<String, String> filesAndContents = new TreeMap<>();
			List<Path> entries = Files.list(problemDir).collect(Collectors.toList());
			for (Path f : entries) {
				if (Files.isRegularFile(f)) filesAndContents.put(f.getFileName().toString(), Util.read(f));
				else return badRequest("Cannot edit problem with directories");
			};
    		Util.deleteDirectory(problemDir);
    		filesAndContents.remove(".editKey");
			return ok(edit.render(problem, filesAndContents, editKey));
			
		} catch (IOException ex) {
		    return badRequest("Problem not found: " + problem);
	    } catch (Exception ex) {
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
	              result.remove(zipfs.getPath("/__MACOSX/")); // Zip directory paths end in /
	              if (result.size() == 1) {
	                 Path child = result.iterator().next();
	                 if (!child.toString().equals("/student/")) { // One subdirectory, not named student 
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
	  
	private String check(String problem, Path problemDir, String studentId) 
			throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
		Path submissionDir = codeCheck.createSubmissionDirectory();
		// Copy solution files 
		Path solutionDir = problemDir.resolve("solution");
		if (Files.exists(solutionDir)) {
			for (Path p : Util.getDescendantFiles(solutionDir))
				Files.copy(solutionDir.resolve(p), submissionDir.resolve(p));
		} else {
			boolean runMode = Files.exists(problemDir.resolve("Input"));						
			Util.forEachFile(problemDir, p -> { 
				if (runMode || Problem.isSolution(p))
					Files.copy(p, submissionDir.resolve(p.getFileName()));
			});
		}
		codeCheck.run("html", repo, problem, studentId, submissionDir);
		return Util.base64(submissionDir, "report.html");
	}	
}
	
