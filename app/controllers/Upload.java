package controllers;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.script.ScriptException;

import com.typesafe.config.Config;

import models.CodeCheck;
import models.Problem;
import models.S3Connection;
import models.Util;
import play.libs.Files.TemporaryFile;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Upload extends Controller {
	final String repo = "ext";
	@Inject
	private S3Connection s3conn;
	@Inject
	private Config config;
	@Inject
	private CodeCheck codeCheck;

	public Result uploadFiles(Http.Request request) {
		return uploadFiles(request, Util.createPublicUID(), Util.createPrivateUID());
	}

	public Result editedFiles(Http.Request request, String problem, String editKey) {
		try {
			if (checkEditKey(problem, editKey))
				return uploadFiles(request, problem, editKey);
			else
				return badRequest("Wrong edit key " + editKey + " in problem " + problem);
		} catch (IOException ex) {
			return badRequest("Problem not found: " + problem);
		} catch (Exception ex) {
			return internalServerError(Util.getStackTrace(ex));
		}
	}

	public Result uploadFiles(Http.Request request, String problem, String editKey) {
		try {
			Path problemDir = null;
			if (problem == null)
				badRequest("No problem id");
			int n = 1;
			boolean isOnS3 = s3conn.isOnS3("ext");
			if (isOnS3) {
				problemDir = Files.createTempDirectory("problem");
			} else {
				Path extDir = java.nio.file.Paths.get(config.getString("com.horstmann.codecheck.repo.ext"));
				problemDir = extDir.resolve(problem);
				Util.deleteDirectory(problemDir);
				Files.createDirectories(problemDir);
			}
			Map<String, String[]> params = request.body().asFormUrlEncoded();
			while (params.containsKey("filename" + n)) {
				String filename = params.get("filename" + n)[0];
				if (filename.trim().length() > 0) {
					String contents = params.get("contents" + n)[0];
					Util.write(problemDir, filename, contents);
				}
				n++;
			}

			Util.write(problemDir, "edit.key", editKey);
			if (isOnS3) {
				Path problemZip = Files.createTempFile("problem", "zip");
				Util.zip(problemDir, problemZip);
				s3conn.putToS3(problemZip, repo, problem);
				Files.delete(problemZip);
			}
			String response = checkProblem(request, problem, problemDir);
			if (isOnS3)
				Util.deleteDirectory(problemDir);
			return ok(response).as("text/html").addingToSession(request, "pid", problem);
		} catch (Exception ex) {
			return internalServerError(Util.getStackTrace(ex));
		}
	}

	public Result uploadProblem(Http.Request request) {
		return uploadProblem(request, Util.createPublicUID(), Util.createPrivateUID());
	}

	private boolean checkEditKey(String problem, String editKey) throws IOException {
		Path problemDir = null;
		try {
			problemDir = codeCheck.loadProblem(repo, problem);
			String correctEditKey = Util.read(problemDir, "edit.key");
			return correctEditKey != null && editKey.equals(correctEditKey.trim());
		} finally {
			Util.deleteDirectory(problemDir);
		}
	}

	/**
	 * Upload of zip file when editing problem with edit key
	 */
	public Result editedProblem(Http.Request request, String problem, String editKey) {
		try {
			if (checkEditKey(problem, editKey))
				return uploadProblem(request, problem, editKey);
			else
				return badRequest("Wrong edit key " + editKey + " of problem " + problem);
		} catch (IOException ex) {
			return badRequest("Problem not found: " + problem);
		} catch (Exception ex) {
			return internalServerError(Util.getStackTrace(ex));
		}
	}

	public Result uploadProblem(Http.Request request, String problem, String editKey) {
		try {
			play.mvc.Http.MultipartFormData<TemporaryFile> body = request.body().asMultipartFormData();
			if (problem == null)
				badRequest("No problem id");
			Path unzipDir;
			boolean isOnS3 = s3conn.isOnS3("ext");
			if (isOnS3) {
				unzipDir = Files.createTempDirectory("problem");
			} else {
				unzipDir = java.nio.file.Paths.get(config.getString("com.horstmann.codecheck.repo.ext"));
				if (!Files.exists(unzipDir))
					Files.createDirectory(unzipDir);
			}
			Path problemZip = unzipDir.resolve(problem + ".zip");
			try {
				Path problemDir = unzipDir.resolve(problem);
				Util.deleteDirectory(problemDir); // Delete any prior contents so that it is replaced by new zip file
				Files.createDirectories(problemDir);
				Http.MultipartFormData.FilePart<TemporaryFile> tempZipPart = body.getFile("file");
				TemporaryFile tempZipFile = tempZipPart.getRef();
				Path savedPath = tempZipFile.path();
				Files.copy(savedPath, problemZip);

				fixZip(problemZip);
				// TODO: Inefficient to unzip and zip just for the edit key
				InputStream in = Files.newInputStream(problemZip);
				Util.unzip(in, problemDir);
				in.close();
				if (!Files.exists(problemDir.resolve("edit.key")))
					Util.write(problemDir, "edit.key", editKey);

				if (isOnS3) {
					Path problemEditKeyZip = Files.createTempFile("problem", "zip");
					Util.zip(problemDir, problemEditKeyZip);
					s3conn.putToS3(problemEditKeyZip, repo, problem);
					Files.delete(problemEditKeyZip);
				}
				String response = checkProblem(request, problem, problemDir);
				return ok(response).as("text/html").addingToSession(request, "pid", problem);
			} finally {
				if (isOnS3)
					Util.deleteDirectory(unzipDir);
				else
					Files.delete(problemZip);
			}
		} catch (Exception ex) {
			return internalServerError(Util.getStackTrace(ex));
		}
	}

	private String checkProblem(Http.Request request, String problem, Path problemDir)
			throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
		Path newProblemDir = Files.createTempDirectory("problem");
		Util.copyDirectory(problemDir, newProblemDir);
		String studentId = Util.createPronouncableUID();
		codeCheck.replaceParametersInDirectory(studentId, newProblemDir);
		String run = check(problem, newProblemDir, studentId);
		Util.deleteDirectory(newProblemDir);
		StringBuilder response = new StringBuilder();
		response.append(
				"<html><head><title></title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>");
		response.append("<body style=\"font-family: sans\">");
		String prefix = (request.secure() ? "https://" : "http://") + request.host() + "/";
		String problemUrl = prefix + "files/" + problem;
		response.append("Public URL (for your students): ");
		response.append("<a href=\"" + problemUrl + "\" target=\"_blank\">" + problemUrl + "</a>");
		String editKey = Util.read(problemDir, "edit.key");
		if (editKey != null) {
			String editURL = prefix + "private/problem/" + problem + "/" + editKey;
			response.append("<br/>Edit URL (for you only): ");
			response.append("<a href=\"" + editURL + "\" target=\"_blank\">" + editURL + "</a>");
		}
		if (run == null)
			response.append("<p>Fatal error: No report could be generated.</p>");
		else
			response.append(
					"<br/><iframe height=\"400\" style=\"width: 90%; margin: 2em;\" src=\"data:text/html;base64," + run
							+ "\"></iframe>");
		response.append("</li>\n");
		response.append("</ul><p></body></html>\n");
		return response.toString();
	}

	public Result editKeySubmit(Http.Request request, String problem, String editKey) {
		if (problem.equals(""))
			return badRequest("No problem id");
		try {
			Path problemDir = null;
			try {
				problemDir = codeCheck.loadProblem(repo, problem);
				String correctEditKey = Util.read(problemDir, "edit.key");
				if (correctEditKey == null || !editKey.equals(correctEditKey.trim())) {
					return badRequest("Wrong edit key " + editKey + " for problem " + problem);
				}
				Map<String, String> filesAndContents = new TreeMap<>();
				List<Path> entries = Files.list(problemDir).collect(Collectors.toList());
				for (Path f : entries) {
					if (Files.isRegularFile(f))
						filesAndContents.put(f.getFileName().toString(), Util.read(f));
					else
						return badRequest("Cannot edit problem with directories");
				}
				filesAndContents.remove("edit.key");
				return ok(views.html.edit.render(problem, filesAndContents, correctEditKey));
			} finally {
				Util.deleteDirectory(problemDir);
			}
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
								public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
										throws IOException {
									int n = dir.getNameCount();
									if (n > 1) {
										Path q = root.resolve(dir.subpath(1, n));
										Files.createDirectory(q);
									}
									return FileVisitResult.CONTINUE;
								}

								public FileVisitResult visitFile(Path p, BasicFileAttributes attrs) throws IOException {
									int n = p.getNameCount();
									if (n > 1) {
										Path q = root.resolve(p.subpath(1, n));
										Files.move(child.resolve(p), q);
									}
									return FileVisitResult.CONTINUE;
								}

								public FileVisitResult postVisitDirectory(Path dir, IOException e) throws IOException {
									if (e != null)
										throw e;
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
