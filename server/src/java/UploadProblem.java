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
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

@javax.ws.rs.Path("/uploadProblem")
public class UploadProblem {
	@Context
	ServletContext context;
	@Context
	private HttpServletRequest request;

	private Path problemDir;
	private String reason;
	private String repo = "ext";
	private Map<String, String> runs = new LinkedHashMap<>();

	@POST
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.TEXT_HTML)
	public Response uploadProblem(@FormDataParam("file") InputStream in,
			@FormDataParam("file") FormDataContentDisposition fileInfo)
			throws IOException {
		try {
			/*
			String repoPath = context
					.getInitParameter("com.horstmann.codecheck.repo." + repo);
			if (repoPath == null) {
				reason = "No such repo";
				return Response.status(Response.Status.NOT_ACCEPTABLE)
						.entity(reason + "\n").build();
			}
			*/
			

			String problem = Util.createUID();
			Path unzipDir;
			boolean isOnS3 = Util.isOnS3(context, "ext"); 
			if (isOnS3) {
				unzipDir = Files.createTempDirectory("problem");				
			} else {
				unzipDir = java.nio.file.Paths.get(context
						.getInitParameter("com.horstmann.codecheck.repo.ext"));
				if (!Files.exists(unzipDir)) Files.createDirectory(unzipDir);
			}
			Path problemZip = unzipDir.resolve(problem + ".zip");
			try {
				problemDir = unzipDir.resolve(problem);

				Files.createDirectory(problemDir);
				Files.copy(in, problemZip);
				in.close();

				fixZip(problemZip);
				
				in = Files.newInputStream(problemZip);
				Util.unzip(in, problemDir);
				in.close();

				if (isOnS3) Util.putToS3(problemZip, repo + ".code-check.org", problem);
				if (check()) {
					boolean grade = runs.keySet().contains("grade");
					boolean multipleLevels = runs.keySet().size() > (grade ? 2
							: 1);
					String hostUrl = Util.hostURL(request);
					String contextPath = request.getContextPath();
					String url =  contextPath + "/files/" + problem; 
					
					StringBuilder response = new StringBuilder();
					response.append("<html><body style=\"font-family: sans\"><ul style=\"list-style: square\">");
					for (String k : runs.keySet()) {
						response.append("<li>");
						String reportUrl = contextPath + "/fetch/" + runs.get(k);
						if (k.equals("grade")) {
							response.append("<a href=\"");
							response.append(reportUrl);
							response.append("\" target=\"_blank\">Grader report</a>");
						} else {
							String problemUrl = url;
							if (multipleLevels) {
								response.append("Level " + k + " ");
								problemUrl += "/" + k;
							}
							response.append("URL: <code>");
							response.append(hostUrl + problemUrl); // TODO: Fix
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
					return Response.status(Response.Status.OK)
							.entity(response.toString()).build();
				} else
					if (isOnS3) Util.deleteFromS3(repo + ".code-check.org", problem);
					return Response.status(Response.Status.NOT_ACCEPTABLE)
							.entity(reason + "\n").build();
			} finally {
				if (isOnS3) Util.deleteDirectory(unzipDir); else Files.delete(problemZip);
			}
		} catch (Exception ex) {
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
					.entity(ex.getClass() + " " + ex.getMessage()).build();
		}
	}
	
	// In case the zip file contains an initial directory
	 private static void fixZip(Path zipPath) throws IOException {
	      try (FileSystem zipfs = FileSystems.newFileSystem(zipPath, null)) {
	          Path root = zipfs.getPath("/");
	          try (Stream<Path> rootEntries = Files.list(root)) {
	              Set<Path> result = rootEntries.filter(Files::isDirectory).collect(Collectors.toSet());
	              if (result.size() == 1) {
	                 Path child = result.iterator().next();
	                 try (Stream<Path> entries = Files.list(child)) {
	                     result = entries.collect(Collectors.toSet());
	                     if (result.contains(child.resolve("solution/"))) {
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
	 
	private boolean check() throws IOException {
		// TODO: Only good for old-style
		if (!Files.exists(problemDir.resolve("student"))) {
			reason = "No student directory";
			return false;
		}

		int maxLevel = 1;
		for (int i = 9; i >= 2 && maxLevel == 1; i--)
			// Find highest level
			if (Files.exists(problemDir.resolve("student" + i))
					|| Files.exists(problemDir.resolve("solution" + i)))
				maxLevel = i;

		boolean grade = Files.exists(problemDir.resolve("grader"));
		Path submissionDir = Util.getDir(context, "submissions");
		List<String> subdirs = new ArrayList<>();
		for (int i = 1; i <= (grade ? maxLevel + 1 : maxLevel); i++) {
			Path tempDir = Util.createTempDirectory(submissionDir);
			// Copy solution files up to the current level
			if (i <= maxLevel)
				subdirs.add(i == 1 ? "solution" : "solution" + i);
			for (Path p : Util.getDescendantFiles(problemDir, subdirs))
				Files.copy(problemDir.resolve(p), tempDir.resolve(Util.tail(p)));

			String problem = problemDir.getFileName().toString();
			String levelString = grade && i == maxLevel + 1 ? "grade" : "" + i;
			Util.runLabrat(context, repo, problem, levelString,
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
}