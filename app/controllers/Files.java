package controllers;

import models.PlayConfig;
import models.Problem;
import models.ProblemData;
import models.Util;
import models.Config;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Result;

public class Files extends Controller {
	private static final Pattern IMG_PATTERN = Pattern
			.compile("[<]\\s*[iI][mM][gG]\\s*[sS][rR][cC]\\s*[=]\\s*['\"]([^'\"]*)['\"][^>]*[>]");

	private static String start = "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" /></head><body style=\"font-family: sans;\">";
	private static String before = "<form method=\"post\" action=\"{0}\" {1}>";

	private static String fileAreaBefore = "<p>{0}</p><textarea name=\"{0}\" rows=\"{1}\" cols=\"66\">";
	private static String fileAreaAfter = "</textarea>";
	private static String after = "<p><input type=\"submit\"/><input type=\"hidden\" name=\"repo\" value=\"{0}\"><input type=\"hidden\" name=\"problem\" value=\"{1}\"><input type=\"hidden\" name=\"level\" value=\"{2}\"></p>";
	private static String callbackTemplate = "<p><input type=\"hidden\" name=\"callback\" value=\"{0}\"></p>";
	private static String end = "</form></body></html>";

	private static String useStart = "<p>Use the following {0,choice,1#file|2#files}:</p>";
	private static String provideStart = "<p>Complete the following {0,choice,1#file|2#files}:</p>";
	
	private static Config config = PlayConfig.INSTANCE;

	class ProblemContext implements AutoCloseable {
		Path unzipDir;
		Path problemPath;
		Problem problem;
		ProblemData data;
		boolean includeCode = true; // TODO: Legacy

		ProblemContext(String repo, String problemName, String level)
				throws IOException {
			if (Util.isOnS3(config, repo)) {
				problemPath = Util.unzipFromS3(repo, problemName);
				unzipDir = problemPath.getParent();
			} else {
				Path repoPath = Paths.get(config.get("com.horstmann.codecheck.repo."
								+ repo));
				// TODO: That comes from Problems.java--fix it there
				if (problemName.startsWith("/"))
					problemName = problemName.substring(1);
				problemPath = repoPath.resolve(problemName);
			}
			problem = new Problem(problemPath, level);
			data = new ProblemData();
			data.description = getDescription(problemPath, "problem.html");
			// TODO: Legacy
			if (data.description == null) {
				data.description = getDescription(problemPath, "statement.html");
				if (data.description == null)
					data.description = getDescription(problemPath,
							"original-statement.html"); // TODO: legacy
				else
					includeCode = false; // code already shown in statement.html
			}

			// TODO: Indicate whether it is ok to add more classes
			// TODO: Should this be a part of the script?
			for (Path p : problem.getRequiredFiles()) {
				String cont = Util.read(problemPath, p);
				data.requiredFiles.put(Util.tail(p).toString(), cont);
			}
			for (Path p : problem.getUseFiles()) {
				String cont = Util.read(problemPath, p);
				if (!Problem.isHidden(cont)) { // TODO: Iffy--how do we know
												// this on the server?
					data.useFiles.put(Util.tail(p).toString(), cont);
				}
			}
		}

		public void close() throws IOException {
			if (unzipDir != null)
				Util.deleteDirectory(unzipDir);
		}
	}

	public Result fileData(String repo, String problem, String level)
			throws IOException {
		try (ProblemContext pc = new ProblemContext(repo, problem, level)) {
			return ok(Json.toJson(pc.data));
		}
	}
/*
	// TODO: Localize
	// TODO: Separate out empty and nonempty files, with "provide" and
	// "complete"

	@GET
	@javax.ws.rs.Path("/{problem}")
	@Produces("text/html")
	public String files(@PathParam("problem") String problem)
			throws IOException {
		return files("ext", problem, "check", "form");
	}

	@GET
	@javax.ws.rs.Path("/{problem}/{level}")
	@Produces("text/html")
	public String files(@PathParam("problem") String problem,
			@PathParam("level") String level) throws IOException {
		return files("ext", problem, level, "form");
	}

	@GET
	@javax.ws.rs.Path("/data")
	// TODO:
	@Produces({ "application/json", "application/xml" })
	public ProblemData fileData(
			@QueryParam("repo") @DefaultValue("ext") String repo,
			@QueryParam("problem") String problemName,
			@DefaultValue("check") @QueryParam("level") String level)
			throws IOException {
		try (ProblemContext pc = new ProblemContext(repo, problemName, level)) {
			return pc.data;
		}
	}
*/
	public Result filesHTML(String repo,
			String problemName,
			String level, String callback)
			throws IOException {
		try (ProblemContext pc = new ProblemContext(repo, problemName, level)) {
			StringBuilder result = new StringBuilder();
			result.append(start);
			if (pc.data.description != null)
				result.append(pc.data.description);
			int nusefiles = pc.data.useFiles.size();
			if (pc.includeCode && nusefiles > 0) {
				result.append(MessageFormat.format(useStart, nusefiles));
				for (Map.Entry<String, String> entry : pc.data.useFiles
						.entrySet()) {
					result.append("<p>");
					result.append(entry.getKey());
					result.append("</p>\n");
					result.append("<pre>");
					result.append(Util.htmlEscape(entry.getValue()));
					result.append("</pre\n>");
				}
			}
			String contextPath = ""; // request().host(); // TODO
			String url = contextPath + "/";
			url += "check";
			result.append(MessageFormat.format(before, url, "")); // TODO
			result.append(MessageFormat.format(provideStart,
					pc.data.requiredFiles.size()));

			// TODO: Remove heuristic for codecomp
			if (pc.data.useFiles.size() == 0
					&& pc.data.requiredFiles.size() == 1)
				pc.includeCode = true;
			for (Map.Entry<String, String> entry : pc.data.requiredFiles
					.entrySet()) {				
				String file = entry.getKey();
				String cont = entry.getValue();
				int lines = 0;
				if (pc.includeCode) {
					lines = Util.countLines(cont);
					if (cont == null)
						cont = "";
				}
				if (lines == 0)
					lines = 20;

				result.append(MessageFormat.format(fileAreaBefore, file,
						lines));
				result.append(Util.htmlEscape(cont));
				result.append(fileAreaAfter);
			}
			result.append(MessageFormat.format(after, repo, problemName, level));
			if (callback != null && callback.length() > 0)
				result.append(MessageFormat.format(callbackTemplate, callback));

			result.append(end);
			return ok(result.toString()).as("text/html");
		} // pc auto-closed
	}

	public static boolean isTest(Path p) {
		String name = p.getFileName().toString();
		int n = name.lastIndexOf(".");
		if (n > 0)
			name = name.substring(0, n); // Remove extension
		n = name.lastIndexOf("Test");
		return n >= 0 && name.substring(n).matches("Test(er)?[0-9]*");
	}

	public static String getDescription(Path problemDir, String problemFile)
			throws IOException {
		String description = Util.read(problemDir, problemFile);
		if (description == null)
			return null;
		// Strip off HTML header. If the file contains "<body>" or "<BODY>",
		String lcdescr = description.toLowerCase();
		int start = lcdescr.indexOf("<body>");
		int end = -1;
		if (start != -1) {
			start += "<body>".length();
			end = lcdescr.lastIndexOf("</body>");
		} else {
			start = lcdescr.indexOf("<html>");
			if (start != -1) {
				start += "<html>".length();
				end = lcdescr.lastIndexOf("</html>");
			}
		}

		StringBuilder result = new StringBuilder(description);
		if (end != -1)
			result.replace(end, result.length(), "");
		if (start != -1)
			result.replace(0, start, "");

		Matcher matcher = IMG_PATTERN.matcher(result);
		start = 0;
		while (matcher.find(start)) {
			start = matcher.start(1);
			end = matcher.end(1);
			String src = result.substring(start, end);
			if (!src.trim().startsWith("http://")) {
				String replacement = "data:image/png;base64,"
						+ Util.base64(problemDir, src);
				result.replace(start, end, replacement);
				start += replacement.length();
			}
		}

		// Old-style Wiley Plus crud
		Path ipf = problemDir.resolve("image.properties");
		if (java.nio.file.Files.exists(ipf)) {
			Properties ip = new Properties();
			ip.load(new FileInputStream(ipf.toFile()));
			String imageToken = "@" + ip.getProperty("image.token") + "@";
			start = result.indexOf(imageToken);
			if (start >= 0) {
				String replacement = "<img src=\"data:image/png;base64,"
						+ Util.base64(problemDir, ip.getProperty("image.file"))
						+ "\"/>";
				result.replace(start, start + imageToken.length(), replacement);
			}
		}
		return result.toString();
	}
}
