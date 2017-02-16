package controllers;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import models.Config;
import models.PlayConfig;
import models.Problem;
import models.ProblemData;
import models.Util;
import play.Logger;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Files extends Controller {
	private static final Pattern IMG_PATTERN = Pattern
			.compile("[<]\\s*[iI][mM][gG]\\s*[sS][rR][cC]\\s*[=]\\s*['\"]([^'\"]*)['\"][^>]*[>]");

	private static String start = "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" /><style type=\"text/css\" media=\"screen\">.studentFiles, .providedFiles { display: none; }\n .ace_editor { border: 3px solid lightgray; height: auto; min-width: 700px; }</style></head><body style=\"font-family: sans;\">";
	private static String before = "<form method=\"post\" action=\"{0}\" {1}>";

	private static String messageScript = "<script src=\"/assets/myReceiveMessage.js\"></script>";

	private static String fileAreaBefore = "\n<p><h3>{0}</h3><textarea id=\"{0}\" name=\"{0}\" rows=\"{1}\" cols=\"80\" class=\"{2}\">";
	private static String fileAreaAfter = "</textarea>";
	private static String after = "<p><input type=\"submit\"/><input type=\"hidden\" name=\"repo\" value=\"{0}\"><input type=\"hidden\" name=\"problem\" value=\"{1}\"><input type=\"hidden\" name=\"level\" value=\"{2}\"></p>";
	private static String callbackTemplate = "<p><input type=\"hidden\" name=\"scoreCallback\" id=\"scoreCallback\" value=\"{0}\"></p>";
	private static String formEnd = "</form>";
	private static String ajaxScriptVariables = "<script>var ajaxResponseType = \"{0}\"; var ajaxDownloadButton = \"{1}\";</script>\n";
	private static String bodyEnd = "</body></html>";

	private static String useStart = "<p>Use the following {0,choice,1#file|2#files}:</p>";
	private static String provideStart = "<p>Complete the following {0,choice,1#file|2#files}:</p>";
	
	private static String submissionCompScript = "<script src=\"/assets/download.js\"></script>";

	private static String jsonpAjaxSubmissionScript = "<script src=\"/assets/myFormSubmit.js\"></script>";

	private static String acePath = "/assets/ace";
	private static String aceScript = "\n<script src=\"{0}/ace.js\"></script>\n<script src=\"{0}/mode-java.js\"></script>\n<script src=\"{0}/mode-c_cpp.js\"></script>\n<script src=\"{0}/mode-plain_text.js\"></script>\n<script src=\"{0}/mode-html.js\"></script>\n<script src=\"{0}/mode-text.js\"></script>\n<script src=\"{0}/ext-language_tools.js\"></script>";
	private static String jqueryPath = "/assets";
	private static String jqueryScript = "\n<script src=\"{0}/jquery.js\"></script>";
	private static String jqueryAceScript = "\n<script src=\"{0}/jquery-ace.js\"></script>";
	private static String myPath = "/assets";
	private static String myButtonScript = "\n<script src=\"{0}/myButton.js\"></script>";
	private static String myAceScript = "\n<script src=\"{0}/myAce.js\"></script>";

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
				problemPath = Util.unzipFromS3(config, repo, problemName);
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

			for (Path p : problem.getRequiredFiles()) {
				String cont = Util.read(problemPath, p);
				data.requiredFiles.put(Util.tail(p).toString(), Problem.processHideShow(p, cont));
			}
			for (Path p : problem.getUseFiles()) {
				String cont = Util.read(problemPath, p);
				cont = Problem.processHideShow(p, cont);
				if (cont.length() > 0) // If it's entirely hidden, don't show its name--this happens with //HIDDEN
					data.useFiles.put(Util.tail(p).toString(), cont);
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

	public Result filesHTML(String repo,
			String problemName,
			String level, String scoreCallback, String type)
			throws IOException {
		try (ProblemContext pc = new ProblemContext(repo, problemName, level)) {
			if (type == null || type.equals("")) type = "json";
			// Safari can't handle download--don't use type json
			String userAgent = request().getHeader("User-Agent");
			Logger.of("com.horstmann.codecheck.files").info("User-Agent: " + userAgent);
			// if (userAgent.contains("Safari") && type.equals("json")) type = "plain";
			
			StringBuilder result = new StringBuilder();
			result.append(start);
			result.append(messageScript);
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
			String url = contextPath + "/check";
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
						lines, "java")); // TODO support more than "java" in ace editor format
				result.append(Util.htmlEscape(cont));
				result.append(fileAreaAfter);
			}
			result.append(MessageFormat.format(after, repo, problemName, level));

			// Include javascripts
			result.append(MessageFormat.format(submissionCompScript, myPath));
			result.append(MessageFormat.format(aceScript, acePath));
			result.append(MessageFormat.format(jqueryScript, jqueryPath));
			result.append(MessageFormat.format(jqueryAceScript, jqueryPath));
			result.append(MessageFormat.format(myButtonScript, myPath));
			result.append(MessageFormat.format(myAceScript, myPath));

			boolean downloadButton = type.equals("json");
			// No AJAX download in interactive elements (jsonp, in Engage)
			
			if (!Util.isEmpty(scoreCallback)) {
				result.append(MessageFormat.format(callbackTemplate, scoreCallback));
				downloadButton = false; // No download with score callback (Sunita's elements)
			}

			result.append(formEnd);
			if (type.equals("json") || type.equals("jsonp")) {
				result.append(MessageFormat.format(ajaxScriptVariables, type, downloadButton));
				result.append(jsonpAjaxSubmissionScript);
			}
			result.append(bodyEnd);
			
	        Http.Cookie ccuCookie = request().cookie("ccu");
			String ccu = ccuCookie == null ? Util.createUID() : ccuCookie.value();					        
	        int age = 180 * 24 * 60 * 60;
	        Http.Cookie newCookie = Http.Cookie.builder("ccu", ccu).withMaxAge(age).build();
			return ok(result.toString()).withCookies(newCookie).as("text/html");
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
			String src = result.substring(start, end).trim();
			if (!(src.startsWith("http://") || src.startsWith("https://"))) {
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