package controllers;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;
import javax.script.ScriptException;

import models.CodeCheck;
import models.Problem;
import models.ProblemData;
import models.Util;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Files extends Controller {
	private static final Pattern IMG_PATTERN = Pattern
			.compile("[<]\\s*[iI][mM][gG]\\s*[sS][rR][cC]\\s*[=]\\s*['\"]([^'\"]*)['\"][^>]*[>]");

	private static String start = "<!DOCTYPE html>\n<html><head>\n" 
			+ "<meta http-equiv='content-type' content='text/html; charset=UTF-8' />\n" 
			+ "<script src='/assets/jquery.js'></script>\n" 
			+ "<script src='/assets/download.js'></script>\n" 
			+ "<script src='/assets/ace/ace.js'></script>\n" 
			+ "<script src='/assets/codecheck.js'></script>\n" 
			+ "<link type='text/css' rel='stylesheet' href='/assets/codecheck.css'/>\n"
			+ "</head><body>\n";
	private static String before = "<form method=\"post\" action=\"{0}\">\n";

	private static String fileAreaBefore = "<div id=\"{0}\" name=\"{0}\" rows=\"{1}\" cols=\"80\" class=\"editor {2}\">";
	private static String fileAreaBeforeNoEdit = "<div id=\"{0}\" name=\"{0}\" rows=\"{1}\" cols=\"{2}\" class=\"editor readonly {3}\">";	
	private static String fileAreaAfter = "</div>\n";
	
	private static String fileOuterDiv = "<div id=\"{0}\" class=\"file\">\n";
	private static String fileOuterDivAfter = "</div>\n";
	
	private static String after = "<div><input id=\"submit\" type=\"submit\"/>\n" 
			+ "<input type=\"hidden\" name=\"repo\" value=\"{0}\">\n" 
			+ "<input type=\"hidden\" name=\"problem\" value=\"{1}\"/>\n" 
			+ "<input type=\"hidden\" name=\"level\" value=\"{2}\"/></div>\n";
	private static String formEnd = "</form>\n<div id=\"codecheck-submit-response\"></div>\n";
	private static String bodyEnd = "</body></html>";

	private static String useStart = "<p>Use the following {0,choice,1#file|2#files}:</p>\n";
	private static String provideStart = "<p>Complete the following {0,choice,1#file|2#files}:</p>\n";

	@Inject CodeCheck codeCheck;

	class ProblemContext implements AutoCloseable {
		Path problemPath;
		Problem problem;
		ProblemData data;
		boolean includeCode = true; // TODO: Legacy
		
		ProblemContext(String repo, String problemName, String level, String sid)
				throws IOException, ScriptException, NoSuchMethodException {
			problemPath = codeCheck.loadProblem(repo, problemName, sid);
			problem = new Problem(problemPath, level);
			data = new ProblemData();
			data.description = getDescription(problemPath, "index.html");
			if (data.description == null) {
				// TODO: Legacy
				data.description = getDescription(problemPath, "problem.html");
				if (data.description == null) {
					data.description = getDescription(problemPath, "statement.html");
					if (data.description == null)
						data.description = getDescription(problemPath,
							"original-statement.html"); // TODO: legacy
					else
						includeCode = false; // code already shown in statement.html
				}
			}

			for (Path p : problem.getRequiredFiles()) {				
				String cont = Util.read(problemPath, p);
				List<String> parts = Problem.processHideShow(p, cont);
				data.requiredFiles.put(Util.tail(p).toString(), parts);
			}
			for (Path p : problem.getUseFiles()) {
				String cont = Util.read(problemPath, p);
				cont = Problem.processHideShow(p, cont).get(0);
				if (cont.length() > 0) // If it's entirely hidden, don't show its name--this happens with //HIDE in line 1
					data.useFiles.put(Util.tail(p).toString(), cont);
			}
		}

		public void close() throws IOException {
			if (problemPath != null)
				Util.deleteDirectory(problemPath);
		}
	}

	public Result fileData(String repo, String problem, String level, String ccu)
			throws IOException, NoSuchMethodException, ScriptException {        
		if (ccu == null) { 
			Http.Cookie ccuCookie = request().cookie("ccu");
		    ccu = ccuCookie == null ? Util.createUID() : ccuCookie.value();
		}
		try (ProblemContext pc = new ProblemContext(repo, problem, level, ccu)) {
			Http.Cookie newCookie = Http.Cookie.builder("ccu", ccu).withMaxAge(Duration.ofDays(180)).build();
			return ok(Json.toJson(pc.data)).withCookies(newCookie);
		}
	}

	public Result filesHTML(String repo,
			String problemName,
			String level, String ccu)
			throws IOException, NoSuchMethodException, ScriptException {
		if (ccu == null) { 
			Http.Cookie ccuCookie = request().cookie("ccu");
		    ccu = ccuCookie == null ? Util.createUID() : ccuCookie.value();
		}
		try (ProblemContext pc = new ProblemContext(repo, problemName, level, ccu)) {						
			StringBuilder result = new StringBuilder();
			result.append(start);
			if (pc.data.description != null)
				result.append(pc.data.description);
			String contextPath = ""; // request().host(); // TODO
			String url = contextPath + "/check";
			result.append(MessageFormat.format(before, url));
			result.append(MessageFormat.format(provideStart,
					pc.data.requiredFiles.size()));

			// TODO: Remove heuristic for codecomp
			if (pc.data.useFiles.size() == 0
					&& pc.data.requiredFiles.size() == 1)
				pc.includeCode = true;
			for (Map.Entry<String, List<String>> entry : pc.data.requiredFiles
					.entrySet()) {				
				String file = entry.getKey();
				List<String> conts = entry.getValue();
				boolean firstTitle=true;
				int textAreaNumber=0;
				String appended;
				//int continuingLines = 0;
				boolean editable = true;
				for(String cont: conts) { 
					if (cont == null) { // only the case for the first time to skip editable
						editable = false;
					} else {
						int lines = 0;
						++textAreaNumber;
						appended=file+"-"+textAreaNumber;
						if (pc.includeCode) {
							lines = Util.countLines(cont);							
						}
						if (lines == 0)
							lines = 20;
						
						if(editable){
							if(firstTitle){
								result.append(MessageFormat.format(fileOuterDiv, file));
								result.append("<h3>");
								result.append(file);
								result.append("</h3>");
								firstTitle=false;
							}
							//result.append(MessageFormat.format(startNumberLines, "editor", "firstLineNumber", continuingLines));
							result.append(MessageFormat.format(fileAreaBefore, appended,
								lines, "java")); // TODO support more than "java" in ace editor format
							result.append(Util.removeTrailingNewline(Util.htmlEscape(cont)));
							result.append(fileAreaAfter);
							editable=false;	
						}else{
							if(firstTitle){
								result.append(MessageFormat.format(fileOuterDiv, file));
								result.append("<h3>");
								result.append(file);
								result.append("</h3>");
								firstTitle=false;
							}
							
							 String s=cont;
							 int max=20;
							 while(s.indexOf("\n")!=-1){
							 	if((s.substring(0, s.indexOf("\n"))).length()>max){
							 		max=(s.substring(0, s.indexOf("\n"))).length();
							 	}
							 	s=s.substring(s.indexOf("\n")+1);
							 }
							 if(s.length()>max){
							 	max=s.length();
							 }
							 
							result.append(MessageFormat.format(fileAreaBeforeNoEdit, appended,
								lines, max, "java")); // TODO support more than "java" in ace editor format
							result.append(Util.removeTrailingNewline(Util.htmlEscape(cont)));
							result.append(fileAreaAfter);
							editable=true;
						}
					}
				}
				result.append(fileOuterDivAfter);				
			}
			result.append(MessageFormat.format(after, repo, problemName, level));
			result.append(formEnd);
			
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

			// result.append(jsonpAjaxSubmissionScript);
			result.append(bodyEnd);
			
			Http.Cookie newCookie = Http.Cookie.builder("ccu", ccu).withMaxAge(Duration.ofDays(180)).build();
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
				String replacement;
				try {
					replacement = "data:image/png;base64,"
						+ Util.base64(problemDir, src);
				} catch (IOException ex) {
					replacement = "data:image/png;base64,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";
				}
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
