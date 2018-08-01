package controllers;

import java.io.IOException;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.time.Duration;
import java.util.List;
import java.util.Map;

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
	private static String start = "<!DOCTYPE html>\n<html><head>\n" 
			+ "<meta http-equiv='content-type' content='text/html; charset=UTF-8' />\n" 
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
			+ "<input type=\"hidden\" name=\"problem\" value=\"{1}\"/>\n";
	private static String formEnd = "</form>\n<div id=\"codecheck-submit-response\"></div>\n";
	private static String bodyEnd = "</body></html>";

	private static String useStart = "<p>Use the following {0,choice,1#file|2#files}:</p>\n";
	private static String provideStart = "<p>Complete the following {0,choice,1#file|2#files}:</p>\n";

	@Inject CodeCheck codeCheck;

	public Result fileData(String repo, String problemName, String ccu)
			throws IOException, NoSuchMethodException, ScriptException {        
		if (ccu == null) { 
			Http.Cookie ccuCookie = request().cookie("ccu");
		    ccu = ccuCookie == null ? Util.createUID() : ccuCookie.value();
		}		
		Path problemPath = null;
		try {
			problemPath = codeCheck.loadProblem(repo, problemName, ccu);
			Problem problem = new Problem(problemPath);
			Http.Cookie newCookie = Http.Cookie.builder("ccu", ccu).withMaxAge(Duration.ofDays(180)).build();
			return ok(Json.toJson(problem.getData())).withCookies(newCookie);
		} finally {
			if (problemPath != null)
				Util.deleteDirectory(problemPath);
		}
	}

	public Result filesHTML(String repo, String problemName, String ccu)
			throws IOException, NoSuchMethodException, ScriptException {
		if (ccu == null) { 
			Http.Cookie ccuCookie = request().cookie("ccu");
		    ccu = ccuCookie == null ? Util.createUID() : ccuCookie.value();
		}
		Path problemPath = null;
		try {						
			problemPath = codeCheck.loadProblem(repo, problemName, ccu);
			Problem problem = new Problem(problemPath);
			ProblemData data = problem.getData();
			StringBuilder result = new StringBuilder();
			result.append(start);
			
			if (data.description != null)
				result.append(data.description);
			String contextPath = ""; // request().host(); // TODO
			String url = contextPath + "/check";
			result.append(MessageFormat.format(before, url));
			result.append(MessageFormat.format(provideStart,
					data.requiredFiles.size()));

			for (Map.Entry<String, List<String>> entry : data.requiredFiles
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
						lines = Util.countLines(cont);							
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
			result.append(MessageFormat.format(after, repo, problemName));
			result.append(formEnd);
			
			int nusefiles = data.useFiles.size();
			if (nusefiles > 0) {
				result.append(MessageFormat.format(useStart, nusefiles));
				for (Map.Entry<String, String> entry : data.useFiles
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
		} finally {
			if (problemPath != null)
				Util.deleteDirectory(problemPath);
		}
	}
}
