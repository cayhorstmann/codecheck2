package controllers;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.HashMap;
import java.util.Iterator;
import static java.util.Map.Entry;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;

import javax.inject.Inject;

import models.CodeCheck;
import models.Util;
import play.Logger;
import play.libs.Json;
import play.libs.Jsonp;
import play.libs.concurrent.HttpExecution;
import play.libs.concurrent.HttpExecutionContext;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class Check extends Controller {
	// @Inject HttpExecutionContext ec;
	private CodecheckExecutionContext ccec; 
	@Inject CodeCheck codeCheck;
	
	// Classic HTML report
	public CompletableFuture<Result> checkHTML(Http.Request request) throws IOException, InterruptedException {
		Map<String, String[]> params = request.body().asFormUrlEncoded();
		return CompletableFuture.supplyAsync(() -> {
			try {
				String ccu = null;
		        String repo = "ext";
		        String problem = "";
		        Path submissionDir = codeCheck.createSubmissionDirectory();
		        
		        for (String key : params.keySet()) {
		            String value = params.get(key)[0];
		            if (key.equals("repo"))
		                repo = value;
		            else if (key.equals("problem"))
		                problem = value;
		            else if (key.equals("ccu"))
		            	ccu = value;
		            else
		                Util.write(submissionDir, key, value);
		        }
		    	if (ccu == null) { 
					Http.Cookie ccuCookie = request.cookie("ccu");
				    ccu = ccuCookie == null ? Util.createPronouncableUID() : ccuCookie.value();
				}
				long startTime = System.nanoTime();			
		        codeCheck.run("html", repo, problem, ccu, submissionDir);
				double elapsed = (System.nanoTime() - startTime) / 1000000000.0;
		        String report = Util.read(submissionDir.resolve("report.html"));
		        if (report == null || report.length() == 0) {
		        	report = String.format("Timed out after %5.0f seconds\n", elapsed);
		        }
		        
		        Http.Cookie newCookie = Http.Cookie.builder("ccu", ccu).withMaxAge(Duration.ofDays(180)).build();
				// TODO: Delete submissionDir unless flag is set to keep it?
				// Util.deleteDirectory(submissionDir);
		        return ok(report).withCookies(newCookie).as("text/html");
			}
			catch (Exception ex) {
				return internalServerError(Util.getStackTrace(ex));
			}
		}, HttpExecution.fromThread((Executor) ccec) /* ec.current() */);                        
	}
			
	// From JS UI
	public CompletableFuture<Result> checkNJS(Http.Request request) throws IOException, InterruptedException  {
		Map<String, String[]> params;
		if ("application/x-www-form-urlencoded".equals(request.contentType().orElse(""))) 
			params = request.body().asFormUrlEncoded();
		else if ("application/json".equals(request.contentType().orElse(""))) {
			params = new HashMap<>();
			JsonNode json = request.body().asJson();
			Iterator<Entry<String, JsonNode>> iter = json.fields();
			while (iter.hasNext()) {
				Entry<String, JsonNode> entry = iter.next();
				params.put(entry.getKey(), new String[] { entry.getValue().asText() });			
			};
		}
		else 
			params = request.queryString();
		
		return CompletableFuture.supplyAsync(() -> {
			try {
				String ccu = null;
				String repo = "ext";
				String problem = null;
		        Path submissionDir = codeCheck.createSubmissionDirectory();
				String reportType = "njs";
				String callback = null;
				String scoreCallback = null;
				StringBuilder requestParams = new StringBuilder();
				ObjectNode studentWork = JsonNodeFactory.instance.objectNode();
				for (String key : params.keySet()) {
					String value = params.get(key)[0];
					
					if (requestParams.length() > 0) requestParams.append(", ");
					requestParams.append(key);
					requestParams.append("=");
					int nl = value.indexOf('\n');
					if (nl >= 0) {
						requestParams.append(value.substring(0, nl));  
						requestParams.append("...");
					}
					else requestParams.append(value);
					
					if ("repo".equals(key)) repo = value;
					else if ("problem".equals(key)) problem = value;
					else if ("callback".equals(key)) callback = value;
					else if ("scoreCallback".equals(key)) scoreCallback = value;
					else if ("ccu".equals(key)) ccu = value;
					else {
						Util.write(submissionDir, key, value);
						studentWork.put(key, value);
					}
				}
				if (ccu == null) { 
					Http.Cookie ccuCookie = request.cookie("ccu");
				    ccu = ccuCookie == null ? Util.createPronouncableUID() : ccuCookie.value();
				};				
				Logger.of("com.horstmann.codecheck.check").info("checkNJS: " + requestParams);
				//TODO last param should be submissionDir
				codeCheck.run(reportType, repo, problem, ccu, submissionDir);
				ObjectNode result = (ObjectNode) Json.parse(Util.read(submissionDir.resolve("report.json")));
				String reportZip = Util.base64(submissionDir, "report.signed.zip");
				
				//TODO: Score callback no longer used from LTIHub. Does Engage use it?
				if (scoreCallback != null) {
					if (scoreCallback.startsWith("https://")) 
						scoreCallback = "http://" + scoreCallback.substring("https://".length()); // TODO: Fix
					
					//TODO: Add to result the student submissions
					ObjectNode augmentedResult = result.deepCopy();
					augmentedResult.set("studentWork", studentWork);
					
					String resultText = Json.stringify(augmentedResult);
					Logger.of("com.horstmann.codecheck.lti").info("Request: " + scoreCallback + " " + resultText);
					String response = Util.httpPost(scoreCallback, resultText, "application/json");
					Logger.of("com.horstmann.codecheck.lti").info("Response: " + response);
				}
				
				result.put("zip", reportZip);
				Http.Cookie newCookie = Http.Cookie.builder("ccu", ccu).withMaxAge(Duration.ofDays(180)).build();				
				// TODO: Delete submissionDir unless flag is set to keep it?
				// Util.deleteDirectory(submissionDir);
				if (callback == null)
					return ok(result).withCookies(newCookie);
				else
					return ok(Jsonp.jsonp(callback, result)).withCookies(newCookie); // TODO: Include "zip" here?
			} catch (Exception ex) {
				return internalServerError(Util.getStackTrace(ex));
			}
		}, HttpExecution.fromThread((Executor) ccec) /* ec.current() */);			
	}
}
