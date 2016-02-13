package controllers;

import models.Config;
import models.PlayConfig;
import models.Util;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.Iterator;
import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.BodyParser;

// TODO: Cookies?

public class Check extends Controller {
	
	private static Config config = PlayConfig.INSTANCE;
	
	public Result checkHTML() throws IOException {
		Map<String, String[]> formParams = request().body().asFormUrlEncoded();
		Path submissionDir = Util.getDir(config, "submissions");
        Path tempDir = Util.createTempDirectory(submissionDir);
        String repo = "ext";
        String problem = "";
        String level = "check";
        String callback = null;
        for (String key : formParams.keySet()) {
            String value = formParams.get(key)[0];
            if (key.equals("repo"))
                repo = value;
            else if (key.equals("problem"))
                problem = value;
            else if (key.equals("level"))
                level = value;
            else if (key.equals("callback"))
            	callback = value;
            else
                Util.write(tempDir, key, value);
        }
        Util.runLabrat(config, "html", repo, problem, level, tempDir.toAbsolutePath());
        String report = Util.read(tempDir.resolve("report.html"));
        if (callback != null) {
        	// Replace download link with Save Score button
        	String target = "<p class=\"score\">";
        	int n = report.indexOf(target) + target.length();
        	int n2 = report.indexOf("<", n);
        	String score = report.substring(n, n2);
        	n = report.indexOf("<p class=\"download\">", n2);
        	target = "</p>";
        	n2 = report.indexOf(target, n) + target.length();
        	String buttonHTML = "<input id='submitScore' type='button' value='Submit score'>";
        	report = report.substring(0, n) + buttonHTML + report.substring(n2);
        	
        	
        	String buttonScriptTemplate = "<script src=''https://code.jquery.com/jquery-2.2.0.min.js''></script>" +
   "<script>$(document).ready(function() '{'" +
    "$(''#submitScore'').click(function()'{'" +
      "$.getJSON(''{0}?callback=?'', '{'  score: ''{1}'' '}')" +
      ".done(function(data) '{'" +
        "if (data.received) $(''#submitScore'').prop(''disabled'', true);" +
      "'}');" +
    "'}');" + 
  "'}');" +      
  "</script>";
        	String buttonScript = MessageFormat.format(buttonScriptTemplate, callback, score);
        	n = report.indexOf("<title>");
        	report = report.substring(0, n) + buttonScript + report.substring(n);        	
        }
        return ok(report).as("text/html");
        
        
        
        // TODO: Delete tempDir
	}
	
	public Result check() throws IOException { // Where is this used?
		Map<String, String[]> formParams = request().body().asFormUrlEncoded();
		Path submissionDir = Util.getDir(config, "submissions");
        Path tempDir = Util.createTempDirectory(submissionDir);
        String repo = "ext";
        String problem = "";
        String level = "check";
        for (String key : formParams.keySet()) {
            String value = formParams.get(key)[0];
            if (key.equals("repo"))
                repo = value;
            else if (key.equals("problem"))
                problem = value;
            else if (key.equals("level"))
                level = value;
            else
                Util.write(tempDir, key, value);
        }
        Util.runLabrat(config, "json", repo, problem, level, tempDir.toAbsolutePath());
        return ok(Util.read(tempDir.resolve("report.json"))).as("application/json");
        
        // TODO: Delete tempDir
	}
	
	@BodyParser.Of(BodyParser.Json.class)
	public Result checkJson() throws IOException  {
		Path submissionDir = Util.getDir(config, "submissions");
        Path tempDir = Util.createTempDirectory(submissionDir);
	    JsonNode json = request().body().asJson();
	    Iterator<Map.Entry<String,JsonNode>> dirs = json.fields();
	    String repo = "untitled";
	    String problem = "untitled";
	    String level = "1";
	    while (dirs.hasNext()) {
	    	Map.Entry<String, JsonNode> dirEntry = dirs.next();
	    	String key = dirEntry.getKey();
	    	JsonNode value = dirEntry.getValue();
	    	if ("repo".equals(key)) repo = value.textValue();
	    	else if ("problem".equals(key)) problem = value.textValue();
	    	else if ("level".equals(key)) level = value.textValue();
	    	else { 
	    		Path dir = tempDir.resolve(key);
	    		java.nio.file.Files.createDirectory(dir);
	    		Iterator<Map.Entry<String,JsonNode>> files = value.fields();
	    		while (files.hasNext()) {
	    			Map.Entry<String, JsonNode> fileEntry = files.next();	    		
	    			Util.write(dir, fileEntry.getKey(), fileEntry.getValue().textValue());
	    		}
	    	}
	    }
        Util.runLabrat(config, "json", repo, problem, level, tempDir.toAbsolutePath(), tempDir.resolve("submission").toAbsolutePath());
        return ok(Util.read(tempDir.resolve("submission/report.json"))).as("application/json");
        // TODO: Delete tempDir	    
	}
}
