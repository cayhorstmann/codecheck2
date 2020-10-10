package controllers;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.inject.Inject;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.PutItemOutcome;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.RangeKeyCondition;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.PutItemSpec;
import com.amazonaws.services.dynamodbv2.model.ConditionalCheckFailedException;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.PutItemResult;
import com.amazonaws.services.dynamodbv2.model.ReturnValue;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import models.S3Connection;
import models.Util;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Assignment extends Controller {
	@Inject private S3Connection s3conn;
	
	// TODO: Move these to Util
	public static <T> Iterable<T> iterable(Iterator<T> iterator) { 
        return new Iterable<T>() { 
            public Iterator<T> iterator() { return iterator; } 
        }; 
    } 	
	
	public static String prefix(Http.Request request) {
		return (request.secure() ? "https://" : "http://") + request.host() + "/";
	}
	
	private static boolean exists(String url) {
		boolean result = false;
		try {
			HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
			try {
				conn.connect();
				result = conn.getHeaderField(null).contains("200");
			} finally {
				conn.disconnect();
			}
		} catch (Exception ex) {
		}
		return result;
	}
	
    private static ArrayNode parseAssignment(String assignment) {
        if (assignment == null || assignment.trim().isEmpty()) 
        	throw new IllegalArgumentException("No assignments");
    	ArrayNode groupsNode = JsonNodeFactory.instance.arrayNode();
    	String[] groups = assignment.split("\\s+-{3,}\\s+");
    	for (int problemGroup = 0; problemGroup < groups.length; problemGroup++) {
            String[] lines = groups[problemGroup].split("\\n+");
            if (lines.length == 0) throw new IllegalArgumentException("No problems given");
            String[] problemURLs = new String[lines.length];
            String[] qids = new String[lines.length];
            Double[] weights = new Double[lines.length];	            
            for (int i = 0; i < lines.length; i++) {
            	for (String token: lines[i].trim().split("\\s+")) {
            		boolean checked = false;
            		if (problemURLs[i] == null) {
            			if (token.startsWith("https")) problemURLs[i] = token;
            			else if (token.startsWith("http")) problemURLs[i] = "https" + token.substring(4);
            			else if (token.matches("[a-zA-Z0-9_]+(-[a-zA-Z0-9_]+)*")) {	
            				qids[i] = token;
            				problemURLs[i] = "https://www.interactivities.ws/" + token + ".xhtml";
            				if (exists(problemURLs[i]))
            					checked = true;
            				else
            					problemURLs[i] = "https://codecheck.it/files?repo=wiley&problem=" + token;            				            					
            			}
            			else throw new IllegalArgumentException("Bad token: " + token);
            			if (!checked && !exists(problemURLs[i]))
            				throw new IllegalArgumentException("Cannot find " + (qids[i] == null 
            					? problemURLs[i] : qids[i]));
            		}
            		else if (problemURLs[i] != null && token.matches("[0-9]+(\\.[0-9]+)?%")) weights[i] = 0.01 * Double.parseDouble(token.substring(0, token.length() - 1));
            		else throw new IllegalArgumentException("Bad token: " + token);            		
            	}	            	
            }
            double weightSum = 0;
            int noWeights = 0;
            for (int i = 0; i < lines.length; i++) {
            	if (weights[i] == null) {
            		noWeights++;
            	} else {
            		if (weights[i] < 0) throw new IllegalArgumentException("Bad weight: " + 100 * weights[i]);
            		else weightSum += weights[i];
            	} 
            }
            if (noWeights > 0) {
            	if (weightSum > 1) {
            		throw new IllegalArgumentException("Sum of weights > 100%");
            	}
            	double defaultWeight = (1 - weightSum) / noWeights;
            	for (int i = 0; i < lines.length; i++)
            		if (weights[i] == null) weights[i] = defaultWeight;
            } else if (weightSum > 1) {
            	for (int i = 0; i < lines.length; i++)
            		weights[i] /= weightSum;
            }
            		
        	ArrayNode group = JsonNodeFactory.instance.arrayNode();
            for (int i = 0; i < lines.length; i++) {
            	ObjectNode problem = JsonNodeFactory.instance.objectNode();
            	problem.put("URL", problemURLs[i]);
            	problem.put("weight", weights[i]);
            	if (qids[i] != null) problem.put("qid", qids[i]);
            	group.add(problem);
            }
            groupsNode.add(group);
    	}
        return groupsNode;
    }
    
    private static String key(ObjectNode problem) {
    	if (problem.has("qid")) return problem.get("qid").asText();
		else {
			String url = problem.get("URL").asText();
			return url.substring(url.lastIndexOf("/") + 1).replaceAll("[^A-Za-z_-]", "");
		}
	}
		  	 
	private static double score(String ccid, ObjectNode assignment, ObjectNode submission) {
		double result = 0;
		ArrayNode groups = (ArrayNode) assignment.get("problems");
		ArrayNode problems = (ArrayNode) groups.get(ccid.hashCode() % groups.size());
		for (JsonNode p : problems) {
			ObjectNode problem = (ObjectNode) p;
			String problemKey = key(problem);
			if (submission.has(problemKey))
				result += problem.get("weight").asDouble() * submission.get(problemKey).get("score").asDouble();
		}
		return result;
	}
	
	/*
	 * assignmentID == null: new assignment
	 * assignmentID != null, editKey != null: edit assignment
	 * assignmentID != null, editKey == null: clone assignment
	 */
    public Result edit(Http.Request request, String assignmentID, String editKey) throws IOException {
    	String assignment;
    	if (assignmentID == null) {
    		assignment = "{}";
    	} else {
    		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    		if (editKey == null) // Clone
    			assignmentNode.remove("editKey");
    		else if (!editKey.equals(assignmentNode.get("editKey").asText())) 
    			return badRequest("editKey " + editKey + " does not match");
    		assignment = assignmentNode.toString(); 
    	} 
    	String lti = "undefined";
    	return ok(views.html.editAssignment.render(assignment, lti));     	    	
    }
    
    /*
     * ccid == null, editKey == null, editable = true: Student starts editing
     * ccid == null, editKey == null, editable = false: Instructor views for possible cloning 
     * ccid != null, editKey != null, editable = true: Student resumes editing
     * ccid != null, editKey != null, editable = false: Instructor views student work
     */
    public Result work(Http.Request request, String assignmentID, String ccid, String editKey, 
    		boolean editable, String newid) 
    		throws IOException, GeneralSecurityException {
    	if (newid != null) {
    		ccid = Util.isPronouncableUID(newid) ? newid : Util.createPronouncableUID();
    	}
    	else if (ccid == null && editable) {    		
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
        }
    	boolean editKeySaved = false;
    	String studentWork = null;
    	if (editable) {
    		if (editKey != null)
    		    studentWork = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID, "workID", ccid + "/" + editKey);
    		if (studentWork == null) { 
    			studentWork = "undefined";
        		editKey = Util.createPrivateUID();
    		}
    		else
    			editKeySaved = true;
    	}
    	    	
    	ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    	if (assignmentNode == null)
    		badRequest("No assignment " + assignmentID);
    	String prefix = prefix(request);
    	assignmentNode.remove("editKey");
    	assignmentNode.put("editable", editable);
    	if (!editable && editKey == null) 
    		assignmentNode.put("cloneURL", "/copyAssignment/" + assignmentID);	
    	
    	String lti = "undefined";
    	if (editable) {
    		String returnToWorkURL = prefix + "private/resume/" + assignmentID + "/" + ccid + "/" + editKey;
    		assignmentNode.put("returnToWorkURL", returnToWorkURL); 
    		String workUpdateURL = prefix + "saveWork/" + assignmentID + "/" + ccid + "/" + editKey; 
    		assignmentNode.put("workUpdateURL", workUpdateURL);
        	assignmentNode.put("editKeySaved", editKeySaved);
        	assignmentNode.put("sentAt", Instant.now().toString());
        	Http.Cookie newCookie = Http.Cookie.builder("ccid", ccid).withPath("/").withMaxAge(Duration.ofDays(180)).build();
        	return ok(views.html.workAssignment.render(assignmentNode.toString(), studentWork, ccid, lti)).withCookies(newCookie);
    	}
    	else // Instructor--no cookie
    		return ok(views.html.workAssignment.render(assignmentNode.toString(), "undefined", ccid, lti));    	
    }
    
	public Result view(Http.Request request, String assignmentID, String editKey)
		throws IOException {
		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
		if (!assignmentNode.get("editKey").asText().equals(editKey))
			throw new IllegalArgumentException("Edit key does not match");

		ObjectNode submissions = JsonNodeFactory.instance.objectNode();

    	Map<String, ObjectNode> itemMap = s3conn.readJsonObjectsFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID, "workID");

		for (String submissionKey : itemMap.keySet()) {
			String[] parts = submissionKey.split("/");
			String ccid = parts[0];
			String submissionEditKey = parts[1];
			if (!submissions.has(ccid)) submissions.set(ccid, JsonNodeFactory.instance.objectNode());
			ObjectNode submissionsForCcid = (ObjectNode) submissions.get(ccid);
			
			ObjectNode work = itemMap.get(submissionKey);
			ObjectNode submissionData = JsonNodeFactory.instance.objectNode();
			submissionData.put("score", score(ccid, assignmentNode, (ObjectNode) work.get("problems")));
			submissionData.set("submittedAt", work.get("submittedAt"));
			submissionsForCcid.set(submissionEditKey, submissionData);
		}
		String editURL = "/private/editAssignment/" + assignmentID + "/" + editKey;
		
		return ok(views.html.viewSubmissions.render(assignmentID, assignmentNode.toString(), submissions.toString(), editURL)); 
	}

	public Result saveAssignment(Http.Request request) throws IOException {		
        ObjectNode params = (ObjectNode) request.body().asJson();
        ObjectNode lti = null;
        if (params.has("lti")) {
        	lti = (ObjectNode) params.get("lti");
        	params.remove("lti");
        }        	
        	
        String assignment = params.get("problems").asText();
    	params.set("problems", parseAssignment(assignment));
    	String assignmentID;
    	if (params.has("assignmentID"))
    		assignmentID = params.get("assignmentID").asText();
    	else {
    		assignmentID = Util.createPublicUID();
        	params.put("assignmentID", assignmentID);     		
    	}
    	String editKey = Util.createPrivateUID();
    	if (params.has("editKey")) {
    		editKey = params.get("editKey").asText();
    		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    		if (!editKey.equals(assignmentNode.get("editKey").asText())) 
    			return badRequest("editKey " + editKey + " does not match");    		
    	} else { // Clone
    		assignmentID = Util.createPublicUID();
        	params.put("assignmentID", assignmentID);     		
    		editKey = Util.createPrivateUID();
        	params.put("editKey", editKey);
    	}
    	params.remove("privateURL");
    	params.remove("publicURL");
    	params.remove("error");        	
    	
    	s3conn.writeJsonObjectToDynamoDB("CodeCheckAssignments", params);
    	
   		if (lti != null) {
   			// TODO: How do we know it's not already saved?
   			ObjectNode res = JsonNodeFactory.instance.objectNode();
   			res.set("resourceID", lti.get("resourceID"));
   			res.put("assignmentID", assignmentID);
   			s3conn.writeJsonObjectToDynamoDB("CodeCheckLTIResources", res);
   			
	   		/*
	   		 * Call launchPresentationReturnURL with:
	   		 * return_type=lti_launch_url
	   		 * url=assignment URL (with id=...)
	   		 * Util.getParams(launchPresentationReturnURL)
	   		 */
   			String launchPresentationReturnURL = lti.get("launchPresentationReturnURL").asText();
   			launchPresentationReturnURL = launchPresentationReturnURL
   					+ (launchPresentationReturnURL.contains("?") ? "&" : "?")     					
   					+ "return_type=lti_launch_url"
   					+ "&url=" + URLEncoder.encode(launchPresentationReturnURL, "UTF-8" /* StandardCharsets.UTF_8 */); // TODO
   			new URL(launchPresentationReturnURL).openStream().close();
   		} else {   		
   	    	String prefix = prefix(request);
   			String publicURL = prefix + "assignment/" + assignmentID;
   	    	String privateURL = prefix + "private/assignment/" + assignmentID + "/" + editKey;
   			params.put("privateURL", privateURL);
   			params.put("publicURL", publicURL);
   		}
    	return ok(params);
	}
	
	public Result saveWork(Http.Request request, String assignmentID, String ccid, String editKey) throws IOException, NoSuchAlgorithmException {
		//TODO: Do we need some level of security? Want to avoid spamming
		ObjectNode contents = (ObjectNode) request.body().asJson();
    	ObjectNode result = JsonNodeFactory.instance.objectNode();
    	result.put("submittedAt", Instant.now().toString());    	
    	
    	contents.put("assignmentID", assignmentID);
		contents.put("workID", ccid + "/" + editKey); 

		s3conn.writeNewerJsonObjectToDynamoDB("CodeCheckWork", contents, "assignmentID", "submittedAt");
		return ok(result); 
	}	
}
