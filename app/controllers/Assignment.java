package controllers;

/*
 An assignment is made up of problems. A problem is provided in a URL 
 that is displayed in an iframe. (In the future, maybe friendly problems 
 could coexist on a page or shared iframe for efficiency.) An assignment 
 weighs its problems.
 
 A problem contains one or more questions. Each question has a globally unique qid. 
 The max scores of a problem must add up to 1.0. It's up the problem to weigh 
 questions. 

 The key of a problem URL is:
 - The qid for problems containing a single question in the book repo 
   (interactive or CodeCheck in Wiley repo)
 - The URL otherwise
 (Note that one can get the key of a URL, but to go from a book database qid to
 the URL, it is not clear whether the URL is a CodeCheck URL or an interactivities URL. )

 Student work on an assignment is a map from qids to scores and states. If the key 
 of the enclosing problem is different from the qid, it must be included in the
 work record. This is necessary for weighing, and to deal with changing assignments.
 
 Tables:
 
 CodeCheckWork
   assignmentID [partition key]
   workID [sort key] // ccid + editKey or LTI resourceID
   problems
     map from qids to { state, score, pid? }
   submittedAt
   lastProblem
   
 CodeCheckAssignment
   assignmentID [primary key]
   deadlineDate
   deadlineTime
   editKey
   problems
     array of // One per group
       array of { URL, qid?, weight } // qid for book repo
  
 CodeCheckLTIResources
   resourceID [primary key] // LTI tool consumer ID + course ID + resource ID 
   assignmentID 
   
 CodeCheckLTICredentials
   oauth_consumer_key [primary key]
   shared_secret
   
 */

import java.io.IOException;
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
	
	public static ArrayNode parseAssignment(String assignment) {
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
            				if (Util.exists(problemURLs[i]))
            					checked = true;
            				else
            					problemURLs[i] = "https://codecheck.it/files?repo=wiley&problem=" + token;            				            					
            			}
            			else throw new IllegalArgumentException("Bad token: " + token);
            			if (!checked && !Util.exists(problemURLs[i]))
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
    
    private static String pid(ObjectNode problem) {
    	if (problem.has("qid")) return problem.get("qid").asText();
		else return problem.get("URL").asText();
	}
		  	 
	public static double score(String ccid, ObjectNode assignment, ObjectNode work) {
		double result = 0;
		ArrayNode groups = (ArrayNode) assignment.get("problems");
		ArrayNode problems = (ArrayNode) groups.get(ccid.hashCode() % groups.size());
		ObjectNode submissions = (ObjectNode) work.get("problems");
		for (String qid : Util.iterable(submissions.fieldNames())) {
			String submissionPid = qid;
			ObjectNode submission = (ObjectNode) submissions.get(qid);
			if (submission.has("pid")) submissionPid = submission.get("pid").asText();
			for (JsonNode p : problems) {
				ObjectNode problem = (ObjectNode) p;
				if (pid(problem).equals(submissionPid))	
					result += problem.get("weight").asDouble() * submission.get("score").asDouble();
			}	
		}
		return result;
	}
	
	/*
	 * assignmentID == null: new assignment
	 * assignmentID != null, editKey != null: edit assignment
	 * assignmentID != null, editKey == null: clone assignment
	 */
    public Result edit(Http.Request request, String assignmentID, String editKey) throws IOException {
    	ObjectNode assignmentNode;
    	if (assignmentID == null) {
    		assignmentNode = JsonNodeFactory.instance.objectNode();
    	} else {
    		assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    		if (editKey == null) { // Clone
    			assignmentNode.remove("editKey");
    			assignmentNode.remove("assignmentID");
    		}
    		else if (!editKey.equals(assignmentNode.get("editKey").asText())) 
    			return badRequest("editKey " + editKey + " does not match");
    	} 
    	assignmentNode.put("saveURL", "/saveAssignment");
    	return ok(views.html.editAssignment.render(assignmentNode.toString()));     	    	
    }
    
    /*
     * ccid == null, editKey == null, isStudent = true: Student starts editing
     * ccid == null, editKey == null, isStudent = false: Instructor views for possible cloning 
     * ccid != null, editKey != null, isStudent = true: Student resumes editing
     * ccid != null, editKey != null, isStudent = false: Instructor views student work
     */
    public Result work(Http.Request request, String assignmentID, String ccid, String editKey, 
    		boolean isStudent, String newid) 
    		throws IOException, GeneralSecurityException {
    	if (newid != null) {
    		ccid = Util.isPronouncableUID(newid) ? newid : Util.createPronouncableUID();
    	}
    	else if (ccid == null && isStudent) {    		
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
        }
    	boolean editKeySaved;
    	String work;
    	if (editKey == null) { 
       		editKey = Util.createPrivateUID();
       		editKeySaved = false;
       		work = "{ assignmentID: \"" + assignmentID + "\", workID: \"" + ccid + "/" + editKey + "\", problems: {} }";
    	}
    	else { 
   		    work = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID, "workID", ccid + "/" + editKey);
   		    if (work == null) return badRequest("Work not found");  
   			editKeySaved = true;
    	}

    	ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    	if (assignmentNode == null) badRequest("No assignment " + assignmentID);
    	String prefix = Util.prefix(request);
    	assignmentNode.remove("editKey");
    	assignmentNode.put("isStudent", isStudent);
    	if (!isStudent && editKey == null) 
    		assignmentNode.put("cloneURL", "/copyAssignment/" + assignmentID);	
    	
    	String lti = "undefined";
    	if (isStudent) {    		
    		String returnToWorkURL = prefix + "private/resume/" + assignmentID + "/" + ccid + "/" + editKey;
    		assignmentNode.put("returnToWorkURL", returnToWorkURL); 
        	assignmentNode.put("editKeySaved", editKeySaved);
        	assignmentNode.put("sentAt", Instant.now().toString());
        	Http.Cookie newCookie = Http.Cookie.builder("ccid", ccid).withPath("/").withMaxAge(Duration.ofDays(180)).build();
        	return ok(views.html.workAssignment.render(assignmentNode.toString(), work, ccid, lti)).withCookies(newCookie);
    	}
    	else // Instructor--no cookie
    		return ok(views.html.workAssignment.render(assignmentNode.toString(), work, ccid, lti));    	
    }
	public Result viewSubmissions(Http.Request request, String assignmentID, String editKey)
		throws IOException {
		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
		if (!assignmentNode.get("editKey").asText().equals(editKey))
			throw new IllegalArgumentException("Edit key does not match");

		ArrayNode submissions = JsonNodeFactory.instance.arrayNode();

    	Map<String, ObjectNode> itemMap = s3conn.readJsonObjectsFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID, "workID");

    	for (String submissionKey : itemMap.keySet()) {
			String[] parts = submissionKey.split("/");
			String ccid = parts[0];
			String submissionEditKey = parts[1];
			
			ObjectNode work = itemMap.get(submissionKey);
			ObjectNode submissionData = JsonNodeFactory.instance.objectNode();
			submissionData.put("opaqueID", ccid);
			submissionData.put("score", Assignment.score(ccid, assignmentNode, work));
			submissionData.set("submittedAt", work.get("submittedAt"));
			submissionData.put("viewURL", "/private/submission/" + assignmentID + "/" + ccid + "/" + submissionEditKey); 
			submissions.add(submissionData);			
		}
		String editURL = "/private/editAssignment/" + assignmentID + "/" + editKey;
		
		return ok(views.html.viewSubmissions.render(submissions.toString(), editURL)); 
	}

	/*
	 * Save existing: request.assignmentID, request.editKey exist
	 * New or cloned: Neither request.assignmentID nor request.editKey exist
	 */
	public Result saveAssignment(Http.Request request) throws IOException {		
        ObjectNode params = (ObjectNode) request.body().asJson();

        String assignment = params.get("problems").asText();
    	params.set("problems", parseAssignment(assignment));
    	String assignmentID;
    	String editKey;
    	ObjectNode assignmentNode;
    	if (params.has("assignmentID")) {
    		assignmentID = params.get("assignmentID").asText();
    		assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    		if (assignmentNode == null) return badRequest("Assignment not found");
    		if (!params.has("editKey")) return badRequest("Missing edit key");
    		editKey = params.get("editKey").asText();
    		if (!editKey.equals(assignmentNode.get("editKey").asText())) 
    			return badRequest("Edit key does not match");
    	}
    	else { // New assignment or clone 
    		assignmentID = Util.createPublicUID();
        	params.put("assignmentID", assignmentID);
        	if (params.has("editKey"))
        		editKey = params.get("editKey").asText();
        	else { // LTI assignments have an edit key
        		editKey = Util.createPrivateUID();
        		params.put("editKey", editKey);
        	}
        	assignmentNode = null;
    	}
    	
    	params.remove("privateURL");
    	params.remove("publicURL");
    	params.remove("error");        	
    	
    	s3conn.writeJsonObjectToDynamoDB("CodeCheckAssignments", params);
    	
    	String prefix = Util.prefix(request);
		String publicURL = prefix + "assignment/" + assignmentID;
    	String privateURL = prefix + "private/assignment/" + assignmentID + "/" + editKey;
		params.put("privateURL", privateURL);
		params.put("publicURL", publicURL);

		return ok(params);
	}
	
	public Result saveWork(Http.Request request) throws IOException, NoSuchAlgorithmException {
		ObjectNode contents = (ObjectNode) request.body().asJson();
    	ObjectNode result = JsonNodeFactory.instance.objectNode();
    	result.put("submittedAt", Instant.now().toString());    	

		s3conn.writeNewerJsonObjectToDynamoDB("CodeCheckWork", contents, "assignmentID", "submittedAt");
		return ok(result); 
	}		
}
