package controllers;

/*
 An assignment is made up of problems. A problem is provided in a URL 
 that is displayed in an iframe. (In the future, maybe friendly problems 
 could coexist on a page or shared iframe for efficiency.) An assignment 
 weighs its problems.
 
 A problem contains one or more questions. Each question has a globally unique qid. 
 The max scores of a problem must add up to 1.0. It's up the problem to weigh 
 questions. 
 
 The problem ID is normally the problem URL. However, for interactive or CodeCheck 
 problems in the textbook repo, it is the qid of the single question in the problem.
 
 A submission contains work on one or more questions. Work is a map from qids to 
 scores, states, and the problem ID (unless it can be determined automatically that 
 a question belongs to a problem.) The problem ID is necessary for weighing and  
 changes in assignments. 
   
 If the key of the enclosing problem is different from the qid, it must be included in the
 work record. 
 
 Tables:
   
 CodeCheckAssignment
   assignmentID [primary key]
   deadlineDate
   deadlineTime
   editKey // LTI: tool consumer ID + user ID
   problems
     array of // One per group
       array of { URL, qid?, weight } // qid for book repo
  
 CodeCheckLTIResources
   resourceID [primary key] // LTI tool consumer ID + course ID + resource ID 
   assignmentID
   
 CodeCheckWork
   assignmentID [partition key] // non-LTI: assignmentID in CodeCheckAssignments, LTI: tool consumer ID + course ID + resource ID, TODO: Should have been named resourceID  
   workID [sort key] // non-LTI: ccid + editKey, LTI: userID
   problems // TODO: Should have been named questions
     map from qids to { state, score, pid? }
   submittedAt
   lastProblem    
   
 CodeCheckLTICredentials
   oauth_consumer_key [primary key]
   shared_secret


 Assignment parsing format:
 
   Groups separated by 3 or more -
   Each line:
     urlOrQid weight%? 
 
 
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    	Pattern problemPattern = Pattern.compile("\\s*(\\S+)(\\s+[0-9.]+%)?(.*)");
    	String[] groups = assignment.split("\\s+-{3,}\\s+");
    	for (int problemGroup = 0; problemGroup < groups.length; problemGroup++) {
            String[] lines = groups[problemGroup].split("\\n+");
            if (lines.length == 0) throw new IllegalArgumentException("No problems given");
            ArrayNode group = JsonNodeFactory.instance.arrayNode();
            for (int i = 0; i < lines.length; i++) {
            	ObjectNode problem = JsonNodeFactory.instance.objectNode();
        		Matcher matcher = problemPattern.matcher(lines[i]);
            	if (!matcher.matches())
            		throw new IllegalArgumentException("Bad input " + lines[i]);
            	String problemDescriptor = matcher.group(1); // URL or qid
            	String problemURL;
            	String qid = null;
            	boolean checked = false;
            	if (problemDescriptor.startsWith("https")) problemURL = problemDescriptor;
    			else if (problemDescriptor.startsWith("http")) problemURL = "https" + problemDescriptor.substring(4);
    			else if (problemDescriptor.matches("[a-zA-Z0-9_]+(-[a-zA-Z0-9_]+)*")) {	
    				qid = problemDescriptor;
    				problemURL = "https://www.interactivities.ws/" + problemDescriptor + ".xhtml";
    				if (Util.exists(problemURL))
    					checked = true;
    				else
    					problemURL = "https://codecheck.it/files?repo=wiley&problem=" + problemDescriptor;            				            					
    			}
    			else throw new IllegalArgumentException("Bad problem: " + problemDescriptor);
    			if (!checked && !Util.exists(problemURL))
    				throw new IllegalArgumentException("Cannot find " + problemDescriptor);            	
            	problem.put("URL", problemURL);
            	if (qid != null) problem.put("qid", qid);
            	
            	String weight = matcher.group(2);
            	if (weight == null) weight = "100";
            	else weight = weight.trim().replace("%", "");
            	problem.put("weight", Double.parseDouble(weight) / 100);

            	String title = matcher.group(3);
            	if (title != null) { 
            		title = title.trim();
            		if (!title.isEmpty())
            			problem.put("title", title);
            	}
            	group.add(problem);
            }
            groupsNode.add(group);
    	}
        return groupsNode;
    }
    
    private static boolean containsQuestion(ObjectNode problem, String qid, ObjectNode submission) {    	
    	// Textbook repo
    	if (problem.has("qid")) return problem.get("qid").asText().equals(qid);
    	String problemURL = problem.get("URL").asText();
    	if (submission.has("pid")) return problemURL.equals(submission.get("pid").asText());    	
		// Some legacy CodeCheck questions have butchered keys such as 0101407088y6iesgt3rs6k7h0w45haxajn 
    	return problemURL.endsWith(qid);
    	// TODO: return problemURL.equals(qid);
	}
		  	 
	public static double score(ObjectNode assignment, ObjectNode work) {
		ArrayNode groups = (ArrayNode) assignment.get("problems");
		String workID = work.get("workID").asText();
		ArrayNode problems = (ArrayNode) groups.get(workID.hashCode() % groups.size());
		ObjectNode submissions = (ObjectNode) work.get("problems");
		double result = 0;
		double sum = 0;
		for (JsonNode p : problems) {
			ObjectNode problem = (ObjectNode) p;
			double weight = problem.get("weight").asDouble();
			sum += weight;
			for (String qid : Util.iterable(submissions.fieldNames())) {
				ObjectNode submission = (ObjectNode) submissions.get(qid);
				if (containsQuestion(problem, qid, submission))	
					result += weight * submission.get("score").asDouble();
			}			
		}
		return sum == 0 ? 0 : result / sum;
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
    		else { // Edit existing assignment
    			if (!editKey.equals(assignmentNode.get("editKey").asText()))     		
    				return badRequest("editKey " + editKey + " does not match");
    			// TODO: Check if there are any submissions?
    			assignmentNode.put("viewSubmissionsURL", "/private/viewSubmissions/" + assignmentID + "/" + editKey);
    		}
    	} 
    	assignmentNode.put("saveURL", "/saveAssignment");
    	return ok(views.html.editAssignment.render(assignmentNode.toString(), true));     	    	
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
    	ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    	if (assignmentNode == null) return badRequest("No assignment " + assignmentID);
    	String prefix = Util.prefix(request);
    	assignmentNode.remove("editKey");
    	assignmentNode.put("isStudent", isStudent);
    	if (!isStudent && editKey == null) 
    		assignmentNode.put("cloneURL", "/copyAssignment/" + assignmentID);	    	
    	
    	if (newid != null) {
    		ccid = Util.isPronouncableUID(newid) ? newid : Util.createPronouncableUID();
    	}
    	else if (ccid == null && isStudent) {    		
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
        }
    	boolean editKeySaved = true;
    	String work = null;
    	if (editKey == null) {
    		Optional<Http.Cookie> editKeyCookie = request.getCookie("cckey");
            editKey = editKeyCookie.map(Http.Cookie::value).orElse(null);
            if (editKey == null) {
            	editKey = Util.createPrivateUID();
            	editKeySaved = false;
            }
    	}
    	if (editKeySaved) 
   		    work = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID, "workID", ccid + "/" + editKey);
    	if (work == null)
       		work = "{ assignmentID: \"" + assignmentID + "\", workID: \"" + ccid + "/" + editKey + "\", problems: {} }";
    	
    	String lti = "undefined";
    	if (isStudent) {    		
    		String returnToWorkURL = prefix + "private/resume/" + assignmentID + "/" + ccid + "/" + editKey;
    		assignmentNode.put("returnToWorkURL", returnToWorkURL); 
        	assignmentNode.put("editKeySaved", editKeySaved);
        	assignmentNode.put("sentAt", Instant.now().toString());
        	Http.Cookie newCookie1 = Http.Cookie.builder("ccid", ccid).withPath("/").withMaxAge(Duration.ofDays(180)).build();
        	Http.Cookie newCookie2 = Http.Cookie.builder("cckey", editKey).withPath("/").withMaxAge(Duration.ofDays(180)).build();
        	return ok(views.html.workAssignment.render(assignmentNode.toString(), work, ccid, lti)).withCookies(newCookie1, newCookie2);
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
			submissionData.put("score", Assignment.score(assignmentNode, work));
			submissionData.set("submittedAt", work.get("submittedAt"));
			submissionData.put("viewURL", "/private/submission/" + assignmentID + "/" + ccid + "/" + submissionEditKey); 
			submissions.add(submissionData);			
		}
		return ok(views.html.viewSubmissions.render(submissions.toString())); 
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
