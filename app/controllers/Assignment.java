package controllers;

/*
 An assignment is made up of problems. A problem is provided in a URL 
 that is displayed in an iframe. (In the future, maybe friendly problems 
 could coexist on a page or shared iframe for efficiency.) An assignment 
 weighs its problems.
 
 The "problem key" is normally the problem URL. However, for interactive or CodeCheck 
 problems in the textbook repo, it is the qid of the single question in the problem.
 
 CodeCheckWork is a map from problem keys to scores and states. It only stores the most recent version.
 CodeCheckSubmissions is an append-only log of all submissions of a single problem. 
    
 Tables:
   
 CodeCheckAssignment
   assignmentID [primary key]
   deadline (an ISO 8601 string like "2020-12-01T23:59:59Z")
   editKey // LTI: tool consumer ID + user ID
   problems
     array of // One per group
       array of { URL, qid?, weight } // qid for book repo
  
 CodeCheckLTIResources (Legacy)
   resourceID [primary key] // LTI tool consumer ID + course ID + resource ID 
   assignmentID
   
 CodeCheckWork
   assignmentID [partition key] // non-LTI: courseID? + assignmentID, LTI: toolConsumerID/courseID + assignment ID, Legacy tool consumer ID/course ID/resource ID  
   workID [sort key] // non-LTI: ccid/editKey, LTI: userID
   problems 
     map from URL/qids to { state, score }
   submittedAt
   tab     
       
 CodeCheckSubmissions
   submissionID [partition key] // non-LTI: courseID? + assignmentID + problemKey + ccid/editKey , LTI: toolConsumerID/courseID + assignmentID + problemID + userID 
     // either way, that's resource ID + workID + problem key
   submittedAt [sort key] 
   state: as string, not JSON
   score
  
   with global secondary index (TODO: Not currently)
     problemID 
     submitterID
   
 CodeCheckLTICredentials
   oauth_consumer_key [primary key]
   shared_secret

CodeCheckComments
   assignmentID [partition key] // non-LTI: courseID? + assignmentID, LTI: toolConsumerID/courseID + assignment ID, Legacy tool consumer ID/course ID/resource ID  
   workID [sort key] // non-LTI: ccid/editKey, LTI: userID
   comment

   (This is a separate table from CodeCheckWork because we can't guarantee atomic updates if a student happens to update their work while the instructor updates a comment)

 Assignment parsing format:
 
   Groups separated by 3 or more -
   Each line:
     urlOrQid (weight%)? title
 
 Cookies 
   ccid (student only)
   cckey (student only)
   PLAY_SESSION
*/

import java.io.IOException;
import java.net.URLEncoder;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import models.AssignmentConnector;
import play.Logger;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Assignment extends Controller {
    @Inject private AssignmentConnector assignmentConn;
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
    
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
                if (problemDescriptor.startsWith("!")) { // suppress checking
                	checked = true;
                	problemDescriptor = problemDescriptor.substring(1);
                }
                if (problemDescriptor.startsWith("https")) problemURL = problemDescriptor;
                else if (problemDescriptor.startsWith("http")) {
                    if (!problemDescriptor.startsWith("http://localhost") && !problemDescriptor.startsWith("http://127.0.0.1")) {
                        problemURL = "https" + problemDescriptor.substring(4);
                    }
                    else
                        problemURL = problemDescriptor;                    
                }   
                else if (problemDescriptor.matches("[a-zA-Z0-9_]+(-[a-zA-Z0-9_]+)*")) { 
                    qid = problemDescriptor;
                    problemURL = "https://www.interactivities.ws/" + problemDescriptor + ".xhtml";
                    if (com.horstmann.codecheck.Util.exists(problemURL))
                        checked = true;
                    else
                        problemURL = "https://codecheck.it/files?repo=wiley&problem=" + problemDescriptor;                                                          
                }
                else throw new IllegalArgumentException("Bad problem: " + problemDescriptor);
                if (!checked && !com.horstmann.codecheck.Util.exists(problemURL))
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
    
    private static boolean isProblemKeyFor(String key, ObjectNode problem) {        
        // Textbook repo
        if (problem.has("qid")) return problem.get("qid").asText().equals(key);
        String problemURL = problem.get("URL").asText();
        // Some legacy CodeCheck questions have butchered keys such as 0101407088y6iesgt3rs6k7h0w45haxajn 
        return problemURL.endsWith(key);
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
            for (String key : com.horstmann.codecheck.Util.iterable(submissions.fieldNames())) {
                if (isProblemKeyFor(key, problem)) {    
                    ObjectNode submission = (ObjectNode) submissions.get(key);
                    result += weight * submission.get("score").asDouble();
                }
            }           
        }
        return sum == 0 ? 0 : result / sum;
    }
    
    private static boolean editKeyValid(String suppliedEditKey, ObjectNode assignmentNode) {
        String storedEditKey = assignmentNode.get("editKey").asText();
        return suppliedEditKey.equals(storedEditKey) && !suppliedEditKey.contains("/");
          // Otherwise it's an LTI edit key (tool consumer ID + user ID)
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
            assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
            if (assignmentNode == null) return badRequest("Assignment not found");
            
            if (editKey == null) { // Clone
                assignmentNode.remove("editKey");
                assignmentNode.remove("assignmentID");
            }
            else { // Edit existing assignment
                if (!editKeyValid(editKey, assignmentNode)) 
                    // In the latter case, it is an LTI toolConsumerID + userID             
                    return badRequest("editKey " + editKey + " does not match");
            }
        } 
        assignmentNode.put("saveURL", "/saveAssignment");
        return ok(views.html.editAssignment.render(assignmentNode.toString(), true));               
    }
    
    /*
     * ccid == null, editKey == null, isStudent = true:  Student starts editing
     * ccid != null, editKey == null, isStudent = true:  Student wants to change ID (hacky)
     * ccid != null, editKey != null, isStudent = true:  Student resumes editing
     * ccid != null, editKey != null, isStudent = false: Instructor views a student submission (with the student's editKey)
     * ccid == null, editKey == null, isStudent = false: Instructor views someone else's assignment (for cloning) 
     * ccid == null, editKey != null, isStudent = false: Instructor views own assignment (for editing, viewing submissions)
     */
    public Result work(Http.Request request, String assignmentID, String ccid, String editKey, 
            boolean isStudent) 
            throws IOException, GeneralSecurityException {
        String prefix = models.Util.prefix(request);
        String workID = "";
        boolean editKeySaved = true;

        ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);        
        if (assignmentNode == null) return badRequest("Assignment not found");
        
        assignmentNode.put("isStudent", isStudent);
        if (isStudent) {
            if (ccid == null) {         
                Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
                if (ccidCookie.isPresent()) {
                    ccid = ccidCookie.get().value();
                    Optional<Http.Cookie> editKeyCookie = request.getCookie("cckey");
                    if (editKeyCookie.isPresent()) 
                        editKey = editKeyCookie.get().value();
                    else { // This shouldn't happen, but if it does, clear ID
                        ccid = com.horstmann.codecheck.Util.createPronouncableUID();
                        editKey = Util.createPrivateUID();
                        editKeySaved = false;                       
                    }
                } else { // First time on this browser
                    ccid = com.horstmann.codecheck.Util.createPronouncableUID();
                    editKey = Util.createPrivateUID();
                    editKeySaved = false;
                }
            } else if (editKey == null) { // Clear ID request
                ccid = com.horstmann.codecheck.Util.createPronouncableUID();
                editKey = Util.createPrivateUID();
                editKeySaved = false;               
            }
            assignmentNode.put("clearIDURL", "/assignment/" + assignmentID + "/" + ccid);
            workID = ccid + "/" + editKey;          
        } else { // Instructor
            if (ccid == null && editKey != null && !editKeyValid(editKey, assignmentNode))
                throw new IllegalArgumentException("Edit key does not match");
            if (ccid != null && editKey != null) {  // Instructor viewing student submission
                assignmentNode.put("saveCommentURL", "/saveComment"); 
                workID = ccid + "/" + editKey;
                // Only put workID into assignmentNode when viewing submission as Instructor, for security reason
                assignmentNode.put("workID", workID);
            }
        }
        assignmentNode.remove("editKey");
        ArrayNode groups = (ArrayNode) assignmentNode.get("problems");
        assignmentNode.set("problems", groups.get(Math.abs(workID.hashCode()) % groups.size()));
        
        // Start reading work and comments
        String work = null;
        ObjectNode commentObject = null;
        String comment = null;
        if (!workID.equals(""))  {
            work = assignmentConn.readJsonStringFromDB("CodeCheckWork", "assignmentID", assignmentID, "workID", workID);
            commentObject = assignmentConn.readJsonObjectFromDB("CodeCheckComments", "assignmentID", assignmentID, "workID", workID);
        }
        if (work == null) 
            work = "{ assignmentID: \"" + assignmentID + "\", workID: \"" 
                + workID + "\", problems: {} }";
        if (commentObject == null)
            comment = "";
        else
            comment = commentObject.get("comment").asText();
        assignmentNode.put("comment", comment);

        String lti = "undefined";
        if (isStudent) {                        
            String returnToWorkURL = prefix + "/private/resume/" + assignmentID + "/" + ccid + "/" + editKey;
            assignmentNode.put("returnToWorkURL", returnToWorkURL); 
            assignmentNode.put("editKeySaved", editKeySaved);
            assignmentNode.put("sentAt", Instant.now().toString());
            Http.Cookie newCookie1 = models.Util.buildCookie("ccid", ccid);
            Http.Cookie newCookie2 = models.Util.buildCookie("cckey", editKey);
            return ok(views.html.workAssignment.render(assignmentNode.toString(), work, ccid, lti))
                    .withCookies(newCookie1, newCookie2);
        }
        else { // Instructor
            if (ccid == null) {
                if (editKey != null) { // Instructor viewing for editing/submissions                    
                    // TODO: Check if there are any submissions?
                    assignmentNode.put("viewSubmissionsURL", "/private/viewSubmissions/" + assignmentID + "/" + editKey);
                    String publicURL = prefix + "/assignment/" + assignmentID;
                    String privateURL = prefix + "/private/assignment/" + assignmentID + "/" + editKey;
                    String editAssignmentURL = prefix + "/private/editAssignment/" + assignmentID + "/" + editKey;
                    assignmentNode.put("editAssignmentURL", editAssignmentURL);
                    assignmentNode.put("privateURL", privateURL);
                    assignmentNode.put("publicURL", publicURL);                 
                }
                String cloneURL = prefix + "/copyAssignment/" + assignmentID;
                assignmentNode.put("cloneURL", cloneURL);
            }
            
            return ok(views.html.workAssignment.render(assignmentNode.toString(), work, ccid, lti));
        }
    }
    
    public Result viewSubmissions(Http.Request request, String assignmentID, String editKey)
        throws IOException {
        ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
        if (assignmentNode == null) return badRequest("Assignment not found");
        
        if (!editKeyValid(editKey, assignmentNode))
            throw new IllegalArgumentException("Edit key does not match");

        ArrayNode submissions = JsonNodeFactory.instance.arrayNode();

        Map<String, ObjectNode> itemMap = assignmentConn.readJsonObjectsFromDB("CodeCheckWork", "assignmentID", assignmentID, "workID");

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
        String allSubmissionsURL = "/lti/allSubmissions?resourceID=" + URLEncoder.encode(assignmentID, "UTF-8");
        return ok(views.html.viewSubmissions.render(allSubmissionsURL, submissions.toString()));    
    }

    /*
     * Save existing: request.assignmentID, request.editKey exist
     * New or cloned: Neither request.assignmentID nor request.editKey exist
     */
    public Result saveAssignment(Http.Request request) throws IOException {     
        ObjectNode params = (ObjectNode) request.body().asJson();

        try {
            params.set("problems", parseAssignment(params.get("problems").asText()));
        } catch (IllegalArgumentException e) {
            return badRequest(e.getMessage());
        }
        String assignmentID;
        String editKey;
        ObjectNode assignmentNode;
        if (params.has("assignmentID")) {
            assignmentID = params.get("assignmentID").asText();
            assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
            if (assignmentNode == null) return badRequest("Assignment not found");
            
            if (!params.has("editKey")) return badRequest("Missing edit key");
            editKey = params.get("editKey").asText();
            if (!editKeyValid(editKey, assignmentNode)) 
                return badRequest("Edit key does not match");
        }
        else { // New assignment or clone 
            assignmentID = com.horstmann.codecheck.Util.createPublicUID();
            params.put("assignmentID", assignmentID);
            if (params.has("editKey"))
                editKey = params.get("editKey").asText();
            else { // LTI assignments have an edit key
                editKey = Util.createPrivateUID();
                params.put("editKey", editKey);
            }
            assignmentNode = null;
        }
        assignmentConn.writeJsonObjectToDB("CodeCheckAssignments", params);

        String prefix = models.Util.prefix(request);
        String assignmentURL = prefix + "/private/assignment/" + assignmentID + "/" + editKey;
        params.put("viewAssignmentURL", assignmentURL);
        
        return ok(params);
    }
    
    public Result saveWork(Http.Request request) throws IOException, NoSuchAlgorithmException {
        try {
            ObjectNode requestNode = (ObjectNode) request.body().asJson();
            ObjectNode result = JsonNodeFactory.instance.objectNode();
            
            Instant now = Instant.now();
            String assignmentID = requestNode.get("assignmentID").asText();
            ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
            if (assignmentNode == null) return badRequest("Assignment not found");
            String workID = requestNode.get("workID").asText();
            String problemID = requestNode.get("tab").asText();
            ObjectNode problemsNode = (ObjectNode) requestNode.get("problems");
            
            String submissionID = assignmentID + " " + workID + " " + problemID; 
            ObjectNode submissionNode = JsonNodeFactory.instance.objectNode();
            submissionNode.put("submissionID", submissionID);
            submissionNode.put("submittedAt", now.toString());
            // TODO: NPE in logs for the line below
            submissionNode.put("state", problemsNode.get(problemID).get("state").toString());
            submissionNode.put("score", problemsNode.get(problemID).get("score").asDouble());
            assignmentConn.writeJsonObjectToDB("CodeCheckSubmissions", submissionNode);
            
            if (assignmentNode.has("deadline")) {
                try {
                    Instant deadline = Instant.parse(assignmentNode.get("deadline").asText());
                    if (now.isAfter(deadline)) 
                        return badRequest("After deadline of " + deadline);
                } catch (DateTimeParseException e) { // TODO: This should never happen, but it did
                    logger.error(Util.getStackTrace(e));
                }
            }
            result.put("submittedAt", now.toString());      
    
            assignmentConn.writeNewerJsonObjectToDB("CodeCheckWork", requestNode, "assignmentID", "submittedAt");
            return ok(result);
        } catch (Exception e) {
            logger.error(Util.getStackTrace(e));
            return badRequest(e.getMessage());
        }           
    }
    
    public Result saveComment(Http.Request request) throws IOException {
        try {
            ObjectNode result = JsonNodeFactory.instance.objectNode();
            ObjectNode requestNode = (ObjectNode) request.body().asJson();
            String assignmentID = requestNode.get("assignmentID").asText();
            String workID = requestNode.get("workID").asText();
            String comment = requestNode.get("comment").asText();
            
            ObjectNode commentNode = JsonNodeFactory.instance.objectNode();
            commentNode.put("assignmentID", assignmentID);
            commentNode.put("workID", workID);
            commentNode.put("comment", comment);
            assignmentConn.writeJsonObjectToDB("CodeCheckComments", commentNode);
            result.put("comment", comment);
            result.put("refreshURL", "/private/submission/" + assignmentID + "/" + workID);
            return ok(result);
        } catch (Exception e) {
            logger.error(Util.getStackTrace(e));
            return badRequest(e.getMessage());
        }           
    } 
}
