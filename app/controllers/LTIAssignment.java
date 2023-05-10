package controllers;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import models.JWT;
import models.LTI;
import models.AssignmentConnector;
import oauth.signpost.exception.OAuthCommunicationException;
import oauth.signpost.exception.OAuthExpectationFailedException;
import oauth.signpost.exception.OAuthMessageSignerException;
import play.Logger;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import play.mvc.Security;

   
/*
Session cookie (LTI Instructors only)
  "user": toolConsumerID + "/" + userID (needs toolConsumerID because it's used for secure identification)

TODO: Can/should this be replaced with JWT? 

*/


public class LTIAssignment extends Controller {
    @Inject private AssignmentConnector assignmentConn;
    @Inject private LTI lti;
    @Inject private JWT jwt;
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
    
    public static boolean isInstructor(Map<String, String[]> postParams) {
        String role = com.horstmann.codecheck.Util.getParam(postParams, "roles");
        return role != null && (role.contains("Faculty") || role.contains("TeachingAssistant") || role.contains("Instructor"));
    }
    
    public Result config(Http.Request request) throws UnknownHostException {
        String host = request.host();
        if (host.endsWith("/")) host = host.substring(0, host.length() - 1);
        return ok(views.xml.lti_config.render(host)).as("application/xml");         
    }     

    private String assignmentOfResource(String resourceID) throws IOException {
        if (resourceID.contains(" ") ) {
            int i = resourceID.lastIndexOf(" ");
            return resourceID.substring(i + 1);
        } else {
            ObjectNode resourceNode = assignmentConn.readJsonObjectFromDB("CodeCheckLTIResources", "resourceID", resourceID); 
            if (resourceNode == null) return null;
            return resourceNode.get("assignmentID").asText();
        }
    }   
    
    /*
     * Called from Canvas and potentially other LMS with a "resource selection" interface
     */
    public Result createAssignment(Http.Request request) throws UnsupportedEncodingException {    
        Map<String, String[]> postParams = request.body().asFormUrlEncoded();
        if (!lti.validate(request)) {
            return badRequest("Failed OAuth validation");
        }       
        
        if (!isInstructor(postParams)) 
            return badRequest("Instructor role is required to create an assignment.");
        String userID = com.horstmann.codecheck.Util.getParam(postParams, "user_id");
        if (com.horstmann.codecheck.Util.isEmpty(userID)) return badRequest("No user id");

        String toolConsumerID = com.horstmann.codecheck.Util.getParam(postParams, "tool_consumer_instance_guid");
        String userLMSID = toolConsumerID + "/" + userID;

        ObjectNode assignmentNode = JsonNodeFactory.instance.objectNode();
        
        String launchPresentationReturnURL = com.horstmann.codecheck.Util.getParam(postParams, "launch_presentation_return_url");
        assignmentNode.put("launchPresentationReturnURL", launchPresentationReturnURL);
        assignmentNode.put("saveURL", "/lti/saveAssignment");
        assignmentNode.put("assignmentID", com.horstmann.codecheck.Util.createPublicUID());
        assignmentNode.put("editKey", userLMSID);
        
        return ok(views.html.editAssignment.render(assignmentNode.toString(), false));
    }

    private static String assignmentIDifAssignmentURL(String url) {
        if (url.contains("\n")) return null;
        Pattern pattern = Pattern.compile("https?://codecheck.[a-z]+/(private/)?(a|viewA)ssignment/([a-z0-9]+)($|/).*");
        Matcher matcher = pattern.matcher(url);
        return matcher.matches() ? matcher.group(3) : null; 
    }
    
    public Result saveAssignment(Http.Request request) throws IOException {         
        ObjectNode params = (ObjectNode) request.body().asJson();
                    
        String problemText = params.get("problems").asText().trim();
        String assignmentID = assignmentIDifAssignmentURL(problemText);
        if (assignmentID == null) {             
            try {
                params.set("problems", Assignment.parseAssignment(problemText));
            } catch (IllegalArgumentException e) {
                return badRequest(e.getMessage());
            }
    
            assignmentID = params.get("assignmentID").asText();
            ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
            String editKey = params.get("editKey").asText();
            
            if (assignmentNode != null && !editKey.equals(assignmentNode.get("editKey").asText())) 
                return badRequest("Edit keys do not match");        
    
            assignmentConn.writeJsonObjectToDB("CodeCheckAssignments", params);
        }

        ObjectNode result = JsonNodeFactory.instance.objectNode();
        String viewAssignmentURL = "/viewAssignment/" + assignmentID; 
        result.put("viewAssignmentURL", viewAssignmentURL);     
        String launchURL = "https://" + request.host() + "/assignment/" + assignmentID;
        result.put("launchURL", launchURL);     

        return ok(result); // Client will redirect to launch presentation URL 
    }
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result viewSubmissions(Http.Request request) throws IOException {
        String resourceID = request.queryString("resourceID").orElse(null);
        Map<String, ObjectNode> itemMap = assignmentConn.readJsonObjectsFromDB("CodeCheckWork", "assignmentID", resourceID, "workID");
        String assignmentID = assignmentOfResource(resourceID);
        
        ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
        if (assignmentNode == null) return badRequest("Assignment not found");

        ArrayNode submissions = JsonNodeFactory.instance.arrayNode();
        for (String workID : itemMap.keySet()) {
            ObjectNode work = itemMap.get(workID);
            ObjectNode submissionData = JsonNodeFactory.instance.objectNode();
            submissionData.put("opaqueID", workID);
            submissionData.put("score", Assignment.score(assignmentNode, work));
            submissionData.set("submittedAt", work.get("submittedAt"));
            submissionData.put("viewURL", "/lti/viewSubmission?resourceID=" 
                    + URLEncoder.encode(resourceID, "UTF-8") 
                    + "&workID=" + URLEncoder.encode(workID, "UTF-8")); 
            submissions.add(submissionData);
        }
        String allSubmissionsURL = "/lti/allSubmissions?resourceID=" + URLEncoder.encode(resourceID, "UTF-8");
        return ok(views.html.viewSubmissions.render(allSubmissionsURL, submissions.toString()));    
    }
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result viewSubmission(Http.Request request) throws IOException {
        String resourceID = request.queryString("resourceID").orElse(null);
        String workID = request.queryString("workID").orElse(null);
        String work = assignmentConn.readJsonStringFromDB("CodeCheckWork", "assignmentID", resourceID, "workID", workID);
        if (work == null) return badRequest("Work not found");
        String assignmentID = assignmentOfResource(resourceID);
        ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
        if (assignmentNode == null) return badRequest("Assignment not found");
        ArrayNode groups = (ArrayNode) assignmentNode.get("problems");
        assignmentNode.set("problems", groups.get(Math.abs(workID.hashCode()) % groups.size()));
        
        return ok(views.html.workAssignment.render(assignmentNode.toString(), work, workID, "undefined"));
    }
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result editAssignment(Http.Request request, String assignmentID) throws IOException {
        String editKey = request.session().get("user").get(); // TODO orElseThrow();    
        ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
        if (assignmentNode == null) return badRequest("Assignment not found");
        
        if (!editKey.equals(assignmentNode.get("editKey").asText())) 
            return badRequest("Edit keys don't match");
        assignmentNode.put("saveURL", "/lti/saveAssignment");       
        return ok(views.html.editAssignment.render(assignmentNode.toString(), false));      
    }

    private static ObjectNode bridgeAssignment(String url) {
    	ObjectNode assignment = JsonNodeFactory.instance.objectNode();
    	assignment.put("id", url);
    	assignment.put("editKey", "");
    	assignment.put("noHeader", true); // TODO for now
        ArrayNode groups = JsonNodeFactory.instance.arrayNode();
    	ArrayNode problems = JsonNodeFactory.instance.arrayNode();
    	ObjectNode problem = JsonNodeFactory.instance.objectNode();
    	problem.put("URL", url);
    	problem.put("weight", 1);
    	problems.add(problem);
    	groups.add(problems);
    	assignment.set("problems", groups);
    	return assignment;
    }
    
    private static Pattern isBridgeAssignment = Pattern.compile("^https?://.*$");
    
    private ObjectNode getAssignmentNode(String assignmentID) throws IOException {
    	if (isBridgeAssignment.matcher(assignmentID).matches()) return bridgeAssignment(assignmentID);
    	else return assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID); 
    }
    
    public Result launch(Http.Request request, String assignmentID) throws IOException {    
        Map<String, String[]> postParams = request.body().asFormUrlEncoded();
        if (!lti.validate(request)) {
            return badRequest("Failed OAuth validation");
        }       
        
        String userID = com.horstmann.codecheck.Util.getParam(postParams, "user_id");
        if (com.horstmann.codecheck.Util.isEmpty(userID)) return badRequest("No user id");

        String toolConsumerID = com.horstmann.codecheck.Util.getParam(postParams, "tool_consumer_instance_guid");
        String contextID = com.horstmann.codecheck.Util.getParam(postParams, "context_id");
        String resourceLinkID = com.horstmann.codecheck.Util.getParam(postParams, "resource_link_id");

        String userLMSID = toolConsumerID + "/" + userID;

        if (assignmentID == null) //TODO: Query string id legacy
            assignmentID = request.queryString("id").orElse(null);
        if (assignmentID == null) // Bridge
            assignmentID = request.queryString("url").orElse(null);

        ObjectNode ltiNode = JsonNodeFactory.instance.objectNode();
        // TODO: In order to facilitate search by assignmentID, it would be better if this was the other way around
        String resourceID = toolConsumerID + "/" + contextID + " " + assignmentID; 
        String legacyResourceID = toolConsumerID + "/" + contextID + "/" + resourceLinkID; 
        ObjectNode resourceNode = assignmentConn.readJsonObjectFromDB("CodeCheckLTIResources", "resourceID", legacyResourceID); 
        if (resourceNode != null) resourceID = legacyResourceID;
        
        if (assignmentID == null) {
            return badRequest("No assignment ID");
        } 
        ObjectNode assignmentNode = getAssignmentNode(assignmentID);
        if (isInstructor(postParams)) {     
            if (assignmentNode == null) return badRequest("Assignment not found");
            ArrayNode groups = (ArrayNode) assignmentNode.get("problems");
            assignmentNode.set("problems", groups.get(0));
            String assignmentEditKey = assignmentNode.get("editKey").asText();
            
            assignmentNode.put("isStudent", false);
            assignmentNode.put("viewSubmissionsURL", "/lti/viewSubmissions?resourceID=" + URLEncoder.encode(resourceID, "UTF-8"));
            if (userLMSID.equals(assignmentEditKey)) {
                assignmentNode.put("editAssignmentURL", "/lti/editAssignment/" + assignmentID);
                assignmentNode.put("cloneURL", "/copyAssignment/" + assignmentID);
            }
            assignmentNode.put("sentAt", Instant.now().toString());             
            String work = "{ problems: {} }";
            // Show the resource ID for troubleshooting
            return ok(views.html.workAssignment.render(assignmentNode.toString(), work, resourceID, "undefined" /* lti */))
                .withNewSession()
                .addingToSession(request, "user", userLMSID);
        } else { // Student
            if (assignmentNode == null) return badRequest("Assignment not found");
            ArrayNode groups = (ArrayNode) assignmentNode.get("problems");
            assignmentNode.set("problems", groups.get(Math.abs(userID.hashCode()) % groups.size()));
            assignmentNode.remove("editKey");

            String lisOutcomeServiceURL = com.horstmann.codecheck.Util.getParam(postParams, "lis_outcome_service_url");
            String lisResultSourcedID = com.horstmann.codecheck.Util.getParam(postParams, "lis_result_sourcedid");
            String oauthConsumerKey = com.horstmann.codecheck.Util.getParam(postParams, "oauth_consumer_key");
            
            if (com.horstmann.codecheck.Util.isEmpty(lisOutcomeServiceURL)) 
                return badRequest("lis_outcome_service_url missing.");
            else
                ltiNode.put("lisOutcomeServiceURL", lisOutcomeServiceURL);
            
            if (com.horstmann.codecheck.Util.isEmpty(lisResultSourcedID)) 
                return badRequest("lis_result_sourcedid missing.");
            else
                ltiNode.put("lisResultSourcedID", lisResultSourcedID);
            ltiNode.put("oauthConsumerKey", oauthConsumerKey);
            ltiNode.put("jwt", jwt.generate(Map.of("resourceID", resourceID, "userID", userID)));

            ObjectNode workNode = assignmentConn.readJsonObjectFromDB("CodeCheckWork", "assignmentID", resourceID, "workID", userID);
            String work = "";
            if (workNode == null) 
                work = "{ problems: {} }";
            else {
                // Delete assignmentID, workID since they are in jwt token
                workNode.remove("assignmentID");
                workNode.remove("workID");
                work = workNode.toString();
            }
            
            assignmentNode.put("isStudent", true);
            assignmentNode.put("editKeySaved", true);
            assignmentNode.put("sentAt", Instant.now().toString());     

            return ok(views.html.workAssignment.render(assignmentNode.toString(), work, userID, ltiNode.toString()));
        }
    }
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result allSubmissions(Http.Request request) throws IOException {
        String resourceID = request.queryString("resourceID").orElse(null);
        if (resourceID == null) return badRequest("Assignment not found");
        Map<String, ObjectNode> itemMap = assignmentConn.readJsonObjectsFromDB("CodeCheckWork", "assignmentID", resourceID, "workID");
        String assignmentID = assignmentOfResource(resourceID);
        
        ObjectNode assignmentNode = assignmentConn.readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
        if (assignmentNode == null) return badRequest("Assignment not found");

        ObjectNode submissions = JsonNodeFactory.instance.objectNode();
        for (String workID : itemMap.keySet()) {
            ObjectNode work = itemMap.get(workID);
            submissions.set(workID, work);
        }
        return ok(submissions);     
    }
    
    public Result saveWork(Http.Request request) throws IOException, NoSuchAlgorithmException {
        ObjectNode requestNode = (ObjectNode) request.body().asJson();
        try {
            ObjectNode workNode = (ObjectNode) requestNode.get("work");
            String token = requestNode.get("jwt").asText();
            Map<String, Object> claims = jwt.verify(token);
            if (claims == null) 
                return badRequest("Invalid token");
            
            ObjectNode result = JsonNodeFactory.instance.objectNode();
            Instant now = Instant.now();
            String resourceID = claims.get("resourceID").toString();
            workNode.put("assignmentID", resourceID);
            String assignmentID = assignmentOfResource(resourceID);
            String userID = claims.get("userID").toString();
            workNode.put("workID", userID);
            String problemID = workNode.get("tab").asText();
            ObjectNode problemsNode = (ObjectNode) workNode.get("problems");
            ObjectNode submissionNode = JsonNodeFactory.instance.objectNode();
            String submissionID = resourceID + " " + userID + " " + problemID; 
            submissionNode.put("submissionID", submissionID);
            submissionNode.put("submittedAt", now.toString());
            submissionNode.put("state", problemsNode.get(problemID).get("state").toString());
            submissionNode.put("score", problemsNode.get(problemID).get("score").asDouble());
            assignmentConn.writeJsonObjectToDB("CodeCheckSubmissions", submissionNode);
            
            ObjectNode assignmentNode = getAssignmentNode(assignmentID);
            if (assignmentNode.has("deadline")) {
                try {
                    Instant deadline = Instant.parse(assignmentNode.get("deadline").asText());
                    if (now.isAfter(deadline)) 
                        return badRequest("After deadline of " + deadline);
                } catch(DateTimeParseException e) { // TODO: This should never happen, but it did
                    logger.error(Util.getStackTrace(e));
                }
            }
            result.put("submittedAt", now.toString());      
            if (assignmentConn.writeNewerJsonObjectToDB("CodeCheckWork", workNode, "assignmentID", "submittedAt")) {
                // Don't submit grade if this is an older submission
                submitGradeToLMS(requestNode, (ObjectNode) requestNode.get("work"), result);
            }
            return ok(result);
        } catch (Exception e) {
            logger.error("saveWork: " + requestNode); 
            logger.error(Util.getStackTrace(e));
            return badRequest("saveWork: " + requestNode);
        }
    }   
    
    public Result sendScore(Http.Request request) throws IOException, NoSuchAlgorithmException {
        ObjectNode requestNode = (ObjectNode) request.body().asJson();
        ObjectNode result = JsonNodeFactory.instance.objectNode();
        result.put("submittedAt", Instant.now().toString());        
        try {
            String token = requestNode.get("jwt").asText();
            Map<String, Object> claims = jwt.verify(token);
            if (claims == null) 
                return badRequest("Invalid token");

            String userID = claims.get("userID").toString();
            String resourceID = claims.get("resourceID").toString();
            
            ObjectNode workNode = assignmentConn.readJsonObjectFromDB("CodeCheckWork", "assignmentID", resourceID, "workID", userID);
            if (workNode == null) return badRequest("Work not found");
            submitGradeToLMS(requestNode, workNode, result);
            String outcome = result.get("outcome").asText();
            if (!outcome.startsWith("success")) {
                logger.error("sendScore: " + requestNode);
                return badRequest(outcome);
            }
            return ok(result);
        } catch (Exception e) {
            logger.error("sendScore: " + requestNode + " " + e.getMessage());
            return badRequest("sendScore: " + requestNode);
        }
    }   
    
    private void submitGradeToLMS(ObjectNode params, ObjectNode work, ObjectNode result) 
            throws IOException, OAuthMessageSignerException, OAuthExpectationFailedException, OAuthCommunicationException, NoSuchAlgorithmException, URISyntaxException {
        String outcomeServiceUrl = params.get("lisOutcomeServiceURL").asText();
        String sourcedID = params.get("lisResultSourcedID").asText();
        String oauthConsumerKey = params.get("oauthConsumerKey").asText();

        String resourceID = work.get("assignmentID").asText();
        String assignmentID = assignmentOfResource(resourceID); 
        
        ObjectNode assignmentNode = getAssignmentNode(assignmentID);
        double score = Assignment.score(assignmentNode, work);
        result.put("score", score);     
        
        String outcome = lti.passbackGradeToLMS(outcomeServiceUrl, sourcedID, score, oauthConsumerKey); 
        // org.imsglobal.pox.IMSPOXRequest.sendReplaceResult(outcomeServiceUrl, oauthConsumerKey, getSharedSecret(oauthConsumerKey), sourcedId, "" + score);
        result.put("outcome", outcome);
    }   
}