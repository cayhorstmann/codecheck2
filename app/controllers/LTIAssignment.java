package controllers;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.AbstractMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthConsumer;
import net.oauth.OAuthMessage;
import net.oauth.OAuthValidator;
import net.oauth.SimpleOAuthValidator;

import models.Util;
import models.S3Connection;
import play.Logger;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class LTIAssignment extends Controller {
	@Inject private S3Connection s3conn;
	private Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
	
	public static boolean isInstructor(String role) {
		return role != null && (role.contains("Faculty") || role.contains("TeachingAssistant") || role.contains("Instructor"));
	}

    public Result launch(Http.Request request) throws IOException {    
	 	Map<String, String[]> postParams = request.body().asFormUrlEncoded();
	 	logger.info("LTIAssignment.launch: " + Util.paramsToString(postParams));
	 	if (!validate(request)) {
	 		return badRequest("Failed OAuth validation").withNewSession();
	 	}	 	
	 	
    	String lisOutcomeServiceURL = Util.getParam(postParams, "lis_outcome_service_url");
    	String lisResultSourcedId = Util.getParam(postParams, "lis_result_sourcedid");
    	String oauthConsumerKey = Util.getParam(postParams, "oauth_consumer_key");
    	
    	String userID = Util.getParam(postParams, "user_id");
		if (Util.isEmpty(userID)) return badRequest("No user id");

		String toolConsumerID = Util.getParam(postParams, "tool_consumer_instance_guid");
		String contextID = Util.getParam(postParams, "context_id");
		String resourceLinkID = Util.getParam(postParams, "resource_link_id");
		String resourceID = toolConsumerID + "/" + contextID + "/" + resourceLinkID;
		
		String role = Util.getParam(postParams, "roles");
		String launchPresentationReturnURL = Util.getParam(postParams, "launch_presentation_return_url");
	    String assignmentID = request.queryString("id").orElse(null);
	    if (assignmentID == null) {
	    	ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckResources", "resourceID", resourceID); 
	    	if (resourceNode.has("assignmentID"))
	    		resourceNode.get("assignmentID").asText();
	    }
		
	    boolean isInstructor = isInstructor(role); 
	    
	    ObjectNode ltiNode = JsonNodeFactory.instance.objectNode();
	    ltiNode.put("launchPresentationReturnURL", launchPresentationReturnURL);
	    ltiNode.put("resourceID", resourceID);
	    
		if (assignmentID == null) {
			if (isInstructor)		
				return ok(views.html.editAssignment.render("{}", ltiNode.toString()))
						.addingToSession(request, "user", userID);
			else {
				String result = "No assignment id and no assignment for " + resourceID;
				logger.info(result);
				return badRequest(result);
			}
		}
		
		if (Util.isEmpty(lisOutcomeServiceURL)) 
          	return badRequest("lis_outcome_service_url missing.");
		else
			ltiNode.put("lisOutcomeServiceURL", lisOutcomeServiceURL);
		
		if (!isInstructor && Util.isEmpty(lisResultSourcedId)) 
			return badRequest("lis_result_sourcedid missing.");
		else
			ltiNode.put("lisResultSourcedId", lisResultSourcedId);
		ltiNode.put("oauthConsumerKey", oauthConsumerKey);
		// TODO: What else in the ltiNode
		String assignment = s3conn.readJsonStringFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
		String work = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID, "workID", userID);
		if (!isInstructor) work = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", assignmentID);
		// TODO: Set editable flag depending on instructor
		// TODO: In view, hide display of ccid options when lti
		return ok(views.html.workAssignment.render(assignment, work, userID, ltiNode.toString()))
			.addingToSession(request, "user", userID);
 	}		
    
    public boolean validate(Http.Request request) {
    	final String OAUTH_KEY_PARAMETER = "oauth_consumer_key";
    	
    	Map<String, String[]> postParams = request.body().asFormUrlEncoded();
    	if (postParams == null) return false;
    	Set<Map.Entry<String, String>> entries = new HashSet<>();
	 	for (Map.Entry<String, String[]> entry : postParams.entrySet()) 
	 		for (String s : entry.getValue())
	 			entries.add(new AbstractMap.SimpleEntry<>(entry.getKey(), s));	 	
	 	String url = (request.secure() ? "https://" : "http://") + request.host() + request.uri();
	 	String key = Util.getParam(postParams, OAUTH_KEY_PARAMETER);
	 	for (Map.Entry<String, String> entry : Util.getParams(url).entrySet())
	 		entries.add(entry);
	 	int n = url.lastIndexOf("?"); 
	 	if (n >= 0) url = url.substring(0, n);
	 	OAuthMessage oam = new OAuthMessage("POST", url, entries);
        OAuthConsumer cons = new OAuthConsumer(null, key, getSharedSecret(key), null); 
        OAuthValidator oav = new SimpleOAuthValidator();
        OAuthAccessor acc = new OAuthAccessor(cons);
        
        try {
	      oav.validateMessage(oam, acc);
          return true;
        } catch (Exception e) {
        	logger.info("Did not validate: " + e.getLocalizedMessage() + "\nurl: " + url + "\nentries: " + entries);
            return false;
        }
    }
            
	public String getSharedSecret(String oauthConsumerKey) {
		String sharedSecret = "";
		try {
			ObjectNode result = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTICredentials", "oauth_consumer_key", oauthConsumerKey);
			if (result != null) sharedSecret = result.get("shared_secret").asText();
			else logger.warn("No shared secret for consumer key " + oauthConsumerKey);
		} catch (IOException e) {
			logger.warn("Could not read CodeCheckLTICredentials");
			// Return empty string
		}		
		return sharedSecret;
	}	    
}