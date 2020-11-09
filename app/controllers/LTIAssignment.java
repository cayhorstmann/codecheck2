package controllers;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.AbstractMap;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import javax.inject.Inject;
import javax.net.ssl.HttpsURLConnection;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthConsumer;
import net.oauth.OAuthMessage;
import net.oauth.OAuthValidator;
import net.oauth.SimpleOAuthValidator;
import oauth.signpost.basic.DefaultOAuthConsumer;
import oauth.signpost.exception.OAuthCommunicationException;
import oauth.signpost.exception.OAuthExpectationFailedException;
import oauth.signpost.exception.OAuthMessageSignerException;
import oauth.signpost.http.HttpParameters;
import models.Util;
import models.S3Connection;
import play.Logger;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Http.Session;
import play.mvc.Result;
import play.mvc.Security;

   
/*
 * 
Session cookies (LTI only)
  "user": toolConsumerID + "/" + userID (needs toolConsumerID because it's used for secure identification)
  "resource": toolConsumerID + "/" + contextID + "/" + resourceLinkID
*/


public class LTIAssignment extends Controller {
	@Inject private S3Connection s3conn;
	private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
	
	public static boolean isInstructor(Map<String, String[]> postParams) {
		String role = Util.getParam(postParams, "roles");
		return role != null && (role.contains("Faculty") || role.contains("TeachingAssistant") || role.contains("Instructor"));
	}
	
    public Result config(Http.Request request) throws UnknownHostException {
        String host = request.host();
        if (host.endsWith("/")) host = host.substring(0, host.length() - 1);
        return ok(views.xml.lti_config.render(host)).as("application/xml");			
    }     

    /*
     * Called from Canvas and potentially other LMS with a "resource selection" interface
     */
    public Result createAssignment(Http.Request request) throws UnsupportedEncodingException {    
	 	Map<String, String[]> postParams = request.body().asFormUrlEncoded();
	 	if (!validate(request)) {
	 		return badRequest("Failed OAuth validation");
	 	}	 	
	 	
		if (!isInstructor(postParams)) 
			return badRequest("Instructor role is required to create an assignment.");
    	String userID = Util.getParam(postParams, "user_id");
		if (Util.isEmpty(userID)) 
			return badRequest("No user id");

		String contextID = Util.getParam(postParams, "context_id");
		String resourceLinkID = Util.getParam(postParams, "resource_link_id");
		String toolConsumerID = Util.getParam(postParams, "tool_consumer_instance_guid");
		String resourceID = toolConsumerID + "/" + contextID + "/" + resourceLinkID;

		ObjectNode assignmentNode = JsonNodeFactory.instance.objectNode();
		
		String launchPresentationReturnURL = Util.getParam(postParams, "launch_presentation_return_url");
	    assignmentNode.put("launchPresentationReturnURL", launchPresentationReturnURL);

		return ok(views.html.editAssignment.render(assignmentNode.toString(), false));
 	}
    
	@Security.Authenticated(Secured.class) // Instructor
	public Result saveAssignment(Http.Request request) throws IOException {		
		// TODO Eliminate session
    	String editKey = request.session().get("user").get(); // TODO orElseThrow();    	
    	String resourceID = request.session().get("resource").get(); // TODO orElseThrow();

    	ObjectNode params = (ObjectNode) request.body().asJson();
        	
        String assignmentText = params.get("problems").asText();
    	params.set("problems", Assignment.parseAssignment(assignmentText));

    	ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTIResources", "resourceID", resourceID); 

    	String assignmentID;
    	if (resourceNode == null) { // New assignment 
    		assignmentID = Util.createPublicUID();
        	params.put("assignmentID", assignmentID);
       		params.put("editKey", editKey);
    		ObjectNode res = JsonNodeFactory.instance.objectNode();
       		res.put("resourceID", resourceID);
       		res.put("assignmentID", assignmentID);
       		s3conn.writeJsonObjectToDynamoDB("CodeCheckLTIResources", res);
    	} else {
    		assignmentID = params.get("assignmentID").asText();
    		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    		if (assignmentNode == null) return badRequest("Assignment not found");
    		if (!editKey.equals(assignmentNode.get("editKey").asText())) 
    			return badRequest("Edit keys do not match");
    	}

    	String launchPresentationReturnURL = params.has("launchPresentationReturnURL") ? 
    			params.get("launchPresentationReturnURL").asText() : null;
    	params.remove("launchPresentationReturnURL");

    	s3conn.writeJsonObjectToDynamoDB("CodeCheckAssignments", params);

    	ObjectNode result = JsonNodeFactory.instance.objectNode();
		String assignmentURL = Util.prefix(request) + "lti/assignment?id=" + assignmentID;
    	result.put("assignmentURL", assignmentURL);    	

   		/*
   		 * Call launchPresentationReturnURL with:
   		 * return_type=lti_launch_url
   		 * url=assignment URL (with id=...)
   		 * Util.getParams(launchPresentationReturnURL)
   		 */
    	/*
    	if (launchPresentationReturnURL != null) {
			launchPresentationReturnURL = launchPresentationReturnURL
					+ (launchPresentationReturnURL.contains("?") ? "&" : "?")     					
					+ "return_type=lti_launch_url"
					+ "&url=" + URLEncoder.encode(assignmentURL, "UTF-8"); // TODO StandardCharsets.UTF_8 
			new URL(launchPresentationReturnURL).openStream().close();
			// TODO: Capture output from above and return?
    	}
	*/
    	return ok(result); // TODO: Maybe we need to let the client redirect ???
	}

	@Security.Authenticated(Secured.class) // Instructor
	public Result viewSubmissions(Http.Request request) throws IOException {		
		// TODO eliminate session
    	String resourceID = request.session().get("resource").get(); // TODO orElseThrow();
    	ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTIResources", "resourceID", resourceID); 
    	if (resourceNode == null) return badRequest("No resource");

    	String assignmentID = resourceNode.get("assignmentID").asText();    	
		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);

    	Map<String, ObjectNode> itemMap = s3conn.readJsonObjectsFromDynamoDB("CodeCheckWork", "assignmentID", resourceID, "workID");

		ArrayNode submissions = JsonNodeFactory.instance.arrayNode();
		for (String workID : itemMap.keySet()) {
			ObjectNode work = itemMap.get(workID);
			ObjectNode submissionData = JsonNodeFactory.instance.objectNode();
			submissionData.put("opaqueID", workID);
			submissionData.put("score", Assignment.score(assignmentNode, work));
			submissionData.set("submittedAt", work.get("submittedAt"));
			submissionData.put("viewURL", "/lti/viewSubmission/" + workID); 
			submissions.add(submissionData);
		}
		return ok(views.html.viewSubmissions.render(submissions.toString())); 	
	}
	
	@Security.Authenticated(Secured.class) // Instructor
	public Result viewSubmission(Http.Request request, String workID) throws IOException {
		// TODO eliminate session
    	String resourceID = request.session().get("resource").get(); // TODO orElseThrow();
    	String work = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", resourceID, "workID", workID);
    	if (work == null) return badRequest("Work not found");
    	ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTIResources", "resourceID", resourceID); 
    	if (resourceNode == null) return badRequest("Resource not found");
    	String assignmentID = resourceNode.get("assignmentID").asText();    	
		String assignment = s3conn.readJsonStringFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    	if (assignment == null) return badRequest("Assignment not found");
    	return ok(views.html.workAssignment.render(assignment, work, workID, "undefined"));
	}
	
	@Security.Authenticated(Secured.class) // Instructor
	public Result editAssignment(Http.Request request) throws IOException {
		// TODO eliminate session
    	String editKey = request.session().get("user").get(); // TODO orElseThrow();    	
    	String resourceID = request.session().get("resource").get(); // TODO orElseThrow();
    	ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTIResources", "resourceID", resourceID); 
    	if (resourceNode == null) return badRequest("Resource not found");
    	String assignmentID = resourceNode.get("assignmentID").asText();    	
		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
    	if (assignmentNode == null) return badRequest("Assignment not found");
		if (!editKey.equals(assignmentNode.get("editKey").asText())) 
			return badRequest("Edit keys don't match");
    	assignmentNode.put("saveURL", "/lti/saveAssignment");		
		return ok(views.html.editAssignment.render(assignmentNode.toString(), false));		
	}
	
	/*
	 
Student:
  No resource or assignment IDs don't match => fail
  Otherwise => work
Instructor:
  No resource?
    Without assignment ID => create with edit key = context + user ID  
    With assignment ID: add resource->assignment mapping => view
  Resource?
    Without assignment ID => view resource assignment ID
    With assignment ID: If don't match, update, then view assignment ID
  	 
	 */
	
    public Result launch(Http.Request request, String assignmentID) throws IOException {    
	 	Map<String, String[]> postParams = request.body().asFormUrlEncoded();
	 	logger.info("LTIAssignment.launch: " + Util.paramsToString(postParams));
	 	if (!validate(request)) {
	 		return badRequest("Failed OAuth validation");
	 	}	 	
	 	
    	String userID = Util.getParam(postParams, "user_id");
		if (Util.isEmpty(userID)) return badRequest("No user id");

		String toolConsumerID = Util.getParam(postParams, "tool_consumer_instance_guid");
		String contextID = Util.getParam(postParams, "context_id");
		String resourceLinkID = Util.getParam(postParams, "resource_link_id");
		String resourceID = toolConsumerID + "/" + contextID + "/" + resourceLinkID;

		ObjectNode ltiNode = JsonNodeFactory.instance.objectNode();
	    ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTIResources", "resourceID", resourceID); 
		
	    if (assignmentID == null)
	    	assignmentID = request.queryString("id").orElse(null);
    	String resourceAssignmentID = resourceNode == null ? null : resourceNode.get("assignmentID").asText(); 
	    
	    if (isInstructor(postParams)) {		
		    if (assignmentID == null && resourceAssignmentID == null) { // Create new assignment
		    	ObjectNode assignmentNode = JsonNodeFactory.instance.objectNode();
		    	assignmentNode.put("saveURL", "/lti/saveAssignment");
			    String launchPresentationReturnURL = Util.getParam(postParams, "launch_presentation_return_url");
			    if (launchPresentationReturnURL != null)
			    	assignmentNode.put("launchPresentationReturnURL", launchPresentationReturnURL);

				return ok(views.html.editAssignment.render(assignmentNode.toString(), false))
					.withNewSession()
					.addingToSession(request, "user", toolConsumerID + "/" + userID)
					.addingToSession(request, "resource", resourceID);  		
		    }
		    else if (assignmentID == null && resourceAssignmentID != null) 
		    	assignmentID = resourceAssignmentID;
		    else if (assignmentID != null && !assignmentID.equals(resourceAssignmentID)) {
		    	ObjectNode res = JsonNodeFactory.instance.objectNode();
	   			res.put("resourceID", resourceID);
	   			res.put("assignmentID", assignmentID);
	   			s3conn.writeJsonObjectToDynamoDB("CodeCheckLTIResources", res);
		    }
			ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
			if (assignmentNode == null)
				return badRequest("No assignment " + assignmentID);
	    	assignmentNode.remove("editKey");
			
			assignmentNode.put("isStudent", false);
			assignmentNode.put("viewSubmissionsURL", "/lti/viewSubmissions");
			assignmentNode.put("editAssignmentURL", "/lti/editAssignment");
	    	assignmentNode.put("sentAt", Instant.now().toString());				
	    	String work = "{ assignmentID: '" + resourceID + "', workID: '" + userID + "', problems: {} }";
			return ok(views.html.workAssignment.render(assignmentNode.toString(), work, userID, "undefined" /* lti */))
				.withNewSession()
				.addingToSession(request, "user", toolConsumerID + "/" + userID)
				.addingToSession(request, "resource", resourceID);  					
	    } else { // Student
		    if (resourceNode == null) 
	    		return badRequest("No resource with ID " + resourceID);	    		
	    	else if (assignmentID != null && !assignmentID.equals(resourceAssignmentID))
	    		return badRequest("Assignment IDs do not match");
	    	else {
	    		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", resourceAssignmentID);
	        	assignmentNode.remove("editKey");

	    		String lisOutcomeServiceURL = Util.getParam(postParams, "lis_outcome_service_url");
	        	String lisResultSourcedID = Util.getParam(postParams, "lis_result_sourcedid");
	        	String oauthConsumerKey = Util.getParam(postParams, "oauth_consumer_key");
	        	
	    	    if (Util.isEmpty(lisOutcomeServiceURL)) 
	              	return badRequest("lis_outcome_service_url missing.");
	    		else
	    			ltiNode.put("lisOutcomeServiceURL", lisOutcomeServiceURL);
	    		
	    		if (Util.isEmpty(lisResultSourcedID)) 
	    			return badRequest("lis_result_sourcedid missing.");
	    		else
	    			ltiNode.put("lisResultSourcedID", lisResultSourcedID);
	    		ltiNode.put("oauthConsumerKey", oauthConsumerKey);
	    		
				String work = s3conn.readJsonStringFromDynamoDB("CodeCheckWork", "assignmentID", resourceID, "workID", userID);
				if (work == null) 
					work = "{ assignmentID: '" + resourceID + "', workID: '" + userID + "', problems: {} }";
	    		
	    		assignmentNode.put("isStudent", true);
	        	assignmentNode.put("editKeySaved", true);
	        	assignmentNode.put("sentAt", Instant.now().toString());		

	        	return ok(views.html.workAssignment.render(assignmentNode.toString(), work, userID, ltiNode.toString()));
	    	}	    	
	    }
 	}		
    
    private boolean validate(Http.Request request) {
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
            
	private String getSharedSecret(String oauthConsumerKey) {
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

	// TODO @Security.Authenticated(Secured.class) // Student
	public Result saveWork(Http.Request request) throws IOException, NoSuchAlgorithmException {
		try {
			ObjectNode requestNode = (ObjectNode) request.body().asJson();
			ObjectNode workNode = (ObjectNode) requestNode.get("work");
	    	ObjectNode result = JsonNodeFactory.instance.objectNode();
	    	result.put("submittedAt", Instant.now().toString());    	
	
			s3conn.writeNewerJsonObjectToDynamoDB("CodeCheckWork", workNode, "assignmentID", "submittedAt");
			double score = submitGradeToLMS(requestNode, (ObjectNode) requestNode.get("work"));
	    	result.put("score", score);    	
			return ok(result);
        } catch (Exception e) {
            logger.info(Util.getStackTrace(e));
            return badRequest(e.getMessage());
        }
	}	
	
	// TODO @Security.Authenticated(Secured.class) // Student
	public Result sendScore(Http.Request request) throws IOException, NoSuchAlgorithmException {
		ObjectNode requestNode = (ObjectNode) request.body().asJson();
    	ObjectNode result = JsonNodeFactory.instance.objectNode();
    	result.put("submittedAt", Instant.now().toString());    	
		try {
			String workID = requestNode.get("workID").asText();
			String resourceID = requestNode.get("resourceID").asText();
	    	ObjectNode workNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckWork", "assignmentID", resourceID, "workID", workID);
	    	if (workNode == null) return badRequest("Work not found");
			double score = submitGradeToLMS(requestNode, workNode);
	    	result.put("score", score);    	
			return ok(result);
        } catch (Exception e) {
            logger.info(Util.getStackTrace(e));
            return badRequest(e.getMessage());
        }
	}	
	
	
	private double submitGradeToLMS(ObjectNode params, ObjectNode work) 
			throws IOException, OAuthMessageSignerException, OAuthExpectationFailedException, OAuthCommunicationException, NoSuchAlgorithmException, URISyntaxException {
        String outcomeServiceUrl = params.get("lisOutcomeServiceURL").asText();
		String sourcedId = params.get("lisResultSourcedID").asText();
		String oauthConsumerKey = params.get("oauthConsumerKey").asText();

		String resourceID = work.get("assignmentID").asText();
    	ObjectNode resourceNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckLTIResources", "resourceID", resourceID); 
	    String assignmentID = resourceNode.get("assignmentID").asText(); 
        
		ObjectNode assignmentNode = s3conn.readJsonObjectFromDynamoDB("CodeCheckAssignments", "assignmentID", assignmentID);
		double score = Assignment.score(assignmentNode, work);
		
		String xmlString1 = "<?xml version = \"1.0\" encoding = \"UTF-8\"?> <imsx_POXEnvelopeRequest xmlns = \"http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0\"> <imsx_POXHeader> <imsx_POXRequestHeaderInfo> <imsx_version>V1.0</imsx_version> <imsx_messageIdentifier>" 
            + System.currentTimeMillis() + "</imsx_messageIdentifier> </imsx_POXRequestHeaderInfo> </imsx_POXHeader> <imsx_POXBody> <replaceResultRequest> <resultRecord> <sourcedGUID> <sourcedId>";
		String xmlString2 = "</sourcedId> </sourcedGUID> <result> <resultScore> <language>en</language> <textString>";
		String xmlString3 = "</textString> </resultScore> </result> </resultRecord> </replaceResultRequest> </imsx_POXBody> </imsx_POXEnvelopeRequest>";        	
		String xmlString = xmlString1 + sourcedId + xmlString2 + score + xmlString3;        	
			
        passbackGradeToLMS(outcomeServiceUrl, xmlString, oauthConsumerKey, 
        		getSharedSecret(oauthConsumerKey)); 
		
		// org.imsglobal.pox.IMSPOXRequest.sendReplaceResult(outcomeServiceUrl, oauthConsumerKey, getSharedSecret(oauthConsumerKey), sourcedId, "" + score);

        return score;
    }

	/**
	 * Pass back the grade to Canvas. If the <code>xml</code> is set up with
	 * fetch URL string, then also pass back the URL to the codecheck report.
	 * 
	 * @param gradePassbackURL
	 *            the grade passback URL from the LTI launch
	 * @param xml
	 *            the data to send off, with the sourcedId, score, and possibly
	 *            the fetchURL
	 * @param oauthKey
	 *            the oauth consumer key
	 * @param oauthSecret
	 *            the oauth secret key
	 * @throws NoSuchAlgorithmException 
	 */
	private static void passbackGradeToLMS(String gradePassbackURL,
			String xml, String oauthKey, String oauthSecret)
			throws URISyntaxException, IOException,
			OAuthMessageSignerException, OAuthExpectationFailedException,
			OAuthCommunicationException, NoSuchAlgorithmException {
		// Create an oauth consumer in order to sign the grade that will be sent.
		DefaultOAuthConsumer consumer = new DefaultOAuthConsumer(oauthKey, oauthSecret);

		consumer.setTokenWithSecret(null, null);

		// This is the URL that we will send the grade to so it can go back to
		// Canvas
		URL url = new URL(gradePassbackURL);

		// This is the part where we send the HTTP request
		HttpsURLConnection request = (HttpsURLConnection) url.openConnection();

		// Set http request to POST
		request.setRequestMethod("POST");

		// Set the content type to accept xml
		request.setRequestProperty("Content-Type", "application/xml");
		//request.setRequestProperty("Authorization", "OAuth"); // Needed for Moodle???
		
		// Set the content-length to be the length of the xml
		byte[] xmlBytes = xml.getBytes("UTF-8"); 
		request.setRequestProperty("Content-Length",
				Integer.toString(xmlBytes.length));
		// https://stackoverflow.com/questions/28204736/how-can-i-send-oauth-body-hash-using-signpost
		MessageDigest md = MessageDigest.getInstance("SHA1");
		String bodyHash = Base64.getEncoder().encodeToString(md.digest(xmlBytes));
		HttpParameters params = new HttpParameters();
        params.put("oauth_body_hash", URLEncoder.encode(bodyHash, "UTF-8"));
        //params.put("realm", gradePassbackURL); // http://zewaren.net/site/?q=node/123
        consumer.setAdditionalParameters(params);
        
		consumer.sign(request); // Throws OAuthMessageSignerException,
				// OAuthExpectationFailedException,
				// OAuthCommunicationException		
		logger.info("Request after signing: {}", consumer.getRequestParameters());
		logger.info("XML: {}", xml);


		// POST the xml to the grade passback url
		request.setDoOutput(true);
		OutputStream out = request.getOutputStream();
		out.write(xmlBytes);
		out.close();

		// request.connect();
		logger.info(request.getResponseCode() + " " + request.getResponseMessage());
		try {
			InputStream in = request.getInputStream();
			String body = new String(Util.readAllBytes(in), "UTF-8");
			logger.info("Response body received from LMS: " + body);
		} catch (Exception e) {			
			InputStream in = request.getErrorStream();
			String body = new String(Util.readAllBytes(in), "UTF-8");
			logger.info("Response error received from LMS: " + body);
		}
	}		
}