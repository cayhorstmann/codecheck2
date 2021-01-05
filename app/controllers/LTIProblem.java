package controllers;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import models.LTI;
import models.S3Connection;
import models.Util;
import play.Logger;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class LTIProblem extends Controller {
	@Inject private S3Connection s3conn;
	@Inject private LTI lti;
	
	private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");

    public Result launch(Http.Request request) throws IOException {    
	 	Map<String, String[]> postParams = request.body().asFormUrlEncoded();
	 	if (!lti.validate(request)) return badRequest("Failed OAuth validation");
	 	
    	String userID = Util.getParam(postParams, "user_id");
		if (Util.isEmpty(userID)) return badRequest("No user id");

		String toolConsumerID = Util.getParam(postParams, "tool_consumer_instance_guid");
		String contextID = Util.getParam(postParams, "context_id");
		String resourceLinkID = Util.getParam(postParams, "resource_link_id");

		String resourceID = toolConsumerID + "/" + contextID + "/" + resourceLinkID; 
	    
	    String qid = request.queryString("qid").orElse(null);

    	if (qid == null) return badRequest("No qid");

    	ObjectNode ltiNode = JsonNodeFactory.instance.objectNode();

		ltiNode.put("lis_outcome_service_url", Util.getParam(postParams, "lis_outcome_service_url"));		
		ltiNode.put("lis_result_sourcedid", Util.getParam(postParams, "lis_result_sourcedid"));		
		ltiNode.put("oauth_consumer_key", Util.getParam(postParams, "oauth_consumer_key"));		

		ltiNode.put("submissionID", resourceID + " " + userID);		
		ltiNode.put("retrieveURL", "/lti/retrieve");
		ltiNode.put("sendURL", "/lti/send");
		
		try {
			String domain = "https://www.interactivities.ws";
			String urlString = domain + "/" + qid + ".xhtml";
			URL url = new URL(urlString);
			InputStream in = url.openStream();
			String contents = new String(Util.readAllBytes(in), StandardCharsets.UTF_8);
			in.close();
			
			contents = contents.replace("<head>", "<head><script>const lti = " + ltiNode.toString() + "</script>");
			Pattern pattern = Pattern.compile("\\s+(src|href)=[\"']([^\"']+)[\"']");
			Matcher matcher = pattern.matcher(contents);
			int previousEnd = 0;
			String document = "";
			while (matcher.find()) {
				int start = matcher.start();
				document += contents.substring(previousEnd, start);
				String group1 = matcher.group(1);
				String group2 = matcher.group(2);
				document += " " + group1 + "='";
				if (group2.startsWith("http:") || group2.startsWith("https:") || group2.startsWith("data:"))
					document += group2;
				else if (group2.startsWith("/"))
					document += domain + "/" + group2;
				else if (group2.equals("assets/receiveMessage.js")){ // TODO: Hack?
					document += "/" + group2;
				} else {
					int i = urlString.lastIndexOf("/");
					document += urlString.substring(0, i + 1) + group2;
				}				
				document += "'";
				previousEnd = matcher.end();
			}			
			document += contents.substring(previousEnd);
			return ok(document).as("text/html");
		} catch (Exception ex) {
			logger.info(Util.getStackTrace(ex));
			return badRequest(ex.getMessage());
		}
 	}		
	
	public Result send(Http.Request request) throws IOException, NoSuchAlgorithmException {
		try {
			ObjectNode requestNode = (ObjectNode) request.body().asJson();
	    	Instant now = Instant.now();
			String submissionID = requestNode.get("submissionID").asText();
			ObjectNode submissionNode = JsonNodeFactory.instance.objectNode();
			submissionNode.put("submissionID", submissionID);
			submissionNode.put("submittedAt", now.toString());
			submissionNode.put("state", requestNode.get("state").toString());
			double score = requestNode.get("score").asDouble();			
			submissionNode.put("score", score);
			s3conn.writeJsonObjectToDynamoDB("CodeCheckSubmissions", submissionNode);
			
	        String outcomeServiceUrl = requestNode.get("lis_outcome_service_url").asText();
			String sourcedID = requestNode.get("lis_result_sourcedid").asText();
			String oauthConsumerKey = requestNode.get("oauth_consumer_key").asText();						
	        lti.passbackGradeToLMS(outcomeServiceUrl, sourcedID, score, oauthConsumerKey); 
			
			return ok("");
        } catch (Exception e) {
            logger.error(Util.getStackTrace(e));
            return badRequest(e.getMessage());
        }
	}
	
	public Result retrieve(Http.Request request) throws IOException {
		try {
			ObjectNode requestNode = (ObjectNode) request.body().asJson();
			String submissionID = requestNode.get("submissionID").asText();
			ObjectNode result = s3conn.readJsonObjectFromDynamoDB("CodeCheckSubmissions", "submissionID", submissionID);
			ObjectMapper mapper = new ObjectMapper();
			result.set("state", mapper.valueToTree(result.get("state").asText()));
			return ok(result);
	    } catch (Exception e) {
	        logger.error(Util.getStackTrace(e));
	        return badRequest(e.getMessage());
	    }
	}	
}