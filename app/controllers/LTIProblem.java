package controllers;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Problem;
import com.horstmann.codecheck.Util;

import models.AssignmentConnector;
import models.CodeCheck;
import models.LTI;
import play.Logger;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class LTIProblem extends Controller {
    @Inject private AssignmentConnector assignmentConn;
    @Inject private LTI lti;
    @Inject private CodeCheck codeCheck;
    
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
    
    private ObjectNode ltiNode(Http.Request request) {
        Map<String, String[]> postParams = request.body().asFormUrlEncoded();
        if (!lti.validate(request)) throw new IllegalArgumentException("Failed OAuth validation");
        
        String userID = com.horstmann.codecheck.Util.getParam(postParams, "user_id");
        if (com.horstmann.codecheck.Util.isEmpty(userID)) throw new IllegalArgumentException("No user id");

        String toolConsumerID = com.horstmann.codecheck.Util.getParam(postParams, "tool_consumer_instance_guid");
        String contextID = com.horstmann.codecheck.Util.getParam(postParams, "context_id");
        String resourceLinkID = com.horstmann.codecheck.Util.getParam(postParams, "resource_link_id");

        String resourceID = toolConsumerID + "/" + contextID + "/" + resourceLinkID; 
        

        ObjectNode ltiNode = JsonNodeFactory.instance.objectNode();

        ltiNode.put("lis_outcome_service_url", com.horstmann.codecheck.Util.getParam(postParams, "lis_outcome_service_url"));       
        ltiNode.put("lis_result_sourcedid", com.horstmann.codecheck.Util.getParam(postParams, "lis_result_sourcedid"));     
        ltiNode.put("oauth_consumer_key", com.horstmann.codecheck.Util.getParam(postParams, "oauth_consumer_key"));     

        ltiNode.put("submissionID", resourceID + " " + userID);     
        ltiNode.put("retrieveURL", "/lti/retrieve");
        ltiNode.put("sendURL", "/lti/send");
        
        return ltiNode;
    }
    
    private String rewriteRelativeLinks(String urlString) throws IOException {
        URL url = new URL(urlString);
        InputStream in = url.openStream();
        String contents = new String(in.readAllBytes(), StandardCharsets.UTF_8);
        in.close();
        int i1 = urlString.indexOf("/", 8); // after https://
        String domain = urlString.substring(0, i1);
        
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
        return document;
    }

    public Result launch(Http.Request request) throws IOException {    
        try {
            ObjectNode ltiNode = ltiNode(request);
            
            String qid = request.queryString("qid").orElse(null);
            // TODO: What about CodeCheck qids?
            if (qid == null) return badRequest("No qid");
            String domain = "https://www.interactivities.ws";
            String urlString = domain + "/" + qid + ".xhtml";
            String document = rewriteRelativeLinks(urlString);
            document = document.replace("<head>", "<head><script>const lti = " + ltiNode.toString() + "</script>");
            return ok(document).as("text/html");
        } catch (Exception ex) {
            logger.error("launch: Cannot load problem " + request, ex);
            return badRequest("Cannot load problem: " + ex.getMessage());
        }
    }       
    
    public Result launchCodeCheck(Http.Request request, String repo, String problemName) {
        try {           
            // TODO: Now the client will do the LTI communication. CodeCheck should do it.
            ObjectNode ltiNode = ltiNode(request);
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            String ccid = ccidCookie.map(Http.Cookie::value).orElse(com.horstmann.codecheck.Util.createPronouncableUID());
            
            Map<Path, byte[]> problemFiles = codeCheck.loadProblem(repo, problemName, ccid);

            Problem problem = new Problem(problemFiles);
            Problem.DisplayData data = problem.getProblemData();
            ObjectNode problemNode = models.Util.toJson(data);
            problemNode.put("url", "/checkNJS"); 
            problemNode.put("repo", repo);
            problemNode.put("problem", problemName);
            problemNode.remove("description"); // TODO: Or let node render it? 
            String qid = "codecheck-" + repo + "-" + problemName;
            String document = "<?xml version='1.0' encoding='UTF-8'?>\n" + 
                "<html xmlns='http://www.w3.org/1999/xhtml'>\n" + 
                "  <head>\n" + 
                "    <meta http-equiv='content-type' content='text/html; charset=UTF-8'/>\n" + 
                "    <title>Interactivities</title> \n" + 
                "    <script src='/assets/download.js'></script>\n" + 
                "    <script src='/assets/ace/ace.js'></script>\n" +
                "    <script src='/assets/ace/theme-kuroir.js'></script>\n" +
                "    <script src='/assets/ace/theme-chrome.js'></script>\n" +
                "    <script src='/assets/util.js'></script>\n" +
                "    <script src='/assets/codecheck2.js'></script>\n" +
                "    <script src='/assets/horstmann_codecheck.js'></script>\n" +
                "    <link type='text/css' rel='stylesheet' href='/assets/codecheck.css'/>\n" + 
                "    <link type='text/css' rel='stylesheet' href='/assets/horstmann_codecheck.css'/>\n" + 
                "    <style type='text/css'>\n" + 
                "      ol.interactivities > li {\n" + 
                "        list-style: none;\n" + 
                "        margin-bottom: 2em;\n" + 
                "      }\n" + 
                "      body {\n" + 
                "        margin-left: 2em;\n" + 
                "        margin-right: 2em;\n" + 
                "        overflow-y: visible;\n" + 
                "      }\n" + 
                "    </style>\n" + 
                "    <script type='text/javascript'>//<![CDATA[\n" +
                "horstmann_config.lti = " + ltiNode.toString() + 
                "\nhorstmann_codecheck.setup.push(\n" +
                problemNode.toString() + 
                ")\n" +
                "\n//]]></script>\n" + 
                "  </head> \n" + 
                "  <body>\n" + 
                "    <p>Submission ID: " + ltiNode.get("submissionID").asText() + "</p>" +
                "    <ol class='interactivities' id='interactivities'>\n" +
                "      <li title='" + qid + "' id='" + qid + "'>\n" + 
                "        <div class='hc-included'>\n" +
                (data.description == null ? "" : data.description) +
                "        </div>\n" + 
                "        <div class='horstmann_codecheck'>\n" +
                "        </div>\n" + 
                "      </li>\n" + 
                "    </ol>" +
                "  </body>" +
                "</html>";
            Http.Cookie newCookie = models.Util.buildCookie("ccid", ccid);         
            return ok(document).withCookies(newCookie).as("text/html");
        }  catch (Exception ex) {
            logger.error("launchCodeCheck: Cannot load problem " + repo + "/" + problemName, ex);
            return badRequest("Cannot load problem " + repo + "/" + problemName);
        }
    }
    
    private static String tracerStart = "<!DOCTYPE html>\n"
            + "<html>\n"
            + "<head>\n"
            + "  <meta charset=\"utf-8\">\n"
            + "  <link href='https://horstmann.com/codecheck/css/codecheck_tracer.css' rel='stylesheet' type='text/css'/>  "
            + "  <title>CodeCheck Tracer</title>\n"
            + "  <script src='/assets/util.js'></script>\n"
            + "  <script src='/assets/codecheck2.js'></script>\n"
            + "</head>\n"
            + "<body>\n";
    private static String tracerScriptStart = "    <div class='codecheck_tracer'>\n"
            + "      <script type='module'>//<![CDATA[\n";
    private static String tracerEnd = "// ]]>\n"
            + "      </script>\n"
            + "  </div>\n"
            + "</body>\n"
            + "</html>";    
    
    public Result launchTracer(Http.Request request, String repo, String problemName) {
        try {           
            ObjectNode ltiNode = ltiNode(request);
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            String ccid = ccidCookie.map(Http.Cookie::value).orElse(com.horstmann.codecheck.Util.createPronouncableUID());
            
            Map<Path, byte[]> problemFiles = codeCheck.loadProblem(repo, problemName, ccid);
    
            Problem problem = new Problem(problemFiles);
            StringBuilder result = new StringBuilder();
            Problem.DisplayData data = problem.getProblemData();            
            result.append(tracerStart);
            result.append("    <p>Submission ID: " + ltiNode.get("submissionID").asText() + "</p>");
            if (data.description != null)
                result.append(data.description);
            result.append(tracerScriptStart);
            result.append("horstmann_config.lti = " + ltiNode.toString() + "\n");
            result.append(Util.getString(problemFiles, Path.of("tracer.js")));
            result.append(tracerEnd);
            Http.Cookie newCookie = models.Util.buildCookie("ccid", ccid);         
            return ok(result.toString()).withCookies(newCookie).as("text/html");
        }  catch (Exception ex) {
            logger.error("launchTracer: Cannot load problem " + repo + "/" + problemName, ex);
            return badRequest("Cannot load problem " + repo + "/" + problemName);
        }    
    }
    
    public Result send(Http.Request request) throws IOException, NoSuchAlgorithmException {
        ObjectNode requestNode = (ObjectNode) request.body().asJson();
        String submissionID = requestNode.get("submissionID").asText();
        try {
            String submittedAt = Instant.now().toString();
            ObjectNode submissionNode = JsonNodeFactory.instance.objectNode();
            submissionNode.put("submissionID", submissionID);
            submissionNode.put("submittedAt", submittedAt);
            submissionNode.put("state", requestNode.get("state").toString());
            double score = requestNode.get("score").asDouble();         
            submissionNode.put("score", score);
            assignmentConn.writeJsonObjectToDB("CodeCheckSubmissions", submissionNode);
            
            String outcomeServiceUrl = requestNode.get("lis_outcome_service_url").asText();
            String sourcedID = requestNode.get("lis_result_sourcedid").asText();
            String oauthConsumerKey = requestNode.get("oauth_consumer_key").asText();                       
            lti.passbackGradeToLMS(outcomeServiceUrl, sourcedID, score, oauthConsumerKey); 

            ObjectNode resultNode = JsonNodeFactory.instance.objectNode();
            resultNode.put("score", score);
            resultNode.put("submittedAt", submittedAt);
            return ok(resultNode);
        } catch (Exception e) {
            logger.error("send: Cannot send submission " + submissionID + " " + e.getMessage());
            return badRequest(e.getMessage());
        }
    }
    
    public Result retrieve(Http.Request request) throws IOException {
        ObjectNode requestNode = (ObjectNode) request.body().asJson();
        String submissionID = requestNode.get("submissionID").asText();
        try {
            ObjectNode result = assignmentConn.readNewestJsonObjectFromDB("CodeCheckSubmissions", "submissionID", submissionID);          
            ObjectMapper mapper = new ObjectMapper();
            result.set("state", mapper.readTree(result.get("state").asText()));
            return ok(result);
        } catch (Exception e) {
            logger.error("retrieve: Cannot retrieve submission " + submissionID + " " + e.getClass() + " " + e.getMessage());
            return badRequest("retrieve: Cannot retrieve submission " + submissionID);
        }
    }   
}