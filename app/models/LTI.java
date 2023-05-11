package models;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.AbstractMap;
import java.util.Base64;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.net.ssl.HttpsURLConnection;

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
import play.Logger;
import play.mvc.Http;

@Singleton
public class LTI {
    @Inject private AssignmentConnector assignmentConn;
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
    
    public boolean validate(Http.Request request) {
        final String OAUTH_KEY_PARAMETER = "oauth_consumer_key";
        
        Map<String, String[]> postParams = request.body().asFormUrlEncoded();
        if (postParams == null) return false;
        Set<Map.Entry<String, String>> entries = new HashSet<>();
        for (Map.Entry<String, String[]> entry : postParams.entrySet()) 
            for (String s : entry.getValue())
                entries.add(new AbstractMap.SimpleEntry<>(entry.getKey(), s));
        String url = Util.prefix(request) + request.uri();
        
        String key = com.horstmann.codecheck.Util.getParam(postParams, OAUTH_KEY_PARAMETER);
        for (Map.Entry<String, String> entry : com.horstmann.codecheck.Util.getParams(url).entrySet())
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
            logger.error("Did not validate: " + e.getLocalizedMessage() + "\nurl: " + url + "\nentries: " + entries);
            return false;
        }
    }
            
    // TODO Move this so that LTI doesn't depend on S3, Play
    public String getSharedSecret(String oauthConsumerKey) {
        String sharedSecret = "";
        try {
            ObjectNode result = assignmentConn.readJsonObjectFromDB("CodeCheckLTICredentials", "oauth_consumer_key", oauthConsumerKey);
            if (result != null) sharedSecret = result.get("shared_secret").asText();
            else logger.warn("No shared secret for consumer key " + oauthConsumerKey);
        } catch (IOException e) {
            logger.warn("Could not read CodeCheckLTICredentials");
            // Return empty string
        }       
        return sharedSecret;
    }       

    private static Pattern codeMajorPattern = Pattern.compile("<imsx_codeMajor>(.+)</imsx_codeMajor>");
    private static Pattern descriptionPattern = Pattern.compile("(?s)<imsx_description>(.+)</imsx_description>");
    
    public String passbackGradeToLMS(String gradePassbackURL,
            String sourcedID, double score, String oauthConsumerKey)
            throws URISyntaxException, IOException,
            OAuthMessageSignerException, OAuthExpectationFailedException,
            OAuthCommunicationException, NoSuchAlgorithmException {
        String oauthSecret = getSharedSecret(oauthConsumerKey);
        String xmlString1 = "<?xml version = \"1.0\" encoding = \"UTF-8\"?> <imsx_POXEnvelopeRequest xmlns = \"http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0\"> <imsx_POXHeader> <imsx_POXRequestHeaderInfo> <imsx_version>V1.0</imsx_version> <imsx_messageIdentifier>" 
                + System.currentTimeMillis() + "</imsx_messageIdentifier> </imsx_POXRequestHeaderInfo> </imsx_POXHeader> <imsx_POXBody> <replaceResultRequest> <resultRecord> <sourcedGUID> <sourcedId>";
        String xmlString2 = "</sourcedId> </sourcedGUID> <result> <resultScore> <language>en</language> <textString>";
        String xmlString3 = "</textString> </resultScore> </result> </resultRecord> </replaceResultRequest> </imsx_POXBody> </imsx_POXEnvelopeRequest>";            
        String xml = xmlString1 + sourcedID + xmlString2 + score + xmlString3;          
        
        URL url = new URL(gradePassbackURL);
        HttpsURLConnection request = (HttpsURLConnection) url.openConnection();
        request.setRequestMethod("POST");
        request.setRequestProperty("Content-Type", "application/xml");
        //request.setRequestProperty("Authorization", "OAuth"); // Needed for Moodle???
        
        byte[] xmlBytes = xml.getBytes("UTF-8"); 
        request.setRequestProperty("Content-Length", Integer.toString(xmlBytes.length));

        // https://stackoverflow.com/questions/28204736/how-can-i-send-oauth-body-hash-using-signpost
        DefaultOAuthConsumer consumer = new DefaultOAuthConsumer(oauthConsumerKey, oauthSecret);
        consumer.setTokenWithSecret(null, null);
        MessageDigest md = MessageDigest.getInstance("SHA1");
        String bodyHash = Base64.getEncoder().encodeToString(md.digest(xmlBytes));
        HttpParameters params = new HttpParameters();
        params.put("oauth_body_hash", URLEncoder.encode(bodyHash, "UTF-8"));
        //params.put("realm", gradePassbackURL); // http://zewaren.net/site/?q=node/123
        consumer.setAdditionalParameters(params);        
        consumer.sign(request);         

        // logger.info("passbackGradeToLMS: URL {}, request {}, XML {}", url, new java.util.TreeMap<>(consumer.getRequestParameters()), xml);

        // POST the xml to the grade passback url
        request.setDoOutput(true);
        OutputStream out = request.getOutputStream();
        out.write(xmlBytes);
        out.close();
        // request.connect();
        if (request.getResponseCode() != 200)
            logger.warn("passbackGradeToLMS: Not successful" + request.getResponseCode() + " " + request.getResponseMessage());
        try {
            InputStream in = request.getInputStream();
            String body = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            Matcher matcher1 = codeMajorPattern.matcher(body);
            Matcher matcher2 = descriptionPattern.matcher(body);
            String message = "";
            if (matcher1.find()) message += matcher1.group(1);
            if (matcher2.find()) message += ": " + matcher2.group(1);
            if (message.length() == 0) message = body;
            if (!body.contains("<imsx_codeMajor>success</imsx_codeMajor>"))
                logger.warn("passbackGradeToLMS: Not successful " + body);
            return message;         
        } catch (Exception e) {         
            InputStream in = request.getErrorStream();
            String body = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            logger.warn("passbackGradeToLMS: Response error " + e.getMessage() + ": " + body);
            return body;
        }
    }           
}
