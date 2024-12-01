package controllers;

import java.lang.System.Logger;
import java.util.Map;
import java.util.Optional;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import services.ServiceException;

public class LTIProblem extends Controller {
    @Inject private services.LTIProblem problemService;    
    private static Logger logger = System.getLogger("com.horstmann.codecheck");     
    
    public Result launch(Http.Request request) {    
        try {
	        String url = controllers.Util.prefix(request) + request.uri();
            String qid = request.queryString("qid").orElse(null);
	        Map<String, String[]> postParams = request.body().asFormUrlEncoded();        
	        String result = problemService.launch(url, qid, postParams);
            return ok(result).as("text/html");
	    }
		catch (ServiceException ex) {
			return badRequest(ex.getMessage());
		}
	    catch (Exception ex) {
	        logger.log(Logger.Level.ERROR, Util.getStackTrace(ex));
	        return internalServerError(Util.getStackTrace(ex));
	    }	        
    }       
    
    public Result launchCodeCheck(Http.Request request, String repo, String problemName) {
        try {           
	        String url = controllers.Util.prefix(request) + request.uri();
	        Map<String, String[]> postParams = request.body().asFormUrlEncoded();        
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            String ccid = ccidCookie.map(Http.Cookie::value).orElse(com.horstmann.codecheck.Util.createPronouncableUID());
            Http.Cookie newCookie = controllers.Util.buildCookie("ccid", ccid);
            String result = problemService.launchCodeCheck(url, repo, problemName, ccid, postParams);
            return ok(result).withCookies(newCookie).as("text/html");
        }
		catch (ServiceException ex) {
			return badRequest(ex.getMessage());
		}
	    catch (Exception ex) {
	        logger.log(Logger.Level.ERROR, Util.getStackTrace(ex));
	        return internalServerError(Util.getStackTrace(ex));
	    }	        
    }
    
    public Result launchTracer(Http.Request request, String repo, String problemName) {
        try {           
	        String url = controllers.Util.prefix(request) + request.uri();
	        Map<String, String[]> postParams = request.body().asFormUrlEncoded();        
            Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
            String ccid = ccidCookie.map(Http.Cookie::value).orElse(com.horstmann.codecheck.Util.createPronouncableUID());
            Http.Cookie newCookie = controllers.Util.buildCookie("ccid", ccid);
            String result = problemService.launchTracer(url, repo, problemName, ccid, postParams);
            return ok(result).withCookies(newCookie).as("text/html");
        }
		catch (ServiceException ex) {
			return badRequest(ex.getMessage());
		}
	    catch (Exception ex) {
	        logger.log(Logger.Level.ERROR, Util.getStackTrace(ex));
	        return internalServerError(Util.getStackTrace(ex));
	    }	        
    }
    
    public Result send(Http.Request request) {
    	try {
    		ObjectNode requestNode = (ObjectNode) request.body().asJson();
    		ObjectNode result = problemService.send(requestNode);
            return ok(result);
        }
		catch (ServiceException ex) {
			return badRequest(ex.getMessage());
		}
	    catch (Exception ex) {
	        logger.log(Logger.Level.ERROR, Util.getStackTrace(ex));
	        return internalServerError(Util.getStackTrace(ex));
	    }	        
    }
    
    public Result retrieve(Http.Request request) {
    	try {
    		ObjectNode requestNode = (ObjectNode) request.body().asJson();
    		ObjectNode result = problemService.retrieve(requestNode);
            return ok(result);
        }
		catch (ServiceException ex) {
			return badRequest(ex.getMessage());
		}
	    catch (Exception ex) {
	        logger.log(Logger.Level.ERROR, Util.getStackTrace(ex));
	        return internalServerError(Util.getStackTrace(ex));
	    }	        
    }   
}