package controllers;

import java.lang.System.Logger;
import java.util.Map;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import play.mvc.Security;
import services.ServiceException;

   
/*
Session cookie (LTI Instructors only)
  "user": toolConsumerID + "/" + userID (needs toolConsumerID because it's used for secure identification)

TODO: Can/should this be replaced with JWT? 

*/


public class LTIAssignment extends Controller {
    @Inject private services.LTIAssignment assignmentService;
    
    private static Logger logger = System.getLogger("com.horstmann.codecheck");     
    
    public Result config(Http.Request request) {
        String host = request.host();
        String result = assignmentService.config(host);
        return ok(result).as("application/xml");         
    }     
    
    public Result createAssignment(Http.Request request) {
    	try {
	        String url = controllers.Util.prefix(request) + request.uri();
	        Map<String, String[]> postParams = request.body().asFormUrlEncoded();
	        String result = assignmentService.createAssignment(url, postParams);
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

    public Result saveAssignment(Http.Request request) {
    	try {
    		ObjectNode params = (ObjectNode) request.body().asJson();
    		String host = request.host();
    		ObjectNode result = assignmentService.saveAssignment(host, params);
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
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result viewSubmissions(Http.Request request) {
    	try {
    		String resourceID = request.queryString("resourceID").orElse(null);
    		String result = assignmentService.viewSubmissions(resourceID);
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
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result viewSubmission(Http.Request request) {
    	try {
	        String resourceID = request.queryString("resourceID").orElse(null);
	        String workID = request.queryString("workID").orElse(null);
	        String result = assignmentService.viewSubmission(resourceID, workID);
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
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result editAssignment(Http.Request request, String assignmentID) {
    	try {
    		String editKey = request.session().get("user").get(); // TODO orElseThrow();    
    		String result = assignmentService.editAssignment(assignmentID, editKey);
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

    public Result launch(Http.Request request, String assignmentID) {
    	try {
	        String url = controllers.Util.prefix(request) + request.uri();
	        if (assignmentID == null) //TODO: Query string id legacy
	            assignmentID = request.queryString("id").orElse(null);
	        if (assignmentID == null) // Bridge
	            assignmentID = request.queryString("url").orElse(null);
	        Map<String, String[]> postParams = request.body().asFormUrlEncoded();        
	        String result = assignmentService.launch(url, assignmentID, postParams);
	        if (services.LTIAssignment.isInstructor(postParams)) {     
	            String toolConsumerID = Util.getParam(postParams, "tool_consumer_instance_guid");
	            String userID = Util.getParam(postParams, "user_id");
	            String userLMSID = toolConsumerID + "/" + userID;
	        	
	            return ok(result)
	            	.as("text/html")             
	                .withNewSession()
	                .addingToSession(request, "user", userLMSID);
	        } else { // Student
	            return ok(result).as("text/html");
	        }
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}
        catch (Exception ex) {
            logger.log(Logger.Level.ERROR, Util.getStackTrace(ex));
            return internalServerError(Util.getStackTrace(ex));
        }
    }
    
    @Security.Authenticated(Secured.class) // Instructor
    public Result allSubmissions(Http.Request request) {
    	try {
	        String resourceID = request.queryString("resourceID").orElse(null);
	        ObjectNode result = assignmentService.allSubmissions(resourceID);
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
    
    public Result saveWork(Http.Request request) {
        try {
            ObjectNode requestNode = (ObjectNode) request.body().asJson();
            ObjectNode result = assignmentService.saveWork(requestNode);
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
    
    public Result sendScore(Http.Request request) {
    	try {
    		ObjectNode requestNode = (ObjectNode) request.body().asJson();
            ObjectNode result = assignmentService.sendScore(requestNode);
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