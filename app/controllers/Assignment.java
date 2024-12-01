package controllers;

import java.io.IOException;
import java.lang.System.Logger;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.util.Optional;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import services.ServiceException;

public class Assignment extends Controller {
    @Inject private services.Assignment assignmentService;
    // TODO Do we need to log, or does internalServerError do it already?
    private static Logger logger = System.getLogger("com.horstmann.codecheck");     
    
    public Result edit(Http.Request request, String assignmentID, String editKey) throws IOException {
    	try {
    		String result = assignmentService.edit(assignmentID, editKey);
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
    	try {
	        String prefix = controllers.Util.prefix(request);
	        
	        if (isStudent) {
	            boolean editKeySaved = true;
	            if (ccid == null) {         
	                Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
	                if (ccidCookie.isPresent()) {
	                    ccid = ccidCookie.get().value();
	                    Optional<Http.Cookie> editKeyCookie = request.getCookie("cckey");
	                    if (editKeyCookie.isPresent()) 
	                        editKey = editKeyCookie.get().value();
	                    else { // This shouldn't happen, but if it does, clear ID
	                        ccid = Util.createPronouncableUID();
	                        editKey = Util.createPrivateUID();
	                        editKeySaved = false;                       
	                    }
	                } else { // First time on this browser
	                    ccid = Util.createPronouncableUID();
	                    editKey = Util.createPrivateUID();
	                    editKeySaved = false;
	                }
	            } else if (editKey == null) { // Clear ID request
	                ccid = Util.createPronouncableUID();
	                editKey = Util.createPrivateUID();
	                editKeySaved = false;               
	            }
	            Http.Cookie newCookie1 = controllers.Util.buildCookie("ccid", ccid);
	            Http.Cookie newCookie2 = controllers.Util.buildCookie("cckey", editKey);
	            String result = assignmentService.work(prefix, assignmentID, ccid, editKey, true /* studet */, editKeySaved);
	            return ok(result).withCookies(newCookie1, newCookie2).as("text/html");
	        } else { // Instructor
	            String result = assignmentService.work(prefix, assignmentID, ccid, editKey, false /* student */, false /* editKeySaved */);
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
    
    public Result viewSubmissions(Http.Request request, String assignmentID, String editKey)
        throws IOException {
    	try {
    		String result = assignmentService.viewSubmissions(assignmentID, editKey);
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

    /*
     * Save existing: request.assignmentID, request.editKey exist
     * New or cloned: Neither request.assignmentID nor request.editKey exist
     */
    public Result saveAssignment(Http.Request request) throws IOException {     
    	try {
            String prefix = controllers.Util.prefix(request);
            ObjectNode params = (ObjectNode) request.body().asJson();
    		ObjectNode result = assignmentService.saveAssignment(prefix, params);
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
    
    public Result saveWork(Http.Request request) throws IOException, NoSuchAlgorithmException {
    	try {
            ObjectNode params = (ObjectNode) request.body().asJson();
    		ObjectNode result = assignmentService.saveWork(params);
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
    
    public Result saveComment(Http.Request request) throws IOException {
    	try {
            ObjectNode params = (ObjectNode) request.body().asJson();
    		ObjectNode result = assignmentService.saveComment(params);
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
