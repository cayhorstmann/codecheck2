package controllers;

import java.io.IOException;
import java.util.Optional;

import javax.inject.Inject;
import javax.script.ScriptException;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import services.ServiceException;

public class Files extends Controller {
    @Inject private services.Files filesService;
    
    public Result filesHTML2(Http.Request request, String repo, String problemName)
            throws IOException, NoSuchMethodException, ScriptException {
    	try {
    		Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
    		String ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
	        String result = filesService.filesHTML2(controllers.Util.prefix(request), repo, problemName, ccid);
	        Http.Cookie newCookie = controllers.Util.buildCookie("ccid", ccid);
	        return ok(result).withCookies(newCookie).as("text/html");
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}
    }

    public Result tracer(Http.Request request, String repo, String problemName)
            throws IOException {
    	try {
    		Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
    		String ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
	        String result = filesService.tracer(repo, problemName, ccid);
	        Http.Cookie newCookie = controllers.Util.buildCookie("ccid", ccid);
	        return ok(result).withCookies(newCookie).as("text/html");
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}	        
    }
        
    // TODO: Caution--this won't do the right thing with param.js randomness when
    // used to prebuild UI like in ebook, Udacity
    public Result fileData(Http.Request request, String repo, String problemName)
            throws IOException, NoSuchMethodException, ScriptException {
    	try {
    		Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
    		String ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
	        ObjectNode result = filesService.fileData(repo, problemName, ccid);
	        Http.Cookie newCookie = controllers.Util.buildCookie("ccid", ccid);
	        return ok(result).withCookies(newCookie);
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}        
    }
}
