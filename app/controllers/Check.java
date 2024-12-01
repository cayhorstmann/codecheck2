package controllers;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import play.libs.Files.TemporaryFile;
import play.libs.concurrent.HttpExecution;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Check extends Controller {
    private CodecheckExecutionContext ccec; 
    @Inject private services.Check checkService;
    
    public CompletableFuture<Result> run(Http.Request request) throws IOException, InterruptedException  {
        return CompletableFuture.supplyAsync(() -> {
            try {
                Map<String, String[]> params;
	            Map<Path, String> submissionFiles = new TreeMap<>();
	            String contentType = request.contentType().orElse("");
		        if ("application/x-www-form-urlencoded".equals(contentType)) {
                    params = request.body().asFormUrlEncoded();
                    for (String key : params.keySet()) {
                        String value = params.get(key)[0];
                        submissionFiles.put(Paths.get(key), value);
                    }
                    return ok(checkService.run(submissionFiles)).as("text/plain");
		        } else if ("multipart/form-data".equals(contentType)) {
		            play.mvc.Http.MultipartFormData<TemporaryFile> body = request.body().asMultipartFormData();
		            for (var f : body.getFiles()) {
		            	String name = f.getFilename();
		                TemporaryFile tempZipFile = f.getRef();
		                Path savedPath = tempZipFile.path();
		                String contents = Util.read(savedPath);
                        submissionFiles.put(Paths.get(name), contents);
	                }
	                return ok(checkService.run(submissionFiles)).as("text/plain");
		        } else if ("application/json".equals(contentType)) {
			        return ok(checkService.runJSON(request.body().asJson())).as("application/json");
		        }
		        else return internalServerError("Bad content type");
            } catch (Exception ex) {
            	return internalServerError(Util.getStackTrace(ex));
            }	
        }, HttpExecution.fromThread((Executor) ccec) /* ec.current() */); 
    }
    
    public CompletableFuture<Result> checkNJS(Http.Request request) throws IOException, InterruptedException  {
        return CompletableFuture.supplyAsync(() -> {
            try {
                JsonNode json = request.body().asJson();
                Optional<Http.Cookie> ccidCookie = request.getCookie("ccid");
                String ccid = ccidCookie.map(Http.Cookie::value).orElse(Util.createPronouncableUID());
                ObjectNode result = checkService.checkNJS(json, ccid);
                Http.Cookie newCookie = controllers.Util.buildCookie("ccid", ccid);
                return ok(result).withCookies(newCookie).as("application/json");
            } catch (Exception ex) {
                return internalServerError(Util.getStackTrace(ex));
            }
        }, HttpExecution.fromThread((Executor) ccec) /* ec.current() */);           
    }    
    
    public CompletableFuture<Result> setupReport(Http.Request request, String repo, String problem) throws IOException, InterruptedException {
        return CompletableFuture.supplyAsync(() -> {
            try {
            	String ccid = Util.createPronouncableUID();
            	String report = checkService.setupReport(repo, problem, ccid);
                return ok(report).as("application/json");
            }
            catch (Exception ex) {
                return internalServerError(Util.getStackTrace(ex));
            }
        }, HttpExecution.fromThread((Executor) ccec) /* ec.current() */);                        
    }    
}
