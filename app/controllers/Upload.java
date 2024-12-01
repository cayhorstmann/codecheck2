package controllers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;

import javax.inject.Inject;
import javax.script.ScriptException;

import play.libs.Files.TemporaryFile;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import services.ServiceException;

public class Upload extends Controller {
    @Inject private services.Upload uploadService;

    /**
     * Upload a new problem as a form
     * @throws ScriptException 
     * @throws InterruptedException 
     * @throws IOException 
     * @throws NoSuchMethodException 
     */
    public Result uploadFiles(Http.Request request) throws NoSuchMethodException, IOException, InterruptedException, ScriptException {
        return editedFiles(request, null, null);
    }

    /**
     * Upload fixes to an existing problem as a form (or new if problem == null)
     * @throws ScriptException 
     * @throws InterruptedException 
     * @throws IOException 
     * @throws NoSuchMethodException 
     */
    public Result editedFiles(Http.Request request, String problem, String editKey) throws NoSuchMethodException, IOException, InterruptedException, ScriptException {
        try {
            int n = 1;
            Map<String, String[]> params = request.body().asFormUrlEncoded();
            Map<Path, byte[]> problemFiles = new TreeMap<>();
            while (params.containsKey("filename" + n)) {
                String filename = params.get("filename" + n)[0];
                if (filename.trim().length() > 0) {
                    String contents = params.get("contents" + n)[0].replaceAll("\r\n", "\n");                    
                    problemFiles.put(Path.of(filename), contents.getBytes(StandardCharsets.UTF_8));
                }
                n++;
            }
            String response = uploadService.checkAndSaveProblem(controllers.Util.prefix(request), problem, problemFiles, editKey);
            return ok(response).as("text/html");
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}
    }

    /**
     * Upload a new problem as a zip file
     * @throws ScriptException 
     * @throws InterruptedException 
     * @throws IOException 
     * @throws NoSuchMethodException 
     */
    public Result uploadProblem(Http.Request request) throws NoSuchMethodException, IOException, InterruptedException, ScriptException {
        return editedProblem(request, null, null);
    }

    /**
     * Upload of zip file when editing problem with edit key
     * @throws ScriptException 
     * @throws InterruptedException 
     * @throws IOException 
     * @throws NoSuchMethodException 
     */
    public Result editedProblem(Http.Request request, String problem, String editKey) throws NoSuchMethodException, IOException, InterruptedException, ScriptException {
        try {
            play.mvc.Http.MultipartFormData<TemporaryFile> body = request.body().asMultipartFormData();
            Http.MultipartFormData.FilePart<TemporaryFile> tempZipPart = body.getFile("file");
            TemporaryFile tempZipFile = tempZipPart.getRef();
            Path savedPath = tempZipFile.path();
            byte[] problemZip = Files.readAllBytes(savedPath);            
            String response = uploadService.checkAndSaveProblem(controllers.Util.prefix(request), problem, problemZip, editKey);
            return ok(response).as("text/html");           
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}
    }

    /**
     * Get a form for editing the files of an existing problem
     * @throws IOException 
     */
    public Result editProblem(Http.Request request, String problem, String editKey) throws IOException {
        if (problem.equals(""))
            return badRequest("No problem id");
        try {
            String response = uploadService.editProblem(controllers.Util.prefix(request), problem, editKey);
            return ok(response).as("text/html");
        }
    	catch (ServiceException ex) {
    		return badRequest(ex.getMessage());
    	}
    }
}
