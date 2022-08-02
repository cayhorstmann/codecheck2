package controllers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Map;
import java.util.TreeMap;

import javax.inject.Inject;
import javax.script.ScriptException;

import com.horstmann.codecheck.Plan;
import com.horstmann.codecheck.Problem;
import com.horstmann.codecheck.Report;
import com.horstmann.codecheck.Util;
import com.typesafe.config.Config;

import models.CodeCheck;
import models.S3Connection;
import play.libs.Files.TemporaryFile;
import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;

public class Upload extends Controller {
    final String repo = "ext";
    @Inject private S3Connection s3conn;
    @Inject private Config config;
    @Inject private CodeCheck codeCheck;

    // Entry point
    public Result uploadFiles(Http.Request request) {
        return uploadFiles(request, com.horstmann.codecheck.Util.createPublicUID(), Util.createPrivateUID());
    }

    public Result editedFiles(Http.Request request, String problem, String editKey) {
        try {
            if (checkEditKey(problem, editKey))
                return uploadFiles(request, problem, editKey);
            else
                return badRequest("Wrong edit key " + editKey + " in problem " + problem);
        } catch (IOException ex) {
            return badRequest("Problem not found: " + problem);
        } catch (Exception ex) {
            return internalServerError(Util.getStackTrace(ex));
        }
    }

    public Result uploadFiles(Http.Request request, String problem, String editKey) {
        try {
            if (problem == null)
                badRequest("No problem id");
            int n = 1;
            // Get POST parameters
            Map<String, String[]> params = request.body().asFormUrlEncoded();
            // Sorted map
            Map<Path, byte[]> problemFiles = new TreeMap<>();

            // While filename1, filename2, filename3, etc keys exist,
            while (params.containsKey("filename" + n)) {
                String filename = params.get("filename" + n)[0];
                // And if filename is not an empty string
                if (filename.trim().length() > 0) {
                	// Get the contents of the corresponding filename
                    String contents = params.get("contents" + n)[0];
                    // Maps the file name to its content bytes
                    problemFiles.put(Path.of(filename), contents.getBytes(StandardCharsets.UTF_8));
                }
                n++;
            }	
            // problemFiles at this point maps the POSTed file names to their corresponding POSTed content strings
            
            // looks like some sort of cryptographic edit key
            problemFiles.put(Path.of("edit.key"), editKey.getBytes(StandardCharsets.UTF_8));
            // Save on S3 (OLD)
            saveProblem(problem, problemFiles, null);
            
            // checkProblem
            String response = checkProblem(request, problem, problemFiles);
            return ok(response).as("text/html").addingToSession(request, "pid", problem);
        } catch (Exception ex) {
            return internalServerError(Util.getStackTrace(ex));
        }
    }
    
    public void saveProblem(String problem, Map<Path, byte[]> problemFiles, Plan plan) throws IOException {
    	// Put plan keys/values into problemFiles
    	
    	// DEBUG ONLY
    	boolean isOnS3 = false;
//        boolean isOnS3 = s3conn.isOnS3(repo);
        if (isOnS3) {
            byte[] problemZip = Util.zip(problemFiles);
            // Zip the plan too
            s3conn.putToS3(problemZip, repo, problem);
        } else {
            Path extDir = java.nio.file.Path.of(config.getString("com.horstmann.codecheck.repo." + repo));
            Path problemDir = extDir.resolve(problem);
            com.horstmann.codecheck.Util.deleteDirectory(problemDir); // Delete any prior contents so that it is replaced by new zip file
            
            Files.createDirectories(problemDir);
            // Create directory for plan using File.createDirectories
            
            // Write every problem file's content to their file path
            for (Map.Entry<Path, byte[]> entry : problemFiles.entrySet()) {
                Path p = problemDir.resolve(entry.getKey());
                Files.write(p, entry.getValue());
            }
            
            for (Map.Entry<Path, byte[]> entry : problemFiles.entrySet()) {
                Path p = problemDir.resolve(entry.getKey());
                if (p.toString().startsWith("solution"))
                	Files.write(p, entry.getValue());
            }
        }       
    }

    public Result uploadProblem(Http.Request request) {
        return uploadProblem(request, com.horstmann.codecheck.Util.createPublicUID(), Util.createPrivateUID());
    }

    private boolean checkEditKey(String problem, String editKey) throws IOException {
        Map<Path, byte[]> problemFiles = codeCheck.loadProblem(repo, problem);
        Path editKeyPath = Path.of("edit.key");
        if (problemFiles.containsKey(editKeyPath)) {
            String correctEditKey = new String(problemFiles.get(editKeyPath), StandardCharsets.UTF_8);
            return editKey.equals(correctEditKey.trim());
        } else return false;
    }

    /**
     * Upload of zip file when editing problem with edit key
     */
    public Result editedProblem(Http.Request request, String problem, String editKey) {
        try {
            if (checkEditKey(problem, editKey))
                return uploadProblem(request, problem, editKey);
            else
                return badRequest("Wrong edit key " + editKey + " of problem " + problem);
        } catch (IOException ex) {
            return badRequest("Problem not found: " + problem);
        } catch (Exception ex) {
            return internalServerError(Util.getStackTrace(ex));
        }
    }

    public Result uploadProblem(Http.Request request, String problem, String editKey) {
        try {
            play.mvc.Http.MultipartFormData<TemporaryFile> body = request.body().asMultipartFormData();
            if (problem == null)
                badRequest("No problem id");
            Http.MultipartFormData.FilePart<TemporaryFile> tempZipPart = body.getFile("file");
            TemporaryFile tempZipFile = tempZipPart.getRef();
            Path savedPath = tempZipFile.path();
            byte[] contents = Files.readAllBytes(savedPath);
            Map<Path, byte[]> problemFiles = Util.unzip(contents);
            problemFiles = fixZip(problemFiles);
            Path editKeyPath = Path.of("edit.key");
            if (!problemFiles.containsKey(editKeyPath)) 
                problemFiles.put(editKeyPath, editKey.getBytes(StandardCharsets.UTF_8));
            saveProblem(problem, problemFiles, null);
            String response = checkProblem(request, problem, problemFiles);
            return ok(response).as("text/html").addingToSession(request, "pid", problem);           
        } catch (Exception ex) {
            return internalServerError(Util.getStackTrace(ex));
        }
    }

    private String checkProblem(Http.Request request, String problem, Map<Path, byte[]> problemFiles)
            throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
    	// Problem is a string (presumably the uploaded file's name)
    	// problemFiles at this point maps the POSTed file names to their corresponding POSTed content strings

    	// copy problemFiles
        Map<Path, byte[]> newProblemFiles = new TreeMap<>(problemFiles);
        StringBuilder response = new StringBuilder();
        String type;
        String report = null;
        
        if (problemFiles.containsKey(Path.of("tracer.js"))) {
        	// If "tracer.js" was a POSTed filename, type is "tracer"
            type = "tracer";
        } else {
        	// If not, proceed normally
            type = "files";
            String studentId = com.horstmann.codecheck.Util.createPronouncableUID();
            // not relevant to my task
            codeCheck.replaceParametersInDirectory(studentId, newProblemFiles);
            // generate the report from the problem and map of the POSTed file names to their corresponding POSTed content strings
            report = check(problem, newProblemFiles, studentId);
        }
        response.append(
                "<html><head><title></title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>");
        response.append("<body style=\"font-family: sans\">");
        String prefix = (request.secure() ? "https://" : "http://") + request.host() + "/";
        String problemUrl = prefix + type + "/" + problem;
        response.append("Public URL (for your students): ");
        response.append("<a href=\"" + problemUrl + "\" target=\"_blank\">" + problemUrl + "</a>");
        Path editKeyPath = Path.of("edit.key");
        if (problemFiles.containsKey(editKeyPath)) {
            String editKey = new String(problemFiles.get(editKeyPath), StandardCharsets.UTF_8);         
            String editURL = prefix + "private/problem/" + problem + "/" + editKey;
            response.append("<br/>Edit URL (for you only): ");
            response.append("<a href=\"" + editURL + "\" target=\"_blank\">" + editURL + "</a>");
        }
        if (report != null) {
            String run = Base64.getEncoder().encodeToString(report.getBytes(StandardCharsets.UTF_8));
            response.append(
                    "<br/><iframe height=\"400\" style=\"width: 90%; margin: 2em;\" src=\"data:text/html;base64," + run
                            + "\"></iframe>");
        }
        response.append("</li>\n");
        response.append("</ul><p></body></html>\n");
        return response.toString();
    }

    public Result editKeySubmit(Http.Request request, String problem, String editKey) {
        if (problem.equals(""))
            return badRequest("No problem id");
        try {
            Map<Path, byte[]> problemFiles = codeCheck.loadProblem(repo, problem);
            Path editKeyPath = Path.of("edit.key");
            if (!problemFiles.containsKey(editKeyPath)) 
                return badRequest("Wrong edit key " + editKey + " for problem " + problem);
            String correctEditKey = new String(problemFiles.get(editKeyPath), StandardCharsets.UTF_8);
            if (!editKey.equals(correctEditKey.trim())) {
                return badRequest("Wrong edit key " + editKey + " for problem " + problem);
            }
            Map<String, String> filesAndContents = new TreeMap<>();
            for (Map.Entry<Path, byte[]> entries : problemFiles.entrySet()) {
                Path p = entries.getKey();
                if (p.getNameCount() == 1)
                    filesAndContents.put(p.toString(), new String(entries.getValue(), StandardCharsets.UTF_8));
                else
                    return badRequest("Cannot edit problem with directories");
            }
            filesAndContents.remove("edit.key");
            return ok(views.html.edit.render(problem, filesAndContents, correctEditKey));
        } catch (IOException ex) {
            return badRequest("Problem not found: " + problem);
        } catch (Exception ex) {
            return internalServerError(Util.getStackTrace(ex));
        }
    }

    private static Path longestCommonPrefix(Path p, Path q) {
        if (p == null || q == null) return null;
        int i = 0;
        boolean matching = true;
        while (matching && i < Math.min(p.getNameCount(), q.getNameCount())) {
            if (p.getName(i).equals(q.getName(i))) i++;
            else matching = false;
        }
        return i == 0 ? null : p.subpath(0, i);
    }
    
    private static Map<Path, byte[]> fixZip(Map<Path, byte[]> problemFiles) throws IOException {
        Path r = null;
        boolean first = true;
        for (Path p : problemFiles.keySet()) {
            if (first) { r = p; first = false; }
            else r = longestCommonPrefix(r, p);
        }
        if (r == null) return problemFiles;
        Map<Path, byte[]> fixedProblemFiles = new TreeMap<>();
        for (Map.Entry<Path, byte[]> entry : problemFiles.entrySet()) {
            fixedProblemFiles.put(r.relativize(entry.getKey()), entry.getValue());
        }
        return fixedProblemFiles;
    }

    private String check(String problem, Map<Path, byte[]> problemFiles, String studentId)
            throws IOException, InterruptedException, NoSuchMethodException, ScriptException {
        Problem p = new Problem(problemFiles);
        Map<Path, String> submissionFiles = new TreeMap<>();
        // Rebuild problemFiles in submissionFiles but with the values in string instead of byte form
        for (Map.Entry<Path, byte[]> entry : p.getSolutionFiles().entrySet()) 
            submissionFiles.put(entry.getKey(), new String(entry.getValue(), StandardCharsets.UTF_8)); 
        // Not sure what input files are
        for (Map.Entry<Path, byte[]> entry : p.getInputFiles().entrySet()) 
            submissionFiles.put(entry.getKey(), new String(entry.getValue(), StandardCharsets.UTF_8));   
        
        
        // action items:
        // finish this and demonstrate the solution files actually appear
        // goal: get solution files to be used in the grading
        
        
        // for git: push entire branch
        
        // goal:
        // called from upload, run solution once and save
        // call from somewhere else, don't change anything
        
        // in the debugger all the files will be under a solution directory
        
        // has a lot of solution-related files we want to save
        // need to work out details
        Plan plan = codeCheck.run("html", repo, problem, studentId, submissionFiles, true);
        // get plan files
        // only care about ones that start with submission
        // make it that they are also saved (i.e. iterating over the map and looking through all of its contents)
        
        
        // See the files in ext 
        Report report = plan.getReport();
        return report.getText(); 
    }
}
