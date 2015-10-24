import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Random;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.CookieParam;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.NewCookie;
import javax.ws.rs.core.Response;

@javax.ws.rs.Path("/checkGit")
public class CheckGit {
    @Context
    ServletContext context;
    static Random random = new Random();

    @POST
    @Consumes("application/x-www-form-urlencoded")
    @Produces("text/html")
    public Response check(MultivaluedMap<String, String> formParams, @CookieParam("ccu") String ccu)
    throws IOException {
        Path submissionDir = Util.getDir(context, "submissions");
        Path tempDir = Util.createTempDirectory(submissionDir);
        String repo = "ext";
        String problem = "";
        String level = "check";
        String gitURL = "";
        for (String key : formParams.keySet()) {
            String value = formParams.getFirst(key);
            if (key.equals("repo"))
                repo = value;
            else if (key.equals("problem"))
                problem = value;
            else if (key.equals("level"))
                level = value;
            else if (key.equals("git"))
                gitURL = value;
        }
        final String BAD_START = "git@github.com:";
        final String GOOD_START = "git@github.com/";
        gitURL = gitURL.trim();
        if (gitURL.startsWith(BAD_START))
        	gitURL = GOOD_START + gitURL.substring(BAD_START.length());
        if (!gitURL.startsWith(GOOD_START))
        	return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
        			.entity("Git SSH URL must have the form git@github.com/xxx/xxx.git	").build();
        // Fetch git repo into tempDir
        StringBuilder gitOutput = new StringBuilder();
        final int GIT_MILLIS = 10000;
        int gitResult = Util.runProcess(Arrays.asList("git", "clone", "ssh://" + gitURL, "."), tempDir, gitOutput, GIT_MILLIS);
        
        // if result != 0 report error
        if (gitResult != 0)
        	return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
        			.entity(gitOutput + "\ntempDir=" + tempDir	).build();
        
        // If student uses src directory, move all files to base
        
        Set<Path> submittedFiles = Util.getDescendantFiles(tempDir);
        for (Path p : submittedFiles) 
        	if (!p.getName(0).toString().equals(".git") && p.getNameCount() > 1)
        		java.nio.file.Files.move(tempDir.resolve(p), tempDir.resolve(p.getFileName()));
               
        Path tempDirName = tempDir.getFileName();
        if (ccu == null) ccu = tempDirName.toString();
        Util.runLabrat(context, repo, problem, level, tempDir.toAbsolutePath(), "User=" + ccu, "git=" + gitURL);
        // Path reportBaseDir = Util.getDir(context, "reports");
        // Path reportDir = reportBaseDir.resolve(tempDirName);
        // Files.createDirectory(reportDir);
        // Files.copy(tempDir.resolve("report.html"), reportDir.resolve("report.html"));
        // TODO: Find the JAR file name and move it
        // Files.copy(tempDir.resolve("report.jar"), reportDir.resolve("report.jar"));
        // TODO: Remove temp dir?
        int age = 180 * 24 * 60 * 60;
        NewCookie cookie = new NewCookie(new NewCookie("ccu", ccu), "", age, false);
        return Response.seeOther(URI.create("fetch/" + tempDirName + "/report.html")).cookie(cookie).build();
    }
}
