import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.Random;

import javax.servlet.ServletContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.CookieParam;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.NewCookie;
import javax.ws.rs.core.Response;

@javax.ws.rs.Path("/check")
public class Check {
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
        for (String key : formParams.keySet()) {
            String value = formParams.getFirst(key);
            if (key.equals("repo"))
                repo = value;
            else if (key.equals("problem"))
                problem = value;
            else if (key.equals("level"))
                level = value;
            else
                Util.write(tempDir, key, value);
        }
        Path tempDirName = tempDir.getFileName();
        if (ccu == null) ccu = tempDirName.toString();
        // TODO: Pass on to report
        Util.runLabrat(context, repo, problem, level, tempDir.toAbsolutePath().toString(), "User=" + ccu);
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
