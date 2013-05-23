import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;

import javax.servlet.ServletContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;

// TODO: Pretty URLs

@javax.ws.rs.Path("/check")
public class Check {
    @Context
    ServletContext context;
    static Random random = new Random();

    @POST
    @Consumes("application/x-www-form-urlencoded")
    @Produces("text/html")
    public Response check(MultivaluedMap<String, String> formParams)
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
        Util.runLabrat(context, repo, problem, level, tempDir.toAbsolutePath().toString());
        String reportFileName = tempDir.getFileName().toString()+ ".html";
        Path reportDir = Util.getDir(context, "reports");
        Path reportFile = reportDir.resolve(reportFileName);
        Files.copy(tempDir.resolve("report.html"), reportFile);
        // TODO: Remove temp dir?
        return Response.seeOther(URI.create("fetch?file=" + reportFileName)).build();
    }
}
