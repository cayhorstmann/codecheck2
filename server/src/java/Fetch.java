import java.io.IOException;
import java.nio.file.Path;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@javax.ws.rs.Path("/fetch")
public class Fetch {
    @Context ServletContext context;

    @GET
    public Response files(@QueryParam("file") String file, @QueryParam("progress") String progress) throws IOException {
        if (file.endsWith(".html")) {
            Path reportDir = Util.getDir(context, "reports");
            Path path = reportDir.resolve(file);
            return Response.ok(path.toFile())
                   .type(MediaType.TEXT_HTML_TYPE)
                   .build();
        } else if (progress != null) { // it's a zip file for grading
            Path submissionDir = Util.getDir(context, "submissions");
            Path path = submissionDir.resolve(file);
            if (java.nio.file.Files.exists(path)) {
                return Response.ok(path.toFile())
                       .type(MediaType.APPLICATION_OCTET_STREAM_TYPE)
                       .header("Content-Disposition", "attachment; filename=" + path.getFileName().toString())
                       .build();
            } else {
                Path progressPath = submissionDir.resolve(progress);
                String message = java.nio.file.Files.exists(progressPath) ?
                                 new String(java.nio.file.Files.readAllBytes(progressPath), "UTF-8")
                                 : "Please wait...";
                return Response.ok("<pre>" + message + "</pre>").header("refresh", "5").build();
            }
        } else return Response.noContent().build();
    }
}
