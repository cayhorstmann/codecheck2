import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@javax.ws.rs.Path("/fetch")
public class Fetch {
    @Context ServletContext context;

    // TODO: If it's a JAR file, it's also in reportDir
    
    @GET 
    @javax.ws.rs.Path("/{dir}/{file}")
    public Response files1(@PathParam("dir") String dir, @PathParam("file") String file) throws IOException {
        if (file.endsWith(".html")) { 
            Path reportDir = Util.getDir(context, "reports");
            File data = reportDir.resolve(dir).resolve(file).toFile();
            return Response.ok(data)
                   .type(MediaType.TEXT_HTML_TYPE)
                   .build();
        }
        // TODO: This is a hack because I couldn't get the JAR file name to move it 
        else { // JAR
            Path submissionDir = Util.getDir(context, "submissions");
            File data = submissionDir.resolve(dir).resolve(file).toFile();
        	return Response.ok(data)
                    .type(MediaType.APPLICATION_OCTET_STREAM)
                    .build();
        }
    }
    
    // TODO: Legacy, but extract grading support
    @GET
    @javax.ws.rs.Path("/")
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
