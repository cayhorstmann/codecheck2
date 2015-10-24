import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;

import javax.servlet.ServletContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

@javax.ws.rs.Path("/grade")
public class Grade {
	public static final int TIMEOUT = 2 * 60 * 1000; // 2 minutes; 
    @Context ServletContext context;

    @POST
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    public Response uploadProblem(@FormDataParam("submissions") InputStream in,
                                  @FormDataParam("submissions") FormDataContentDisposition fileInfo,
                                  @FormDataParam("lmstype") String lmstype,
                                  @FormDataParam("repo") String repo,
                                  @FormDataParam("problems") String problems,
                                  @FormDataParam("level") String level,
                                  @Context UriInfo uriInfo
                                 )
    throws IOException {
        try {
            String submissionsDir = context.getInitParameter("com.horstmann.codecheck.submissions");
            File submissionsFile = new File(submissionsDir);
            if (!submissionsFile.exists()) submissionsFile.mkdir();

            Path zip = Files.createTempDirectory(Paths.get(submissionsDir), "grade").resolve("submissions.zip");
            Files.copy(in, zip);
            String submissions = zip.toAbsolutePath().toString();

            String repoPathName = context.getInitParameter("com.horstmann.codecheck.repo." + repo);

            if (repoPathName == null) {
                return Response.status(Response.Status.NOT_FOUND).entity("No repo " + repo).build();
            }

            if (lmstype == null) {
                return Response.status(Response.Status.NOT_FOUND).entity("Select a LMS format").build();
            }

            if (level == null) {
                return Response.status(Response.Status.NOT_FOUND).entity("Select draft or final").build();
            }

            Path repoPath = Paths.get(repoPathName);

            for (String problem : problems.split("[\\s,]+")) {
                if (!Files.exists(repoPath.resolve(problem))) {
                    return Response.status(Response.Status.NOT_FOUND).entity("No problem " + problem + " in repo " + repo).build();
                }
            }

            String command = context.getInitParameter("com.horstmann.codecheck.gradecommand");
            Path reportDir = Util.getDir(context, "reports");
            String reportURL = uriInfo.getBaseUri() + "fetch?file=";
            final Path archiveDir = Files.createTempDirectory(Paths.get(submissionsDir), "scores");
            final String script = MessageFormat.format(command,
                                  archiveDir.toAbsolutePath(),
                                  submissions, lmstype,
                                  reportDir.toString(), reportURL,
                                  level, repoPathName,
                                  problems.replaceAll("[\\s,]+", " "));
            new Thread() {
                public void run() {
                    Util.runProcess(script, TIMEOUT);
                }
            } .start();
            String fetchfile = Paths.get(submissionsDir).relativize(archiveDir.resolve("scores.zip")).toString();
            String progress = Paths.get(submissionsDir).relativize(archiveDir.resolve("codecheck.log")).toString();
            return Response.seeOther(URI.create("fetch?file=" + fetchfile + "&progress=" + progress)).build();
        } catch (Exception ex) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            ex.printStackTrace(pw);

            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).type(MediaType.TEXT_PLAIN).entity(sw.toString()).build();
        }
    }



}
