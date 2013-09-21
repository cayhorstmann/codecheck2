import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

@javax.ws.rs.Path("/uploadProblem")
public class UploadProblem {
    @Context ServletContext context;
    @Context private HttpServletRequest request;

    private Path dir;
    private String reason;
    private String repo = "ext";
    private Map<String, String> runs = new LinkedHashMap<>();

    @POST
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Produces(MediaType.TEXT_HTML)
    public Response uploadProblem(@FormDataParam("file") InputStream in, @FormDataParam("file") FormDataContentDisposition fileInfo)
    throws IOException {
        try {
            String repoPath = context
                              .getInitParameter("com.horstmann.codecheck.repo." + repo);
            if (repoPath == null) {
                reason = "No such repo";
                return Response.status(Response.Status.NOT_ACCEPTABLE).entity(reason + "\n").build();
            }
            Path repoDir = FileSystems.getDefault().getPath(repoPath);
            if (!Files.exists(repoDir)) Files.createDirectory(repoDir);
            dir = Util.createTempDirectory(repoDir);
            Util.unzip(in, dir);
            if (check()) {
                boolean grade = runs.keySet().contains("grade");
                boolean multipleLevels = runs.keySet().size() > (grade ? 2 : 1);
                String problem = dir.getFileName().toString();
                String requestURL = request.getRequestURL().toString();
                String url = requestURL.substring(0, requestURL.lastIndexOf("/")) + "/files/" + problem;
                StringBuilder response = new StringBuilder();
                response.append("<html><body style=\"font-family: sans\"><ul style=\"list-style: square\">");
                for (String k : runs.keySet()) {
                    response.append("<li>");
                    String reportUrl = requestURL.substring(0, requestURL.lastIndexOf("/")) + "/fetch/" + runs.get(k);
                    if (k.equals("grade")) {
                        response.append("<a href=\"");
                        response.append(reportUrl);
                        response.append("\" target=\"_blank\">Grader report</a>");
                    } else {
                        String problemUrl = url;
                        if (multipleLevels) {
                            response.append("Level " + k + " ");
                            problemUrl += "/" + k;
                        }
                        response.append("URL: <code>");
                        response.append(problemUrl);
                        response.append("</code> | <a href=\"");
                        response.append(problemUrl);
                        response.append("\" target=\"_blank\">Preview</a>");
                        response.append(" | <a href=\"");
                        response.append(reportUrl);
                        response.append("\" target=\"_blank\">Report</a>");
                    }
                    response.append("</li>\n");
                }
                response.append("</ul></body></html>\n");
                return Response.status(Response.Status.OK).entity(response.toString()).build();
            } else
                return Response.status(Response.Status.NOT_ACCEPTABLE).entity(reason + "\n").build();
        } catch (Exception ex) {
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(ex.getClass() + " " + ex.getMessage()).build();
        }
    }

    private boolean check() throws IOException {
        // TODO: Only good for old-style
        if (!Files.exists(dir.resolve("student"))) {
            reason = "No student directory";
            return false;
        }
        if (!Files.exists(dir.resolve("solution"))) {
            reason = "No solution directory";
            return false;
        }

        int maxLevel = 1;
        for (int i = 9; i >= 2 && maxLevel == 1; i--) // Find highest level
            if (Files.exists(dir.resolve("student" + i)) || Files.exists(dir.resolve("solution" + i))) maxLevel = i;

        boolean grade = Files.exists(dir.resolve("grader"));
        Path submissionDir = Util.getDir(context, "submissions");
        List<String> subdirs = new ArrayList<>();
        for (int i = 1; i <= (grade ? maxLevel + 1 : maxLevel); i++) {
            Path tempDir = Util.createTempDirectory(submissionDir);
            // Copy solution files up to the current level
            if (i <= maxLevel) subdirs.add(i == 1 ? "solution" : "solution" + i);
            for (Path p : Util.getDescendantFiles(dir, subdirs))
                Files.copy(dir.resolve(p), tempDir.resolve(Util.tail(p)));

            String problem = dir.getFileName().toString();
            String levelString = grade && i == maxLevel + 1 ? "grade" : "" + i;
            Util.runLabrat(context, repo, problem, levelString, tempDir.toAbsolutePath().toString());
            // Path reportDir = Util.getDir(context, "reports").resolve(tempDir.getFileName());
            // Files.createDirectory(reportDir);
            // Files.copy(tempDir.resolve("report.html"), reportDir.resolve("report.html"));
            // TODO: Remove temp dir?
            runs.put(levelString, tempDir.getFileName().toString() + "/report.html");
        }
        return true;
    }


}
