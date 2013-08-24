import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;

@javax.ws.rs.Path("/files")
public class Files {
    private static final Pattern IMG_PATTERN = Pattern
            .compile("[<]\\s*[iI][mM][gG]\\s*[sS][rR][cC]\\s*[=]\\s*['\"]([^'\"]*)['\"][^>]*[>]");

    @Context
    ServletContext context;
    @Context private HttpServletRequest request;

    private static String start = "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" /></head><body style=\"font-family: sans;\">";
    private static String before = "<form method=\"post\" action=\"{0}\" {1}>";

    private static String fileAreaBefore = "<p>{0}</p><textarea name=\"{0}\" rows=\"{1}\" cols=\"66\">";
    private static String fileAreaAfter = "</textarea>";
    private static String fileUpload = "<p>{0}: <input type=\"file\" name=\"{0}\"/></p>";
    private static String after = "<p><input type=\"submit\"/><input type=\"hidden\" name=\"repo\" value=\"{0}\"><input type=\"hidden\" name=\"problem\" value=\"{1}\"><input type=\"hidden\" name=\"level\" value=\"{2}\"></p></form>";
    private static String end = "</body></html>";

    // TODO: Singular/plural

    private static String useStart = "<p>Use the following {0,choice,1#file|2#files}:</p>";
    private static String provideStart = "<p>Complete the following {0,choice,1#file|2#files}:</p>";
    // TODO: Separate out empty and nonempty files, with "provide" and "complete"

    @GET
    @javax.ws.rs.Path("/{problem}") 
    @Produces("text/html")
    public String files(@PathParam("problem") String problem)
    		throws IOException {
    	return files("ext", problem, "check", false);
    }

    @GET
    @javax.ws.rs.Path("/{problem}/{level}") 
    @Produces("text/html")
    public String files(@PathParam("problem") String problem, @PathParam("level") String level)
    		throws IOException {
    	return files("ext", problem, level, false);
    }
    
    @GET
    @javax.ws.rs.Path("/")    
    @Produces("text/html")
    public String files(@QueryParam("repo") @DefaultValue("ext") String repo,
                        @QueryParam("problem") String problemName,
                        @DefaultValue("check") @QueryParam("level") String level,
                        @DefaultValue("false") @QueryParam("upload") boolean upload)
    throws IOException {
        StringBuilder result = new StringBuilder();
        result.append(start);

        Path repoPath = Paths.get(context
                                  .getInitParameter("com.horstmann.codecheck.repo." + repo));
        // TODO: That comes from Problems.java--fix it there
        if (problemName.startsWith("/")) problemName = problemName.substring(1);
        Path problemPath = repoPath.resolve(problemName);
        Problem problem = new Problem(problemPath, level);
        boolean includeCode = true;
        String description = getDescription(problemPath, "statement.html"); // TODO: Legacy
        if (description == null) {
            description = getDescription(problemPath, "problem.html");
            if (description == null)
                description = getDescription(problemPath, "original-statement.html"); // TODO: legacy
        } else
            includeCode = false; // code already shown in statement.html

        // TODO: Indicate whether it is ok to add more classes
        // TODO: Should this be a part of the script?

        Set<Path> requiredFiles = problem.getRequiredFiles();
        Set<Path> useFiles = problem.getUseFiles();
        Map<Path, StringBuilder> contents = new HashMap<>();
        Iterator<Path> iter = useFiles.iterator();
        while (iter.hasNext()) {
        	Path p = iter.next();
        	StringBuilder cont = Util.htmlEscape(Util.read(problemPath, p));
        	if (cont.substring(0, 7).matches("//HIDE\\s"))
        		iter.remove();
        	else
        		contents.put(p, cont);
        }
        if (description != null)
            result.append(description);
        if (includeCode && useFiles.size() > 0) {
            result.append(MessageFormat.format(useStart, useFiles.size()));
            for (Path p : useFiles) {
            	StringBuilder cont = contents.get(p); 
                result.append("<p>");
                result.append(Util.tail(p).toString());
                result.append("</p>\n");
                result.append("<pre>");
                result.append(cont);
                result.append("</pre\n>");
            }
        }
        // TODO: In file upload, must still SHOW the non-empty required files
        String requestURL = request.getRequestURL().toString();
        String url = requestURL.substring(0, requestURL.indexOf("files")) + (upload ? "checkUpload" : "check");
        
        result.append(MessageFormat.format(before, url,
                                           upload ? "encoding=\"multipart/form-data\"" : ""));
        result.append(MessageFormat.format(provideStart, requiredFiles.size()));

        // TODO: Remove heuristic for codecomp
        if (!upload && useFiles.size() == 0 && requiredFiles.size() == 1) includeCode = true;

        for (Path p : requiredFiles) {
            String file = Util.tail(p).toString();
            if (upload) {
                if (includeCode) {
                    result.append("<p>");
                    result.append(file);
                    result.append("</p>\n");
                    result.append("<pre>");
                    result.append(Util.htmlEscape(Util.read(problemPath, p)));
                    result.append("</pre\n>");
                }
                result.append(MessageFormat.format(fileUpload, file));
            } else {
                int lines = 0;
                String cont = "";
                if (includeCode) {
                    cont = Util.read(problemPath, p);
                    lines = Util.countLines(cont);
                    if (cont == null) cont = "";
                }
                if (lines == 0) lines = 20;

                result.append(MessageFormat.format(fileAreaBefore, file, lines));
                result.append(Util.htmlEscape(cont));
                result.append(fileAreaAfter);
            }
        }

        result.append(MessageFormat.format(after, repo, problemName, level));
        result.append(end);
        return result.toString();
    }

    public static String getDescription(Path problemDir, String problemFile)
    throws IOException {
        String description = Util.read(problemDir, problemFile);
        if (description == null)
            return null;
        // Strip off HTML header. If the file contains "<body>" or "<BODY>",
        String lcdescr = description.toLowerCase();
        int start = lcdescr.indexOf("<body>");
        int end = -1;
        if (start != -1) {
            start += "<body>".length();
            end = lcdescr.lastIndexOf("</body>");
        } else {
            start = lcdescr.indexOf("<html>");
            if (start != -1) {
                start += "<html>".length();
                end = lcdescr.lastIndexOf("</html>");
            }
        }

        StringBuilder result = new StringBuilder(description);
        if (end != -1) result.replace(end, result.length(), "");
        if (start != -1) result.replace(0, start, "");

        Matcher matcher = IMG_PATTERN.matcher(result);
        start = 0;
        while (matcher.find(start)) {
            start = matcher.start(1);
            end = matcher.end(1);
            String replacement = "data:image/png;base64,"
                                 + Util.base64(problemDir, result.substring(start, end));
            result.replace(start, end, replacement);
            start += replacement.length();
        }

        // Old-style Wiley Plus crud
        Path ipf = problemDir.resolve("image.properties");
        if (java.nio.file.Files.exists(ipf)) {
            Properties ip = new Properties();
            ip.load(new FileInputStream(ipf.toFile()));
            String imageToken = "@" + ip.getProperty("image.token") + "@";
            start = result.indexOf(imageToken);
            if (start >= 0) {
                String replacement = "<img src=\"data:image/png;base64,"
                                     + Util.base64(problemDir, ip.getProperty("image.file"))
                                     + "\"/>";
                result.replace(start, start + imageToken.length(), replacement);
            }
        }
        return result.toString();
    }
}
