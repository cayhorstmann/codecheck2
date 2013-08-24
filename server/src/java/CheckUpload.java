import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.Random;
import java.util.Scanner;

import javax.servlet.ServletContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;

import org.glassfish.jersey.media.multipart.BodyPart;
import org.glassfish.jersey.media.multipart.MultiPart;

// TODO: Pretty URLs

@Path("/checkUpload")
public class CheckUpload {
    @Context
    ServletContext context;
    static Random random = new Random();

    // TODO: Can we just differentiate by consumes?
    @POST
    @Consumes("multipart/form-data")
    @Produces("text/html")
    public String check(MultiPart multiPart)
    throws IOException {
        try {
            String submissionPath = context
                                    .getInitParameter("com.horstmann.codecheck.submissions");
            File submissionDir = new File(submissionPath);
            if (!submissionDir.exists())
                submissionDir.mkdir();
            String TEMP_PREFIX = new SimpleDateFormat("yyMMdd")
            .format(new Date());
            File tempDir = createTempDir(submissionDir, TEMP_PREFIX);
            String repo = "ext";
            String problem = "";
            for (BodyPart bodyPart : multiPart.getBodyParts()) {
                // TODO: Ugh--how do we get these params?
                /*
                            String value = formParams.getFirst(key);
                            if (key.equals("repo"))
                               repo = value;
                            else if (key.equals("problem"))
                               problem = value;
                            else {
                					writeFile(tempDir, ..., bodyPart.getEntity().getInputStream());
                				}
                */
            }
            String repoPath = context
                              .getInitParameter("com.horstmann.codecheck.repo." + repo);
            Properties repoProps = new Properties();
            repoProps.load(new FileInputStream(new File(repoPath,
                                               "repo.properties")));
            String command = repoProps.getProperty("repo.command");
            Process process = Runtime.getRuntime().exec(
                                  MessageFormat.format(command, tempDir, problem));
            process.waitFor();

            Scanner in = new Scanner(process.getInputStream(), "UTF-8");
            StringBuilder result = new StringBuilder();
            while (in.hasNextLine()) {
                result.append(in.nextLine());
                result.append("\n");
            }
            in.close();
            return result.toString();
        } catch (Exception ex) {
            return ex.getMessage();
        }
    }

    private static File createTempDir(File parent, String prefix) {
        File tempDir = null;
        do {
            BigInteger n = new BigInteger(128, random);
            String name = prefix + n.toString(36);
            tempDir = new File(parent, name);
        } while (tempDir.exists());
        tempDir.mkdir();
        return tempDir;
    }
}
