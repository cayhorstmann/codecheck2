import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Properties;

import javax.servlet.ServletContext;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;

@Path("/problems")
public class Problems {
    @Context
    ServletContext context;

    private static String start = "<html><head><title>Problems</title><style type=\"text/css\">\n"
                                  + "body { font-style: sans; }\n"
                                  + ".dir { font-weight: bold; }\n"
                                  + "ul { list-style-type: square; }\n"
                                  + "</style><body><ul>\n";
    private static String end = "</ul></body></html>\n";
    private static String dirlink = "<li><a class=\"dir\" href=\"problems?repo={0}&path={1}\">{2}</a></b></li>\n";
    private static String problemlink = "<li><a href=\"files?repo={0}&problem={1}\">{2}</a></li>\n";

    @GET
    @Produces("text/html")
    public String problems(@QueryParam("repo") String repo,
                           @QueryParam("path") String path) throws IOException {
        String repoPath = context
                          .getInitParameter("com.horstmann.codecheck.repo." + repo);
        StringBuilder result = new StringBuilder();
        result.append(start);
        File dir;
        if (path == null)
            dir = new File(repoPath);
        else
            dir = new File(repoPath, path);
        for (File f : numericSort(dir.listFiles())) {
            String newPath = (path == null ? "" : path + "/") + f.getName();
            if (f.isDirectory()) {
                File checkProperties = new File(f, "student" + File.separator + "check.properties");
                File qProperties = new File(f, "q.properties");
                if (checkProperties.exists() || qProperties.exists()) {
                    // It's a problem
                    String title = null;

                    if (qProperties.exists()) {
                        Properties p = new Properties();
                        p.load(new FileInputStream(qProperties));
                        title = p.getProperty("question.title");
                        if (title == null) title = p.getProperty("title");
                    }
                    if (title == null && checkProperties.exists()) {
                        Properties p = new Properties();
                        p.load(new FileInputStream(checkProperties));
                        title = p.getProperty("title");
                    }
                    if (title == null)
                        title = f.getName();
                    result.append(MessageFormat.format(problemlink, repo,
                                                       newPath, title));
                } else {
                    // it's a directory
                    // TODO: Util for getting a property with default
                    String title = null;
                    File gProperties = new File(f, "g.properties");
                    if (gProperties.exists()) {
                        Properties p = new Properties();
                        p.load(new FileInputStream(gProperties));
                        title = p.getProperty("group.title");
                    }
                    if (title == null)
                        title = f.getName();
                    result.append(MessageFormat.format(dirlink, repo, newPath,
                                                       title));
                }
            }
        }
        result.append(end);
        return result.toString();
    }

    private static File[] numericSort(File[] files) {
        Arrays.sort(files, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                // Break into sequence of numeric and non-numeric
                Object[] a1 = decompose(f1.getName());
                Object[] a2 = decompose(f2.getName());
                return compare(a1, a2);
            }

            public Object[] decompose(String s) {
                String[] nums = s.split("[^0-9]+");
                String[] nonNums = s.split("[0-9]+");
                int n = nums.length;
                int numstart = 0;
                if (n > 0 && nums[0].equals("")) {
                    numstart++;
                    n--;
                }
                Object[] result = new Object[2 * n + 1];
                int k;
                for (k = 0; k < result.length - 1; k += 2) {
                    result[k] = nonNums[k / 2];
                    result[k + 1] = Integer.parseInt(nums[k / 2 + numstart]);
                }
                if (k / 2 < nonNums.length) result[k] = nonNums[k / 2];
                else result[k] = "";
                return result;
            }

            public int compare(Object[] a, Object[] b) {
                int i;
                for (i = 0; i < a.length; i++) {
                    if (i > b.length) // a is longer, hence later
                        return 1;
                    int d;
                    if (a[i] instanceof Integer && b[i] instanceof Integer) d = ((Integer) a[i]).compareTo((Integer) b[i]);
                    else d = a[i].toString().compareTo(b[i].toString());
                    if (d != 0) return d;
                }
                if (i < b.length) return -1;
                else return 0;
            }
        });
        return files;
    }
}
