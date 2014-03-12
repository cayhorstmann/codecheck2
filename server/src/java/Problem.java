import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

public class Problem {
    private Path problemPath;
    private Set<Path> requiredFiles = new TreeSet<>();
    private Set<Path> useFiles = new TreeSet<>();
    private Properties checkProperties = new Properties();
    private int level;
    private List<String> studentDirectories = new ArrayList<>();
    private List<String> solutionDirectories = new ArrayList<>();

    public Path getProblemPath() {
        return problemPath;
    }

    public Problem(Path problemPath, String levelString) {
        this.problemPath = problemPath;
        if (levelString.equals("check")) level = 1; // NOTE: "grade" doesn't work for web app
        else {
            try {
                level = Integer.parseInt(levelString);
            } catch (NumberFormatException ex) {
                ex.printStackTrace();
            }
        }
        try {
            getLevelDirectories();
            checkProperties = gatherProperties();
            classifyFiles();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public Set<Path> getRequiredFiles() {
        return requiredFiles;
    }

    public Set<Path> getUseFiles() {
        return useFiles;
    }

    private void getLevelDirectories() {
        if (Files.exists(problemPath.resolve("student")))
            studentDirectories.add("student");
        for (int n = 1; n <= level; n++)
            if (Files.exists(problemPath.resolve("student" + n)))
                studentDirectories.add("student" + n);

        if (Files.exists(problemPath.resolve("solution")))
            solutionDirectories.add("solution");
        for (int n = 1; n <= level; n++)
            if (Files.exists(problemPath.resolve("solution" + n)))
                solutionDirectories.add("solution" + n);
    }

    private Properties gatherProperties() throws IOException {
        Properties props = new Properties();
        for (String levelDir : studentDirectories) {
            File modeCheckProperties = problemPath.resolve(levelDir).resolve("check.properties").toFile();
            if (modeCheckProperties.exists()) {
                try (InputStream in = new FileInputStream(modeCheckProperties)) {
                    props.load(in);
                }
            }
        }
        return props;
    }

    public String getStringProperty(String key, String... fallbackKeysAndDefaultValue) {
        String value = checkProperties.getProperty(key);
        if (value != null)
            return value;
        for (int i = 0; i < fallbackKeysAndDefaultValue.length - 1; i++) {
            value = checkProperties.getProperty(fallbackKeysAndDefaultValue[i]);
            if (value != null)
                return value;
        }
        if (fallbackKeysAndDefaultValue.length > 0)
            return fallbackKeysAndDefaultValue[fallbackKeysAndDefaultValue.length - 1];
        else
            return null;
    }

    public boolean getBooleanProperty(String key, boolean defaultValue) {
        String value = checkProperties.getProperty(key);
        if (value == null)
            return defaultValue;
        return value.toLowerCase().equals("true");
    }

    private Path find(Path p) {
        for (int i = studentDirectories.size() - 1; i >= 0; i--) {
            String dir = studentDirectories.get(i);
            Path dp = Paths.get(dir).resolve(p);
            if (java.nio.file.Files.exists(problemPath.resolve(dp)))
                return dp;
        }
        return Paths.get("student0").resolve(p);
    }

    private Path findClass(String cl) {
        return find(Util.javaPath(cl));
    }

    private void classifyFiles() throws IOException {
        Set<Path> solutionFiles = Util.getDescendantFiles(problemPath, solutionDirectories);
        // Remove rubrics
        Iterator<Path> iter = solutionFiles.iterator();
        while (iter.hasNext()) if (iter.next().toString().endsWith(".txt")) iter.remove();

        String mainclass = getStringProperty("mainclass");

        if (mainclass == null && solutionFiles.size() == 1)
            mainclass = Util.javaClass(Util.tail(solutionFiles.iterator().next()));

        String requiredclasses = getStringProperty("requiredclasses");

        if (requiredclasses != null)
            for (String cl : requiredclasses.trim().split("\\s*,\\s*"))
                requiredFiles.add(findClass(cl));
        else       // TODO: Maybe always
            for (Path p : solutionFiles)
                if (p.toString().endsWith(".java"))
                    requiredFiles.add(find(Util.tail(p)));

        String editclass = getStringProperty("editclass");
        // Ask to complete editclass (if exists--for codecomp) or mainclass
        // editclass=X is shortcut for requiredclasses=X,
        // mainclass.required=false

        if (editclass != null)
            requiredFiles.add(findClass(editclass));
        else if (mainclass != null && getBooleanProperty("mainclass.required", true))
            requiredFiles.add(findClass(mainclass));

        String nodoc = getStringProperty("nodoc");
        List<String> nodocCl = new ArrayList<String>();
        if (nodoc != null)
            nodocCl.addAll(Arrays.asList(nodoc.trim().split("\\s*,\\s*")));

        Set<Path> studentFiles = Util.getDescendantFiles(problemPath, studentDirectories);
        studentFiles = Util.filterNot(studentFiles, ".*");

        for (Path path : studentFiles)
            if (path.toString().endsWith(".java")) {
                String cl = Util.javaClass(Util.tail(path));
                if (!requiredFiles.contains(path) && !nodocCl.contains(cl))
                    useFiles.add(path);
            }
        if (editclass != null)
            useFiles.remove(findClass(mainclass));
    }
}
