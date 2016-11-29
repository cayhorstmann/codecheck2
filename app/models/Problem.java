package models;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystems;
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
import java.util.regex.Pattern;

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
    
    private static Pattern solutionPattern = Pattern.compile("\\s*[^\\pL&&[^\\s]]*SOLUTION[^\\pL]*\\s*\\n");
	// No letters or spaces, SOLUTION, optional no letters, trailing space (e.g. \r), \n

    public boolean isSolution(Path p) {
    	String cont = Util.read(problemPath, p);
    	return solutionPattern.matcher(cont).lookingAt();
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
        
        if (studentDirectories.size() + solutionDirectories.size() == 0) {
        	// new-style packaging with no student or solution directories
        	studentDirectories.add(".");
        }        
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
        File qProperties = problemPath.resolve("q.properties").toFile();
        if (qProperties.exists()) {
            try (InputStream in = new FileInputStream(qProperties)) {
                props.load(in);
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

    private Path findClass(String cl) { // TODO: Legacy
		Path p = FileSystems.getDefault().getPath("", cl.split("[.]"));
		Path parent = p.getParent();
		if (parent == null)
			p = FileSystems.getDefault().getPath(cl + ".java");
		else
			p = parent.resolve(p.getFileName().toString() + ".java");
    	
        return find(p);
    }

   // TODO: Unify with client identification code
    private void classifyFiles() throws IOException {
        Set<Path> solutionFiles = Util.getDescendantFiles(problemPath, solutionDirectories);
        solutionFiles = Util.filterNot(solutionFiles, ".*", "*.class");

        // TODO: Remove rubrics
        Iterator<Path> iter = solutionFiles.iterator();
        while (iter.hasNext()) if (iter.next().toString().endsWith(".txt")) iter.remove();

        String mainclass = getStringProperty("mainclass");
        String requiredclasses = getStringProperty("requiredclasses");

        if (requiredclasses != null)
            for (String cl : requiredclasses.trim().split("\\s*,\\s*"))
                requiredFiles.add(findClass(cl));
        else    
            for (Path p : solutionFiles)               
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
        studentFiles = Util.filterNot(studentFiles, ".*", "*.class");

        // TODO: We only show students source files, not text or images
        // Would be better to show those as well
        // But then need to filter out problem.html and the images used inside  
        
        for (Path path : studentFiles)
        	if (isSolution(path))
        		requiredFiles.add(path);
			else if (isSource(path)) {
                String cl = Util.moduleOf(Util.tail(path));
                if (!requiredFiles.contains(path) && !nodocCl.contains(cl))
                    useFiles.add(path);
            }
        if (editclass != null) 
            useFiles.remove(findClass(editclass));
    }
    
	public static boolean isSource(Path path) {
		return isSource(path.toString());
	}
	
	public static boolean isSource(String path) {
		return Arrays.asList("java", "c", "cpp", "c++", "h", "py", "scala", "m", "rkt", "js", "cs").contains(
				Util.extension(path));
	}

	public static String processHideShow(Path filepath, String contents)
	{
		if (contents == null) return "";
		if (!isSource(filepath)) return contents;
		String start = "//";
		String end = "";
		String extension = Util.extension(filepath);
		if (extension.equals("py")) {
                   start = "##";
		} else if (extension.equals("c")) {
                   start = "/*"; end = "*/";
		} else if (extension.equals("rkt")) {
                   start = ";;";
                }
		String[] lines = contents.split("\n");
		boolean hiding = false;
		boolean isSolution = false;
		boolean somethingHidden = false;
		for (int i = 0; i < lines.length; i++) {
			String line = lines[i].trim();
			if (line.equals(start + "HIDDEN" + end)) return "";
			else if (line.equals(start + "SOLUTION" + end)) {
				isSolution = true;
				lines[i] = null;						
			} else if (line.equals(start + "HIDE" + end)) {
				hiding = true;
				somethingHidden = true;
				lines[i] = null;
			} else if (line.startsWith(start + "SHOW")){
				hiding = false;
				String showString = start + "SHOW";
				int n = lines[i].indexOf(showString);
				int n2 = end.equals("") ? lines[i].length() : lines[i].indexOf(end, n);
				lines[i] = lines[i].substring(0, n) + lines[i].substring(n + showString.length(), n2).trim() + lines[i].substring(n2 + end.length());
			} else if (hiding) { 
				lines[i] = null;
			} else if (line.startsWith(start + "CALL ") 
					|| line.startsWith(start + "ID ") 
					|| line.startsWith(start + "ARGS") 
					|| line.startsWith(start + "IN ")
					|| line.startsWith(start + "OUT")) {
				lines[i] = null; // TODO: More cases like that? Student files shouldn't have pseudocomments
			} else if (line.startsWith(start + "REQUIRED") || line.startsWith(start + "FORBIDDEN")) {
				lines[i] = null;
				if (lines[i + 1].startsWith(start) && lines[i + 1].endsWith(end)) {
					lines[i + 1] = null;
					i++;
				}					
			} else if (line.contains(start + "SUB ")) {			
				int n = lines[i].indexOf(start + "SUB");
				int n2 = end.equals("") ? lines[i].length() : lines[i].indexOf(end, n) + end.length();
				lines[i] = lines[i].substring(0, n) + lines[i].substring(n2);
			}			
		}
		if (isSolution && !somethingHidden) return "";
		StringBuilder result = new StringBuilder();
		for (String l : lines) if (l != null) { result.append(l); result.append("\n"); }
		return result.toString();
	}	    
}
