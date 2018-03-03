package models;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
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
import java.util.stream.Stream;

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
		if (levelString.equals("check"))
			level = 1; // TODO: Legacy
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

	private static Pattern solutionPattern = Pattern.compile("\\s*[\\PL&&[^\\s]]+(SOLUTION|SHOW|EDIT)($|\\s.*|[\\PL].*)");

	// No letters or spaces, SOLUTION, optional no letters, trailing space (e.g.
	// \r), \n

	public static boolean isSolution(Path p) {
		// TODO: Filter image extensions etc.
		try (Stream<String> lines = Files.lines(p)) {
			return lines.anyMatch(solutionPattern.asPredicate());
		} catch (IOException | UncheckedIOException ex) { // This happens if the file is binary
			return false;
		}
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
			File modeCheckProperties = problemPath.resolve(levelDir)
					.resolve("check.properties").toFile();
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

	public String getStringProperty(String key,
			String... fallbackKeysAndDefaultValue) {
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
		Set<Path> solutionFiles = Util.getDescendantFiles(problemPath,
				solutionDirectories);
		solutionFiles = Util.filterNot(solutionFiles, ".*", "*.class");

		// TODO: Remove rubrics
		Iterator<Path> iter = solutionFiles.iterator();
		while (iter.hasNext())
			if (iter.next().toString().endsWith(".txt"))
				iter.remove();

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
		else if (mainclass != null
				&& getBooleanProperty("mainclass.required", true))
			requiredFiles.add(findClass(mainclass));

		String nodoc = getStringProperty("nodoc");
		List<String> nodocCl = new ArrayList<String>();
		if (nodoc != null)
			nodocCl.addAll(Arrays.asList(nodoc.trim().split("\\s*,\\s*")));

		Set<Path> studentFiles = Util.getDescendantFiles(problemPath,
				studentDirectories);
		studentFiles = Util.filterNot(studentFiles, ".*", "*.class", "param.js");

		// TODO: We only show students source files, not text or images
		// Would be better to show those as well
		// But then need to filter out problem.html and the images used inside

		for (Path path : studentFiles)
			if (isSolution(problemPath.resolve(path)))
				requiredFiles.add(path);
			else if (isSourceExtension(Util.extension(path))) {
				String cl = Util.moduleOf(Util.tail(path));
				if (!requiredFiles.contains(path) && !nodocCl.contains(cl))
					useFiles.add(path);
			}
		if (editclass != null)
			useFiles.remove(findClass(editclass));
	}

	public static boolean isSourceExtension(String extension) {
		return Arrays.asList("java", "c", "cpp", "c++", "h", "py", "scala",
				"m", "rkt", "js", "cs", "hs").contains(extension);
	}

	public static boolean isPseudocomment(String line, String type,
			String start, String end) {
		line = line.trim();
		if (!line.startsWith(start + type))
			return false;
		if (!line.endsWith(end))
			return false;
		int slen = start.length();
		int tlen = type.length();
		int elen = end.length();
		if (line.length() == slen + tlen + elen)
			return true;
		// If there is stuff after the type, there must be a white space
		return Character.isWhitespace(line.charAt(slen + tlen));
	}

	public static List<String> processHideShow(Path filepath, String contents){
		ArrayList <String> result = new ArrayList<String> ();
		if (contents == null){
			result.add("");
			return result;
		}
		String start = "//"; 
		String end = "";
		String[] lines = contents.split("\n");
		String extension = Util.extension(filepath);
		if (extension.equals("py")) {
			start = "##";
		} else if (extension.equals("rkt")) {
			start = ";;";
		} else if (extension.equals("hs")) {
			start = "--";
		} else if (extension.equals("c")) {
			start = "/*";
			end = "*/";
		}
		
		boolean hasEdit = false;
		for (int i = 0; i < lines.length && !hasEdit; ++i){
			if (isPseudocomment(lines[i], "EDIT", start, end)) {
				hasEdit = true;
			}
		}
		
		if (hasEdit) {
			int sectionStart = 0;
			boolean hiding = false;
			boolean editOnPreviousLine = false;
			boolean startWithEdit = false;
			for (int i = 0; i < lines.length; i++) {
				String line = lines[i].trim();
				if (isPseudocomment(line, "EDIT", start, end)) {
					hiding = false;
					if (!editOnPreviousLine) { // emit preceding readonly section
						StringBuilder section = new StringBuilder();
						for (int j = sectionStart; j < i; j++){
							if (lines[j] != null) {
								section.append(lines[j]);
								section.append("\n");
							}
						}
						if (section.length() == 0) // Don't add a blank readonly section at the top
						{
							if (result.size() > 0) result.add("\n");
						}
						else						 
							result.add(section.toString());
						sectionStart = i;
					}					
					editOnPreviousLine = true;
					
					String showString = start + "EDIT";
					int n1 = lines[i].indexOf(showString);
					int n2 = showString.length();
					int n3 = lines[i].lastIndexOf(end);
					if (n1 + n2 < n3)
						lines[i] = lines[i].substring(0, n1)
								+ lines[i].substring(n1 + n2 + 1, n3);
					else
						lines[i] = ""; // Edit section is never empty				
				}
				else {
					if (editOnPreviousLine) { // emit edit section
						StringBuilder section = new StringBuilder();
						for (int j = sectionStart; j < i; j++){
							if (lines[j] != null) {
								section.append(lines[j]);
								section.append("\n");
							}
						}
						if (section.toString().trim().length() == 0)
						{	section.insert(0, "\n");
							section.append("\n");
						}
						if (result.size() == 0) startWithEdit = true; 
						result.add(section.toString());
						sectionStart = i;						
					}
					editOnPreviousLine = false;
					
					if (isPseudocomment(line, "HIDE", start, end)) hiding = true;
					if (hiding) lines[i] = null;
					else if (isPseudocomment(line, "SOLUTION", start, end)
							|| isPseudocomment(line, "CALL", start, end)
							|| isPseudocomment(line, "ID", start, end)
							|| isPseudocomment(line, "ARGS", start, end)
							|| isPseudocomment(line, "IN", start, end)
							|| isPseudocomment(line, "OUT", start, end)){
						lines[i] = null;
					} else if(isPseudocomment(line, "REQUIRED", start, end)
							|| isPseudocomment(line, "FORBIDDEN", start, end)){
						lines[i] = null;
						if (i < lines.length - 1) {
							String nextLine = lines[i + 1].trim();
							if (nextLine.startsWith(start) && nextLine.endsWith(end)) {
								lines[i + 1] = null;
								i++;
							}
						}
					} else if (isPseudocomment(line, "SHOW", start, end)) {
						hiding = false;
						String showString = start + "SHOW";
						int n1 = lines[i].indexOf(showString);
						int n2 = showString.length();
						int n3 = lines[i].lastIndexOf(end);
						if (n1 + n2 < n3)
							lines[i] = lines[i].substring(0, n1)
									+ lines[i].substring(n1 + n2 + 1, n3);
						else
							lines[i] = null;
					} else if (line.contains(start + "SUB ")) {
						int n = lines[i].indexOf(start + "SUB");
						int n2 = end.equals("") ? lines[i].length() : lines[i].indexOf(
								end, n) + end.length();
						lines[i] = lines[i].substring(0, n) + lines[i].substring(n2);
					}
				}
			}
			// Emit final section
			StringBuilder section = new StringBuilder();
			for (int j = sectionStart; j < lines.length; j++) {
				if (lines[j] != null) {
					section.append(lines[j]);
					section.append("\n");
				}
			}
			if (editOnPreviousLine && section.toString().trim().length() == 0)
			{	
				section.insert(0, "\n");
				section.append("\n");
			}
			
			result.add(section.toString());
			if (!startWithEdit) result.add(0, null);
			return result;
		} else { // SHOW mode			
			boolean hiding = false;
			boolean somethingHidden = false;
			boolean isSolution = false;			
			
			for (int i = 0; i < lines.length; i++) {
				String line = lines[i].trim();
				if (isPseudocomment(line, "SOLUTION", start, end)) {
					isSolution = true;
					lines[i] = null;
				} else if (isPseudocomment(line, "HIDE", start, end)) {
					hiding = true;
					somethingHidden = true;
					lines[i] = null;
				} else if (isPseudocomment(line, "SHOW", start, end)) {
					hiding = false;
					isSolution = true;
					String showString = start + "SHOW";
					int n1 = lines[i].indexOf(showString);
					int n2 = showString.length();
					int n3 = lines[i].lastIndexOf(end);
					if (n1 + n2 < n3)
						lines[i] = lines[i].substring(0, n1)
								+ lines[i].substring(n1 + n2 + 1, n3);
					else
						lines[i] = null;
				} else if (hiding) {
					lines[i] = null;
				} else if (isPseudocomment(line, "CALL", start, end)
						|| isPseudocomment(line, "ID", start, end)
						|| isPseudocomment(line, "ARGS", start, end)
						|| isPseudocomment(line, "IN", start, end)
						|| isPseudocomment(line, "OUT", start, end)) {
					lines[i] = null; // TODO: More cases like that? Student files
										// shouldn't have pseudocomments
				} else if (isPseudocomment(line, "REQUIRED", start, end)
						|| isPseudocomment(line, "FORBIDDEN", start, end)) {
					lines[i] = null;
					String nextLine = lines[i + 1].trim();
					if (nextLine.startsWith(start) && nextLine.endsWith(end)) {
						lines[i + 1] = null;
						i++;
					}
				} else if (line.contains(start + "SUB ")) {
					int n = lines[i].indexOf(start + "SUB");
					int n2 = end.equals("") ? lines[i].length() : lines[i].indexOf(
							end, n) + end.length();
					lines[i] = lines[i].substring(0, n) + lines[i].substring(n2);
				}
			}
			if (isSolution && !somethingHidden) {
				result.add("");
				return result;
			}
			StringBuilder allRemainingLines = new StringBuilder();
			for (String l : lines) {
				if (l != null) {
					allRemainingLines.append(l);
					allRemainingLines.append("\n");
				}
			}
			result.add(allRemainingLines.toString());
			return result;						
		}
	}	
	
	/*
	public static String processHideShow(Path filepath, String contents) {
		if (contents == null)
			return "";
		if (!isSource(filepath))
			return contents;
		String start = "//";
		String end = "";
		String extension = Util.extension(filepath);
		if (extension.equals("py")) {
			start = "##";
		} else if (extension.equals("rkt")) {
			start = ";;";
		}
		String[] lines = contents.split("\n");
		boolean hiding = false;
		boolean isSolution = false;
		boolean somethingHidden = false;
		for (int i = 0; i < lines.length; i++) {
			String line = lines[i].trim();
			if (isPseudocomment(line, "HIDDEN", start, end))
				return "";
			else if (isPseudocomment(line, "SOLUTION", start, end)) {
				isSolution = true;
				lines[i] = null;
			} else if (isPseudocomment(line, "HIDE", start, end)) {
				hiding = true;
				somethingHidden = true;
				lines[i] = null;
			} else if (isPseudocomment(line, "SHOW", start, end)) {
				hiding = false;
				String showString = start + "SHOW";
				int n1 = lines[i].indexOf(showString);
				int n2 = showString.length();
				int n3 = lines[i].lastIndexOf(end);
				if (n1 + n2 < n3)
					lines[i] = lines[i].substring(0, n1)
							+ lines[i].substring(n1 + n2 + 1, n3);
				else
					lines[i] = null;
			} else if (hiding) {
				lines[i] = null;
			} else if (isPseudocomment(line, "CALL", start, end)
					|| isPseudocomment(line, "ID", start, end)
					|| isPseudocomment(line, "ARGS", start, end)
					|| isPseudocomment(line, "IN", start, end)
					|| isPseudocomment(line, "OUT", start, end)) {
				lines[i] = null; 

// TODO: filter out IGNORExxx directives
                                
			} else if (isPseudocomment(line, "REQUIRED", start, end)
					|| isPseudocomment(line, "FORBIDDEN", start, end)) {
				lines[i] = null;
				String nextLine = lines[i + 1].trim();
				if (nextLine.startsWith(start) && nextLine.endsWith(end)) {
					lines[i + 1] = null;
					i++;
				}
			} else if (line.contains(start + "SUB ")) {
				int n = lines[i].indexOf(start + "SUB");
				int n2 = end.equals("") ? lines[i].length() : lines[i].indexOf(
						end, n) + end.length();
				lines[i] = lines[i].substring(0, n) + lines[i].substring(n2);
			}
		}
		if (isSolution && !somethingHidden)
			return "";
		StringBuilder result = new StringBuilder();
		for (String l : lines)
			if (l != null) {
				result.append(l);
				result.append("\n");
			}
		return result.toString();
	}
	*/
}
