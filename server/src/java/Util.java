import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;

import com.amazonaws.ClientConfiguration;
import com.amazonaws.Protocol;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;

public class Util {
	private static Random generator = new Random();
	

	public static Path getDir(ServletContext context, String key)
			throws IOException {
		String dirName = context.getInitParameter("com.horstmann.codecheck."
				+ key);
		Path dir = Paths.get(dirName);
		if (!Files.exists(dir))
			Files.createDirectory(dir);
		return dir;
	}

	public static Path tail(Path p) {
		return p.subpath(1, p.getNameCount());
	}

	public static String read(Path path) {
		try {
			return new String(java.nio.file.Files.readAllBytes(path), "UTF-8");
		} catch (IOException ex) {
			return null;
		}
	}

	public static String read(Path dir, String file) {
		return read(dir.resolve(file));
	}

	public static String read(Path dir, Path file) {
		return read(dir.resolve(file));
	}

	public static void write(Path parent, String name, String contents) {
		try {
			java.nio.file.Files.write(parent.resolve(name),
					contents.getBytes("UTF-8"));
		} catch (IOException ex) {
			ex.printStackTrace();
			// TODO: log
			throw new RuntimeException(ex);
		}
	}

	public static String base64(Path dir, String fileName) throws IOException {
		return new sun.misc.BASE64Encoder().encode(java.nio.file.Files
				.readAllBytes(dir.resolve(fileName)));
	}

	public static String getProperty(String dir, String file, String property) {
		File pf = new File(dir, file);
		if (pf.exists()) {
			Properties p = new Properties();
			try {
				p.load(new FileInputStream(pf));
				return p.getProperty(property);
			} catch (IOException ex) {
			}
		}
		return null;
	}

	public static int countLines(String s) {
		if (s == null)
			return 0;
		int lines = 0;
		for (int i = 0; i < s.length(); i++)
			if (s.charAt(i) == '\n')
				lines++;
		return lines;
	}

	public static StringBuilder htmlEscape(CharSequence s) {
		StringBuilder b = new StringBuilder();
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (c == '<')
				b.append("&lt;");
			else if (c == '>')
				b.append("&gt;");
			else if (c == '&')
				b.append("&amp;");
			else
				b.append(c);
		}
		return b;
	}

	public static Path createTempDirectory(Path parent) throws IOException {
		String prefix = new SimpleDateFormat("yyMMddkkmm").format(new Date());
		Set<PosixFilePermission> perms = PosixFilePermissions.fromString("rwxr-xr-x");
		return java.nio.file.Files.createTempDirectory(parent, prefix, PosixFilePermissions.asFileAttribute(perms));
	}

	public static String createUID() {
		String prefix = new SimpleDateFormat("yyMMddkkmm").format(new Date());
		BigInteger big = new BigInteger(128, generator);
		return prefix + big.toString(36);
	}

	public static void deleteDirectory(Path start) throws IOException {
		Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult visitFile(Path file,
					BasicFileAttributes attrs) throws IOException {
				Files.delete(file);
				return FileVisitResult.CONTINUE;
			}

			@Override
			public FileVisitResult postVisitDirectory(Path dir, IOException e)
					throws IOException {
				if (e == null) {
					Files.delete(dir);
					return FileVisitResult.CONTINUE;
				} else {
					// directory iteration failed
					throw e;
				}
			}
		});
	}

	public static void unzip(InputStream in, Path dir) throws IOException {
		// TODO: Check for containing dir whose name equals zip file
		ZipInputStream zin = new ZipInputStream(in);
		ZipEntry entry;
		while ((entry = zin.getNextEntry()) != null) {
			if (!entry.isDirectory()) {
				String name = entry.getName();
				Path outputPath = dir.resolve(name);
				Path parent = outputPath.getParent();
				Stack<Path> parents = new Stack<>();
				while (!parent.equals(dir)) {
					parents.push(parent);
					parent = parent.getParent();
				}
				while (!parents.empty()) {
					Path top = parents.pop();
					if (!java.nio.file.Files.exists(top))
						java.nio.file.Files.createDirectory(top);
				}
				OutputStream out = new FileOutputStream(outputPath.toFile());
				byte[] buf = new byte[1024];
				int len;
				while ((len = zin.read(buf)) > 0)
					out.write(buf, 0, len);
				out.close();
			}
			zin.closeEntry();
		}
		zin.close();
	}

	public static String runScript(String script) {
		try {
			Process process = Runtime.getRuntime().exec(script);
			process.waitFor();

			Scanner in = new Scanner(process.getErrorStream(), "UTF-8");
			StringBuilder result = new StringBuilder();
			while (in.hasNextLine()) {
				result.append(in.nextLine());
				result.append("\n");
			}
			in.close();
			if (result.length() > 0)
				return result.toString();

			in = new Scanner(process.getInputStream(), "UTF-8");
			result = new StringBuilder();
			while (in.hasNextLine()) {
				result.append(in.nextLine());
				result.append("\n");
			}
			in.close();
						
			// CAUTION: Apparently, one can't just large input from the process
			// stdout
			// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4062587
			return result.toString().trim();
		} catch (Exception ex) {
			return ex.getMessage();
		}
	}

	public static boolean isOnS3(ServletContext context, String repo) {
		String repoPath = context
				.getInitParameter("com.horstmann.codecheck.repo." + repo);		
		return repoPath == null;
	}
	
	private static String s3AccessKey = null;
	private static String s3SecretKey = null;
	
	public static void loadS3Credentials(ServletContext context) throws IOException
	{
		String s3CredentialsPath = context
				.getInitParameter("com.horstmann.codecheck.s3credentials");
		Properties props = new Properties();
		props.load(Files.newBufferedReader(Paths.get(s3CredentialsPath), StandardCharsets.UTF_8));
		s3AccessKey = props.getProperty("accessKey");
		s3SecretKey = props.getProperty("secretKey");
	}
	

	private static AmazonS3 getS3Connection() {
		AWSCredentials credentials = new BasicAWSCredentials(s3AccessKey,
				s3SecretKey);

		ClientConfiguration clientConfig = new ClientConfiguration();
		clientConfig.setProtocol(Protocol.HTTP);

		return new AmazonS3Client(credentials, clientConfig);
	}

	public static void putToS3(Path problemZip, String bucket, String key)
			throws IOException {
		InputStream in = Files.newInputStream(problemZip);
		try {
		   getS3Connection().putObject(bucket, key, in, new ObjectMetadata());
		} finally {
		  in.close();
		}
	}
	
	public static void deleteFromS3(String bucket, String key)
			throws IOException {
		getS3Connection().deleteObject(bucket, key);
	}
	

	// Delete returnedPath.getParent() when done
	public static Path unzipFromS3(String repo, String problem)
			throws IOException {
		Path unzipDir = java.nio.file.Files.createTempDirectory("problem");
		String id = problem.replaceAll("/", "_"); // No / in dir name
		Path problemDir = unzipDir.resolve(id);
		Files.createDirectory(problemDir);
		String bucket = repo + ".code-check.org";

		InputStream in = getS3Connection().getObject(bucket, problem)
				.getObjectContent();
		unzip(in, problemDir);
		in.close();
		return problemDir;
	}
		
	public static void runLabrat(ServletContext context, String repo,
			String problem, String level, String submissionDir, String metaData) throws IOException {
		String problemDir;

		// If problem is on S3 (eventually all will be)
		java.nio.file.Path unzipDir = null;
		if (isOnS3(context, repo)) {
			Path tempProblemDir = unzipFromS3(repo, problem);
			unzipDir = tempProblemDir.getParent();
			problemDir = tempProblemDir.toAbsolutePath().toString();
		} else {
			String repoPath = context
					.getInitParameter("com.horstmann.codecheck.repo." + repo);
			problemDir = repoPath + File.separator + problem;			
		}

		runLabrat(context, repo, problem, level, problemDir, submissionDir, metaData);

		if (unzipDir != null && Files.exists(Paths.get(submissionDir).resolve("report.html"))) // TODO: For now, leave directory for debugging
			deleteDirectory(unzipDir);
	}

	public static void runLabrat(ServletContext context, String repo,
			String problem, String level, String problemDir, String submissionDir, String metaData)
			throws IOException {
		// TODO: Obsolete
		String repoPath = context
				.getInitParameter("com.horstmann.codecheck.repo." + repo);
		String command = Util.getProperty(repoPath, "repo.properties",
				"repo.command");
		if (command == null)
			command = context
					.getInitParameter("com.horstmann.codecheck.defaultcommand");
		
		String script = MessageFormat.format(command, level, submissionDir,
				problemDir, metaData);
		String result = runScript(script);
		Files.write(Paths.get(submissionDir).resolve("codecheck.log"), (script + "\n" + result).getBytes("UTF-8"));
	}

	/**
	 * Gets all files contained in a directory and its subdirectories
	 * 
	 * @param dir
	 *            a directory
	 * @return the list of files, as Path objects that are relativized against
	 *         dir
	 * @throws IOException
	 */
	public static Set<Path> getDescendantFiles(final Path dir)
			throws IOException {
		final Set<Path> result = new TreeSet<>();
		if (dir == null || !Files.exists(dir))
			return result;
		Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult visitFile(Path file,
					BasicFileAttributes attrs) throws IOException {
				result.add(dir.relativize(file));
				return FileVisitResult.CONTINUE;
			}
		});
		return result;
	}

	public static Set<Path> getDescendantFiles(Path dir, List<String> subdirs)
			throws IOException {
		Set<Path> result = new TreeSet<>();
		for (String subdir : subdirs) {
			for (Path p : getDescendantFiles(dir.resolve(subdir))) {
				// Is there a matching one? If so, replace it
				boolean found = false;
				Iterator<Path> iter = result.iterator();
				while (!found && iter.hasNext())
					if (p.equals(Util.tail(iter.next()))) {
						iter.remove();
						found = true;
					}
				result.add(Paths.get(subdir).resolve(p));
			}
		}
		return result;
	}

	public static String javaClass(Path path) {
		String name = path.toString();
		if (!name.endsWith(".java"))
			return null;
		name = name.substring(0, name.length() - 5); // drop .java
		return name.replace(FileSystems.getDefault().getSeparator(), ".");
	}

	public static Path javaPath(String classname) {
		Path p = FileSystems.getDefault().getPath("", classname.split("[.]"));
		Path parent = p.getParent();
		if (parent == null)
			return FileSystems.getDefault().getPath(classname + ".java");
		else
			return parent.resolve(p.getFileName().toString() + ".java");
	}
	
	public static String hostURL(HttpServletRequest request) {
		String requestUrl = request.getRequestURL().toString(); 
			// If that doesn't work, try request.getHeader("referer")) --Referer: http://localhost:8080/codecheck-server/static/uploadProblem.html
		int pos = requestUrl.indexOf("//"); // http://
		pos = requestUrl.indexOf("/", pos + 2); // cs12.cs.sjsu.edu:8080/
		return requestUrl.substring(0, pos);
	}
	
    public static Set<Path> filterNot(Set<Path> paths, String... glob) {
        Set<Path> result = new TreeSet<>();
        PathMatcher[] matcher = new PathMatcher[glob.length];
        for (int i = 0; i < matcher.length; i++)
            matcher[i] = FileSystems.getDefault().getPathMatcher(
                             "glob:" + glob[i].replace("/", FileSystems.getDefault().getSeparator()));
        for (Path p : paths) {
            boolean matchesOne = false;
            for (int i = 0; i < matcher.length && !matchesOne; i++)
                if (matcher[i].matches(p.getFileName()))
                    matchesOne = true;
            if (!matchesOne)
                result.add(p);
        }
        return result;
    }    	
}
