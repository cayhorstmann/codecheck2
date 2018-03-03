package models;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
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
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class Util {
	private static Random generator = new Random();

	public static boolean isEmpty(String str) { return str == null || str.isEmpty(); }
	
	public static Path tail(Path p) {
		return p.subpath(1, p.getNameCount());
	}

	public static String read(Path path) {
		try {
			return new String(java.nio.file.Files.readAllBytes(path), "UTF-8").replace("\r\n", "\n");
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
		Base64.Encoder encoder = Base64.getEncoder();
		return encoder.encodeToString(java.nio.file.Files
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

	public static StringBuilder removeTrailingNewline(StringBuilder b) {
		if (b.length() > 0 && b.charAt(b.length() - 1) == '\n')
			b.deleteCharAt(b.length() - 1);
		return b;
	}
	
	public static StringBuilder htmlEscape(CharSequence s) {
		StringBuilder b = new StringBuilder();
		if (s == null) return b;
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
		Set<PosixFilePermission> perms = PosixFilePermissions
				.fromString("rwxr-xr-x");
		return java.nio.file.Files.createTempDirectory(parent, prefix,
				PosixFilePermissions.asFileAttribute(perms));
	}

	public static String createUID() {
		String prefix = new SimpleDateFormat("yyMMddkkmm").format(new Date());
		BigInteger big = new BigInteger(128, generator);
		return prefix + big.toString(36);
	}

	public static void deleteDirectory(Path start) throws IOException {
		if (!Files.exists(start)) return;
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
	
	public static void copyDirectory(Path source, Path target) throws IOException {
		try {
			Files.walk(source).forEach(p -> {
				try {
					Path q = target.resolve(source.relativize(p));
					if (Files.isDirectory(p)) {
						if (!Files.exists(q)) Files.createDirectory(q);
					}
					else
						Files.copy(p, q);
					} catch (IOException ex) {
						throw new UncheckedIOException(ex);
					}
				});
		} catch (UncheckedIOException ex) {
			throw ex.getCause();
		}
	}

	public static void unzip(InputStream in, Path dir) throws IOException {
		ZipInputStream zin = new ZipInputStream(in);
		ZipEntry entry;
		while ((entry = zin.getNextEntry()) != null) {
			String name = entry.getName();
			Path outputPath = dir.resolve(name);
			if (!entry.isDirectory() 
					&& !name.startsWith("__MACOSX/") 
					&& !outputPath.getFileName().toString().startsWith(".")) {
				Path parent = outputPath.getParent();
				java.nio.file.Files.createDirectories(parent);
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

	public static String runProcess(String command, int millis) {
		try {
			Process process = Runtime.getRuntime().exec(command);
			boolean completed = process.waitFor(millis, TimeUnit.MILLISECONDS);

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

            if (!completed) {
            	process.destroyForcibly();
            	result.append("\nTimeout after " + millis + " milliseconds\n");
            }
			
			// CAUTION: Apparently, one can't just large input from the process
			// stdout
			// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4062587
			return result.toString().trim();
		} catch (Exception ex) {
			return ex.getMessage();
		}
	}
	
	public static int runProcess(List<String> cmd, Path directory, StringBuilder output, int millis) {
	 try {
            Path out = Files.createTempFile("codecheck", "");
            Path in = null;
            try {
                ProcessBuilder builder = new ProcessBuilder(cmd);
                builder.directory(directory.toFile());
                builder.redirectErrorStream(true);
                builder.redirectOutput(out.toFile());
                Process process = builder.start();
                boolean completed = process.waitFor(millis, TimeUnit.MILLISECONDS);
                int exitValue = process.exitValue();
                String result = new String(Files.readAllBytes(out), StandardCharsets.UTF_8);
                if (!completed) {
                	process.destroyForcibly();
                	result += "\nTimeout after " + millis + " milliseconds\n";
                }
                output.append(result);
                return exitValue;
            } finally {
                if (in != null) Files.delete(in);                
                Files.deleteIfExists(out);
            }                
        } catch (Exception ex) {
            output.append(getStackTrace(ex));
            return -1;
        }
	}

	public static void zip(Path source, Path zipPath) throws IOException {
		URI uri;
		try {
			uri = new URI("jar", zipPath.toUri().toString(), null);
			Files.deleteIfExists(zipPath);
			// Constructs the URI jar:file://myfile.zip
			try (FileSystem zipfs = FileSystems.newFileSystem(uri,
					Collections.singletonMap("create", "true"))) {
				Files.walk(source).forEach(p -> {
					try {
						String target = source.relativize(p).toString();
						if (target.length() > 0) {
							System.out.println(p + ", " + target);
							Path q = zipfs.getPath("/" + target);
							if (Files.isDirectory(p))
								Files.createDirectory(q);
							else
								Files.copy(p, q);
						}
					} catch (IOException ex) {
						throw new java.io.UncheckedIOException(ex);
				}});
			}
		} catch (URISyntaxException e) {
			throw new IOException(e);
		} catch (java.io.UncheckedIOException ex) {
			throw ex.getCause();
		}
	}

	/**
	 * Gets all files contained in a directory but not its subdirectories
	 * 
	 * @param dir
	 *            a directory
	 * @return the list of files, as Path objects that are relativized against
	 *         dir
	 * @throws IOException
	 */
	public static Set<Path> getFiles(final Path dir)
			throws IOException {
		if (dir == null || !Files.exists(dir) || !Files.isDirectory(dir))
			return Collections.emptySet();
		return Files.list(dir).filter(Files::isRegularFile).map(dir::relativize).collect(Collectors.toSet());
	}
	
	@FunctionalInterface
	public interface EConsumer<T, E extends Throwable> {
		void accept(T t) throws E;
	}
	
	public static void forEachFile(Path dir, EConsumer<Path, IOException> body) throws IOException {
		if (dir != null && Files.exists(dir) && Files.isDirectory(dir)) {
			try (Stream<Path> stream = Files.list(dir)) {
				for (Path p : stream.filter(Files::isRegularFile).collect(Collectors.toSet()))
					body.accept(p);
			}
		}
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
	public static SortedSet<Path> getDescendantFiles(final Path dir)
			throws IOException {
		final SortedSet<Path> result = new TreeSet<>();
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

	public static SortedSet<Path> getDescendantFiles(Path dir, List<String> subdirs)
			throws IOException {
		SortedSet<Path> result = new TreeSet<>();
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

	public static String moduleOf(Path path) {
		String name = path.toString();
		if (name.endsWith(".java")) {
			name = name.substring(0, name.length() - 5); // drop .java
			return name.replace(FileSystems.getDefault().getSeparator(), ".");
		} else {
			int n = name.lastIndexOf('.');
			return name.substring(0, n);
		}
	}

	public static String extension(Path path) {
		return extension(path.toString());
	}
	
	public static String extension(String path) {
		String name = path.toString();
		int n = name.lastIndexOf('.');
		if (n == -1)
			return "";
		else
			return name.substring(n + 1).toLowerCase();
	}
	/*
	public static String hostURL(javax.servlet.http.HttpServletRequest request) {
		String requestUrl = request.getRequestURL().toString();
		// If that doesn't work, try request.getHeader("referer")) --Referer:
		// http://localhost:8080/codecheck-server/static/uploadProblem.html
		int pos = requestUrl.indexOf("//"); // http://
		pos = requestUrl.indexOf("/", pos + 2); // cs12.cs.sjsu.edu:8080/
		return requestUrl.substring(0, pos);
	}
    */
	public static Set<Path> filterNot(Set<Path> paths, String... glob) {
		Set<Path> result = new TreeSet<>();
		PathMatcher[] matcher = new PathMatcher[glob.length];
		for (int i = 0; i < matcher.length; i++)
			matcher[i] = FileSystems.getDefault().getPathMatcher(
					"glob:"
							+ glob[i].replace("/", FileSystems.getDefault()
									.getSeparator()));
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

	public static String getStackTrace(Throwable t) {
		StringWriter out = new StringWriter();
		t.printStackTrace(new PrintWriter(out));
		return out.toString();
	}	
	
	public static String httpPost(String urlString, String content, String contentType) {
		StringBuilder result = new StringBuilder();
		try {
			URL url = new URL(urlString);
			HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			connection.setRequestProperty("Content-Type", contentType);
			connection.setDoOutput(true);
			try (OutputStream out = connection.getOutputStream()) {
				out.write(content.getBytes("UTF-8"));
			}
			int response = connection.getResponseCode();
			result.append(response);
			result.append("\n");
			try (Scanner in = new Scanner(connection.getInputStream(), "UTF-8")) {
				while (in.hasNextLine()) {
					result.append(in.nextLine());
					result.append("\n");
				}
			}
			catch (IOException e) {
			    InputStream err = connection.getErrorStream();
			    if (err == null) throw e;
			    try (Scanner in = new Scanner(err, "UTF-8")) {
			        result.append(in.nextLine());
			        result.append("\n");
			    }
			}			
		} catch (Throwable ex) {
			result.append(getStackTrace(ex));
		}
		return result.toString();		
	}
}
