package models;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
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
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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

import play.Logger;
import play.mvc.Http;

public class Util {
	private static Random generator = new Random();
	private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");

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

	public static String base64(Path dir, String fileName) {
		try {
			Path file = dir.resolve(fileName);
			if (!Files.exists(file)) return null;
			Base64.Encoder encoder = Base64.getEncoder();
			return encoder.encodeToString(java.nio.file.Files
				.readAllBytes(file));
		} catch (IOException e) {
			return null;
		}
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
		String prefix = datePrefix();
		Set<PosixFilePermission> perms = PosixFilePermissions
				.fromString("rwxr-xr-x");
		return java.nio.file.Files.createTempDirectory(parent, prefix,
				PosixFilePermissions.asFileAttribute(perms));
	}

	public static String createPublicUID() {
		return datePrefix() + new BigInteger(128, generator).toString(36);		
	}
	
	public static String createPrivateUID() {
		return new BigInteger(128, generator).toString(36).toUpperCase();		
	}
	
	private static String datePrefix() {
		return DateTimeFormatter.ofPattern("yyMMddkkmm").format(LocalDateTime.now());
	}
	
	private static String vowels = "aeiouy";
	private static String consonants = "bcdfghjklmnpqrstvwxz";
	public static String createPronouncableUID() {
		StringBuilder result = new StringBuilder();
		int len = 16;
		int b = generator.nextInt(2);
		for (int i = 0; i < len; i++) {
			String s = i % 2 == b ? consonants : vowels;
			int n = generator.nextInt(s.length());
			result.append(s.charAt(n));
			if (i % 4 == 3 && i < len - 1) {
				result.append('-');
				b = generator.nextInt(2);
			}
		}
		return result.toString();
	}
	
	public static boolean isPronouncableUID(String s) {
		return s.matches("(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})(-(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})){3}");
	}

	public static void deleteDirectory(Path start) throws IOException {
		if (start == null) return;
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

	/*
	 * Gets all files contained in the given directory.
	 * @return a set of of files, as Path objects that are relativized against
	 * dir 
	 */
	public static SortedSet<Path> getChildren(Path dir) throws IOException {
		if (dir == null || !Files.exists(dir) || !Files.isDirectory(dir))
			return Collections.emptySortedSet();
		else
			return Files.list(dir).filter(Files::isRegularFile).map(dir::relativize).collect(Collectors.toCollection(TreeSet::new));
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
	
    public static String getParam(Map<String, String[]> params, String key) {
		String[] values = params.get(key);
		if (values == null || values.length == 0) return null;
		else return values[0];
	}
    
    public static String paramsToString(Map<String, String[]> params) {
    	if (params == null) return "null";
    	StringBuilder result = new StringBuilder();
    	result.append("{");
    	for (String key : params.keySet()) {
    		if (result.length() > 1) result.append(", ");
    		result.append(key); 
    		result.append("=");
    		result.append(Arrays.toString(params.get(key)));
    	}
    	result.append("}");
    	return result.toString();
    }    
    
	/**
	 * Yields a map of query parameters in a HTTP URI
	 * @param url the HTTP URL
	 * @return the map of query parameters or an empty map if there are none
	 * For example, if uri is http://fred.com?name=wilma&passw%C3%B6rd=c%26d%3De
	 * then the result is { "name" -> "wilma", "passwörd" -> "c&d=e" }
	 */
	public static Map<String, String> getParams(String url)
	{		
		// https://www.talisman.org/~erlkonig/misc/lunatech%5Ewhat-every-webdev-must-know-about-url-encoding/
		Map<String, String> params = new HashMap<>();
		String rawQuery;
		try {
			rawQuery = new URI(url).getRawQuery();
			if (rawQuery != null) {
				for (String kvpair : rawQuery.split("&"))
				{
					int n = kvpair.indexOf("=");
					params.put(
						URLDecoder.decode(kvpair.substring(0, n), "UTF-8"), 
						URLDecoder.decode(kvpair.substring(n + 1), "UTF-8"));
				}
			}
		} catch (UnsupportedEncodingException e) {
			// UTF-8 is supported
		} catch (URISyntaxException e1) {
			// Return empty map
		}
		return params;
	}

	public static String prefix(Http.Request request) {
	 	boolean secure = request.secure() || request.getHeaders().getAll("X-Forwarded-Proto").contains("https");
	 	/*
	 	   One shouldn't have to do this, but with Google Cloud, X-Forwarded-For has two entries (e.g. [95.90.234.41, 130.211.33.19])
	 	   and X-Forwarded-Proto has one ([https]). From 
	 	   https://github.com/playframework/playframework/blob/814f0c73f86eb0e85bcae7f2167c73a08fed9fd7/transport/server/play-server/src/main/scala/play/core/server/common/ForwardedHeaderHandler.scala
	 	   line 204, Play doesn't conclude that the connection was secure. 
	 	 */		
		return (secure ? "https://" : "http://") + request.host();
	}

	public static boolean exists(String url) {
		boolean result = false;
		try {
			HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
			try {
				conn.connect();
				result = conn.getHeaderField(null).contains("200");
			} finally {
				conn.disconnect();
			}
		} catch (Exception ex) {
		}
		return result;
	}

	public static <T> Iterable<T> iterable(Iterator<T> iterator) { 
	    return new Iterable<T>() { 
	        public Iterator<T> iterator() { return iterator; } 
	    }; 
	}
	
	public static byte[] readAllBytes(InputStream in) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		copy(in, out);
		out.close();
		return out.toByteArray();
	}
	
	public static void copy(InputStream in, OutputStream out) throws IOException {
		final int BLOCKSIZE = 1024;
		byte[] bytes = new byte[BLOCKSIZE];
		int len;
		while ((len = in.read(bytes)) != -1) out.write(bytes, 0, len);
	}
}
