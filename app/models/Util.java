package models;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;

import play.mvc.Http;

public class Util {
	private static Random generator = new Random();

	public static boolean isEmpty(String str) { return str == null || str.isEmpty(); }
	
	public static Path tail(Path p) {
		return p.subpath(1, p.getNameCount());
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
