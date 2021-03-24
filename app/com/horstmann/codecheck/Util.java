package com.horstmann.codecheck;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

public class Util {
    private static Random generator = new Random();

    public static <T> String join(Collection<T> items, String separator) {
        return items.stream().map(Object::toString).collect(Collectors.joining(separator));
    }
    // TODO if separator is "\n", add a terminal one
    public static <T> String join(T[] items, String separator) {
        return Stream.of(items).map(Object::toString).collect(Collectors.joining(separator));
    }
    public static List<String> lines(String contents) {
        return contents.lines().collect(Collectors.toList());
    }
    
    public static List<String> lines(byte[] contents) {
    	return new String(contents, StandardCharsets.UTF_8).lines().collect(Collectors.toList());
    }
    
    public static String createPrivateUID() {
        return new BigInteger(128, generator).toString(36).toUpperCase();               
    }
    
    public static boolean sameContents(Path p1, Path p2) throws IOException {
        return Files.exists(p1)
                && Files.exists(p2)
                && Arrays.equals(Files.readAllBytes(p1), Files.readAllBytes(p2));
    }

    public static Path tail(Path p) {
        if (p.getNameCount() < 2) return p;
        return p.subpath(1, p.getNameCount());
    }
    
    public static String removeExtension(Path p) {
        String result = p.toString();
        int n = result.lastIndexOf(".");
        if (n >= 0) result = result.substring(0, n);
        return result;
    }
    
    public static Path createTempDirectory() throws IOException {
        return Files.createTempDirectory("codecheck");
    }
    
    public static Path createTempFile() throws IOException {
        return Files.createTempFile("codecheck", "");
    }
    

    public static String read(Path path) {
        try {
            String result = new String(java.nio.file.Files.readAllBytes(path), "UTF-8");
            result = result.replaceAll("\r", "");
            return result.endsWith("\n") ? result : result + "\n";
        } catch (IOException ex) {
            return null;
        }
    }
    
    public static void write(Path path, String contents) throws IOException {
        Files.write(path, contents.getBytes(StandardCharsets.UTF_8));
    }
    
    public static String readString(InputStream in) {
        try {
            return new String(readAllBytes(in), "UTF-8");
        } catch (IOException ex) {
            return null;
        }
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

    @SuppressWarnings("unchecked")
    public static List<String> readLines(Path path) {
        try {
            return java.nio.file.Files.readAllLines(path,
                    StandardCharsets.UTF_8);
        } catch (IOException ex) {
            return Collections.EMPTY_LIST;
        }
    }


    public static byte[] readBytes(Path path) {
        try {
            return java.nio.file.Files.readAllBytes(path);
        } catch (IOException ex) {
            return null;
        }
    }

    public static void deleteDirectory(Path start) throws IOException {
        if (start == null) 
            return;
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

    public static String getStackTrace(Throwable t) {
        StringWriter out = new StringWriter();
        t.printStackTrace(new PrintWriter(out));
        return out.toString();
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
	
    
    
    public static String unescapeJava(String s) {
        StringBuilder out = new StringBuilder();
        StringBuilder unicode = new StringBuilder(4);
        boolean hadSlash = false;
        boolean inUnicode = false;
        int sz = s.length();
        for (int i = 0; i < sz; i++) {
           char ch = s.charAt(i);
           if (inUnicode) {
               unicode.append(ch);
               if (unicode.length() == 4) {
                   out.append((char) Integer.parseInt(unicode.toString(), 16));
                   unicode.setLength(0);
                   inUnicode = false;
                   hadSlash = false;
               }
           }
           else if (hadSlash) {
               hadSlash = false;
               if (ch == 'u') inUnicode = true;
               else {
                  String from = "'\"\\rftnb";
                  String to = "'\"\\\r\f\t\n\b";
                 
                  boolean found = false;
                  for (int j = 0; !found && j < from.length(); j++) {
                      if (ch == from.charAt(j)) {
                          out.append(to.charAt(j));
                          found = true;
                      }
                  }
                  if (!found) out.append(ch);
               }
            } else if (ch == '\\') 
               hadSlash = true;
            else 
                out.append(ch);
        }
        if (hadSlash) out.append('\\'); 
        return out.toString();
    }
    
    public static String truncate(String str, int maxlen) {
        if (str.length() > maxlen) return str.substring(0, maxlen - 3) + "...";
        else return str;
    }

    /**
     * Yields all paths not matching a set of name patterns 
     * @param paths a set of paths
     * @param namePatterns A set of name patterns in glob syntax. CAUTION: Directory
     * patterns such as __pycache__/** are NOT supported. 
     * @return the paths whose file names don't match any of the given name patterns
     */
    public static Set<Path> filterNot(Set<Path> paths, String... namePatterns) {
        Set<Path> result = new TreeSet<>();
        PathMatcher[] matcher = new PathMatcher[namePatterns.length];
        for (int i = 0; i < matcher.length; i++)
            matcher[i] = FileSystems.getDefault().getPathMatcher(
                             "glob:" + namePatterns[i]);
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

    public static boolean matches(Path path, String... namePatterns) {
    	for (String glob : namePatterns) {
    		PathMatcher matcher = FileSystems.getDefault().getPathMatcher(
                                  "glob:" + glob.replace("/", FileSystems.getDefault().getSeparator()));
    		if (matcher.matches(path)) return true;
    	}
    	return false;
    }

    public static void copyAll(Collection<Path> paths, Path fromDir, Path toDir)  throws IOException {
        for (Path p : paths) {
            Path source = fromDir.resolve(p);
            Path target = toDir.resolve(p);
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
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
    
    public static byte[] zip(Map<Path, byte[]> contents) throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        ZipOutputStream zout = new ZipOutputStream(bout);
        for (Map.Entry<Path, byte[]> entry : contents.entrySet()) {
           ZipEntry ze = new ZipEntry(entry.getKey().toString());
           zout.putNextEntry(ze);
           zout.write(entry.getValue());
           zout.closeEntry();
        }
        zout.close();
        return bout.toByteArray();
    }
        
    static class FileMap extends TreeMap<Path, byte[]> {
        public String toString() {
            StringBuilder result = new StringBuilder();
            for (Map.Entry<Path, byte[]> entry : entrySet()) {
                if (result.length() == 0) result.append("{ "); else result.append(", ");
                result.append(entry.getKey());
                result.append(" -> ");
                try {
                    result.append(new String(entry.getValue(), StandardCharsets.UTF_8));
                } catch (Exception e) {
                    result.append("(binary, " + entry.getValue().length + " bytes)");
                }
            }
            if (result.length() == 0) result.append("{}");
            else result.append(" }");
            return result.toString();
        }
    }
    
    public static String getString(Map<Path, byte[]> fileMap, Path p) {
	   if (fileMap.containsKey(p)) 
		   return new String(fileMap.get(p), StandardCharsets.UTF_8);		   
	   else 
		   return "";
    }
	   
    public static void putString(Map<Path, byte[]> fileMap, Path p, String contents) {
    	fileMap.put(p, contents.getBytes(StandardCharsets.UTF_8));
    }    
    
    public static Map<Path, byte[]> unzip(byte[] bytes) throws IOException {
        Map<Path, byte[]> result = new FileMap(); 
        ZipInputStream zin = new ZipInputStream(new ByteArrayInputStream(bytes));
        ZipEntry entry;
        while ((entry = zin.getNextEntry()) != null)
        {
        	if (!entry.isDirectory())
        		result.put(Paths.get(entry.getName()), readAllBytes(zin));
            zin.closeEntry();
        }
        zin.close();
        return result;
    }
    
    public static Map<Path, byte[]> descendantFiles(Path dir) throws IOException {
        Map<Path, byte[]> result = new FileMap(); 
        if (dir == null || !Files.exists(dir))
            return result;
        Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file,
                    BasicFileAttributes attrs) throws IOException {
                result.put(dir.relativize(file), readBytes(file));
                return FileVisitResult.CONTINUE;
            }
        });
        return result;
    }
    
    public static Map<Path, String> descendantTextFiles(Path dir) throws IOException {
        Map<Path, String> result = new TreeMap<Path, String>(); 
        if (dir == null || !Files.exists(dir))
            return result;
        Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file,
                    BasicFileAttributes attrs) throws IOException {
                result.put(dir.relativize(file), read(file));
                return FileVisitResult.CONTINUE;
            }
        });
        return result;
    }
    
    public static byte[] fileUpload(String urlString, String fieldName, String fileName, byte[] bytes) throws IOException {
        final int TIMEOUT = 90000; // 90 seconds
        String boundary = "===" + Util.createPrivateUID() + "===";
        URL url = new URL(urlString);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setConnectTimeout(TIMEOUT);
        conn.setReadTimeout(TIMEOUT);
        conn.setUseCaches(false);
        conn.setDoOutput(true); 
        conn.setDoInput(true);
        conn.setRequestProperty("Content-Type",
                "multipart/form-data; boundary=" + boundary);
        OutputStream out = conn.getOutputStream();
        StringBuilder builder = new StringBuilder();
        builder.append("--").append(boundary)
            .append("\r\n")
            .append("Content-Disposition: form-data; name=\"").append(fieldName)
            .append("\"; filename=\"").append(fileName).append("\"")
            .append("\r\n")
            .append("Content-Type: ")
            .append(URLConnection.guessContentTypeFromName(fileName))
            .append("\r\n")
            .append("Content-Transfer-Encoding: binary")
            .append("\r\n")
            .append("\r\n");
        out.write(builder.toString().getBytes(StandardCharsets.UTF_8));
        out.write(bytes);
        builder.delete(0,  builder.length());
        builder.append("\r\n\r\n--").append(boundary).append("--\r\n");
        out.write(builder.toString().getBytes(StandardCharsets.UTF_8));
        out.close();
        int status = conn.getResponseCode();
        if (status == HttpURLConnection.HTTP_OK) {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            InputStream in = conn.getInputStream();
            int bytesRead = -1;
            byte[] buffer = new byte[1024];
            while ((bytesRead = in.read(buffer)) != -1) {
                bout.write(buffer, 0, bytesRead);
            }
            in.close();
            bout.close();
            conn.disconnect();
            return bout.toByteArray();
        } else {
            conn.disconnect();
            throw new IOException("Status: " + status);
        }        
    }
}
