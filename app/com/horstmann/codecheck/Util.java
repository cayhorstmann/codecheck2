package com.horstmann.codecheck;

import java.io.ByteArrayInputStream;
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
import java.net.URLConnection;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
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

    // Generics
    
    public static <T> Iterable<T> iterable(Iterator<T> iterator) { 
        return new Iterable<T>() { 
            public Iterator<T> iterator() { return iterator; } 
        }; 
    }
    
    // String handling

    public static <T> String join(Collection<T> items, String separator) {
        return items.stream().map(Object::toString).collect(Collectors.joining(separator));
    }
    // TODO if separator is "\n", add a terminal one
    public static <T> String join(T[] items, String separator) {
        return Stream.of(items).map(Object::toString).collect(Collectors.joining(separator));
    }
    
    public static boolean isEmpty(String str) { return str == null || str.isEmpty(); }
        
    public static List<String> lines(String contents) {
        return contents.lines().collect(Collectors.toList());
    }
    
    public static List<String> lines(byte[] contents) {
        return new String(contents, StandardCharsets.UTF_8).lines().collect(Collectors.toList());
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
    
    public static String truncate(String str, int maxlen) {
        if (str.length() > maxlen) return str.substring(0, maxlen - 3) + "...";
        else return str;
    }

    // Paths
    
    public static Path tail(Path p) {
        if (p.getNameCount() < 2) return p;
        return p.subpath(1, p.getNameCount());
    }
    
    public static String extension(Path path) {
        String name = path.toString();
        int n = name.lastIndexOf('.');
        if (n == -1)
            return "";
        else
            return name.substring(n + 1).toLowerCase();
    }

    public static String removeExtension(Path p) {
        String result = p.toString();
        int n = result.lastIndexOf(".");
        if (n >= 0) result = result.substring(0, n);
        return result;
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

    // Files    
    
    public static boolean sameContents(Path p1, Path p2) throws IOException {
        return Files.exists(p1)
                && Files.exists(p2)
                && Arrays.equals(Files.readAllBytes(p1), Files.readAllBytes(p2));
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
    
    public static List<String> readLines(Path path) {
        try {
            return java.nio.file.Files.readAllLines(path,
                    StandardCharsets.UTF_8);
        } catch (IOException ex) {
            return Collections.emptyList();
        }
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
    
    // Stack traces, process, etc.

    public static String getStackTrace(Throwable t) {
        StringWriter out = new StringWriter();
        t.printStackTrace(new PrintWriter(out));
        return out.toString();
    }
    
    public static String runProcess(String command, int millis) {
        try {
            Process process = Runtime.getRuntime().exec(command);
            boolean completed = process.waitFor(millis, TimeUnit.MILLISECONDS);

            String err = new String(process.getErrorStream().readAllBytes(), "UTF-8");
            process.getErrorStream().close();
            String out = new String(process.getInputStream().readAllBytes(), "UTF-8");
            process.getInputStream().close();
            StringBuilder result = new StringBuilder();
            result.append(err);
            if (!completed) {
                process.destroyForcibly();
                result.append("\nTimeout after " + millis + " milliseconds\n");
            }
            result.append(out);
            return result.toString();
        } catch (Exception ex) {
            return ex.getMessage();
        }
    }
    
    // Escaping
    
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
    
    // TreeMap<Path, byte[]>
    
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
            String name = entry.getName();
            if (!entry.isDirectory() && !name.startsWith("__MACOSX") && !name.endsWith(".DS_Store"))
                result.put(Paths.get(name), zin.readAllBytes());
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
                result.put(dir.relativize(file), Files.readAllBytes(file));
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
    
    // HTTP
    
    public static byte[] fileUpload(String urlString, String fieldName, String fileName, byte[] bytes) throws IOException {
        final int TIMEOUT = 90000; // 90 seconds
        String boundary = "===" + createPrivateUID() + "===";
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

    // TODO: What about redirects?
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
    
    // UIDs
    
    // TODO Consider using https://github.com/scru128/spec
    public static String createPrivateUID() {
        return new BigInteger(128, generator).toString(36).toUpperCase();               
    }
    
    public static String createPublicUID() {
        return datePrefix() + new BigInteger(128, generator).toString(36);      
    }

    private static String datePrefix() {
        return DateTimeFormatter.ofPattern("yyMMddkkmm").format(LocalDateTime.now());
    }
    private static String vowels = "aeiouy";
    private static String consonants = "bcdfghjklmnpqrstvwxz";

    public static String createPronouncableUID() {
        StringBuilder result = new StringBuilder();
        int len = 4;
        for (int i = 0; i < len; i++) {
            if (i > 0) result.append("-");
            result.append(generatePronounceableWord());
        }
        return result.toString();
    }
    
    // generates a non-bad four-letter pronounceable word
    // of the form vcvc or cvcv (c = consonant, v = vowel)
    private static StringBuilder generatePronounceableWord() { 
        StringBuilder word;
        int len = 4;
        int b = Util.generator.nextInt(2);
        do {
            word = new StringBuilder();
            for (int i = 0; i < len; i++) { 
                String s = i % 2 == b ? Util.consonants : vowels;
                int n = Util.generator.nextInt(s.length());
                word.append(s.charAt(n));
            }
        } while (isBadWord(word.toString())); // generate a word until we get a non bad word
        
        return word;
    }
    private static boolean isBadWord(String word){ 
        String[] filteredOutWords = {"anal", "anus", "anil", "anes", "anis", "babe", "bozo", "coky", "dick", "dike", "dyke", "homo", "lube", "nude", "oral", "rape", "sexy", "titi", "wily"};
        return Arrays.asList(filteredOutWords).contains(word);
    }

    public static boolean isPronouncableUID(String s) {
        return s.matches("(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})(-(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})){3}");
    }
}
