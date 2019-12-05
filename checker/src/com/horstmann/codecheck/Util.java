package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
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
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;

public class Util {
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
        // Don't do it in /tmp, or the Java tools will fail on Cygwin 
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

    @SuppressWarnings("unchecked")
    public static List<String> readLines(Path path) {
        try {
            return java.nio.file.Files.readAllLines(path,
                    StandardCharsets.UTF_8);
        } catch (IOException ex) {
            return Collections.EMPTY_LIST;
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

    public static byte[] readBytes(Path path) {
        try {
            return java.nio.file.Files.readAllBytes(path);
        } catch (IOException ex) {
            return null;
        }
    }

    public static void deleteDirectory(Path start) throws IOException {
        if (start == null || Main.DEBUG) 
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

    public static int runProcess(List<String> cmd, String input, int millis, StringBuilder output, int maxOutputLength) {
        try {
            Path out = Util.createTempFile();
            Path in = null;
            try {            
                ProcessBuilder builder = new ProcessBuilder(cmd);
                if (input == null) input = "";
                in = Util.createTempFile();
                Files.write(in, input.getBytes(StandardCharsets.UTF_8));
                /*
                 * TODO: It's weird that we write the input to a file and then, in runprog/
                 * interleaveio, read it from stdin a line at a time. Maybe we should just
                 * make a special case for runprog and give it the file?
                 */
                builder.redirectInput(in.toFile());
                builder.redirectErrorStream(true);
                builder.redirectOutput(out.toFile());
                Process process = builder.start();
                boolean completed = process.waitFor(millis, TimeUnit.MILLISECONDS);
                int exitValue = completed ? process.exitValue() : -1;
                String result;
                if (Files.size(out) > maxOutputLength) {
                    char[] chars = new char[maxOutputLength];
                    int n = Files.newBufferedReader(out).read(chars);
                    result = new String(chars, 0, n) + "\n...\nRemainder truncated\n";
                }
                else result = new String(Files.readAllBytes(out), StandardCharsets.UTF_8);
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
    
    public static String getStackTrace(Throwable t) {
        StringWriter out = new StringWriter();
        t.printStackTrace(new PrintWriter(out));
        return out.toString();
    }
    
    public static Path getHomeDir() {
        String home = System.getProperty("com.horstmann.codecheck.home");
        if (home != null) return Paths.get(home);
        home = System.getenv("CODECHECK_HOME");
        if (home != null) return Paths.get(home);
        for (String p : System.getProperty("java.class.path").split(System.getProperty("path.separator"))) {
            if (p.endsWith("codecheck.jar")) {
                return Paths.get(p).getParent();
            }
            // TODO: If p ends with *, enumerate all JAR files?
            // TODO: What if we make a module? 
        }
        
        Object obj = new Util();
        ClassLoader loader = obj.getClass().getClassLoader();
        if (loader instanceof URLClassLoader) { // TODO: Not true in Java 9. Retire this fallback when Java 8 is EOL
            for (URL url : ((URLClassLoader) loader).getURLs()) {
                String urlString = url.toString();
                if (urlString.startsWith("file:") && urlString.endsWith("codecheck.jar")) {
                    try {
                        return Paths.get(new URL(urlString).toURI()).getParent();
                        // This works with file:///C:/... URLs in Windows
                    } catch (MalformedURLException | URISyntaxException e) {
                        // We tried
                    }
                }
            }
        }
        throw new CodeCheckException("Cannot find codecheck home. Set CODECHECK_HOME.");        
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

    // TODO: Should be in Util
    public static boolean matches(Path path, String glob) {
        PathMatcher matcher = FileSystems.getDefault().getPathMatcher(
                                  "glob:" + glob.replace("/", FileSystems.getDefault().getSeparator()));
        return matcher.matches(path);
    }

    // TODO: Should be in Util
    public static Set<Path> filter(Set<Path> paths, String glob) {
        Set<Path> result = new TreeSet<>();
        for (Path p : paths)
            if (matches(p.getFileName(), glob))
                result.add(p);
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

    // TODO: Should be in Util
    public static void copyAll(Collection<Path> paths, Path fromDir, Path toDir)  throws IOException {
        for (Path p : paths) {
            Path source = fromDir.resolve(p);
            Path target = toDir.resolve(p);
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        }        
    }
}
