package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;

public class Util {
    public static boolean sameContents(Path p1, Path p2) throws IOException {
        return Files.exists(p1)
                && Files.exists(p2)
                && Arrays
                        .equals(Files.readAllBytes(p1), Files.readAllBytes(p2));
    }

    public static Path tail(Path p) {
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
            return new String(java.nio.file.Files.readAllBytes(path), "UTF-8");
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

    public static int runProcess(List<String> cmd, String input, int millis, StringBuilder output, int maxOutputLength) {
        try {
            Path out = Util.createTempFile();
            Path in = null;
            try {            
                ProcessBuilder builder = new ProcessBuilder(cmd);
                if (input != null && input.length() > 0) {
                    in = Util.createTempFile();
                    Files.write(in, input.getBytes(StandardCharsets.UTF_8));
                    builder.redirectInput(in.toFile());
                }
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
                if (!completed) result += "\nTimeout after " + millis + " milliseconds\n";
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
        Object obj = new Util();
        for (URL url : ((URLClassLoader) obj.getClass().getClassLoader()).getURLs()) {
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
        String home = System.getProperty("com.horstmann.codecheck.home");
        return home == null ? null : Paths.get(home);        
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
}
