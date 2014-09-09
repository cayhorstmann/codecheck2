package com.horstmann.codecheck;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
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

    public static String read(Path path) {
        try {
            return new String(java.nio.file.Files.readAllBytes(path), "UTF-8");
        } catch (IOException ex) {
            return null;
        }
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

    public static String read(Path dir, Path file) {
        return read(dir.resolve(file));
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

    @Deprecated
    public static String htmlEscape(String s) {
        StringBuilder b = new StringBuilder();
        if (s == null)
            return "null";
        if (s.length() == 0)
            return "&#160;"; // for inclusion in lame table
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '<')
                b.append("&lt;");
            else if (c == '>')
                b.append("&gt;");
            else if (c == '&')
                b.append("&amp;");
            else if (c == ' ')
                b.append("&#160;"); // so that multiple spaces show up
            else
                b.append(c);
        }
        return b.toString();
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

    public static String runProcess(List<String> cmd, String input, int millis) {
        try {            
            ProcessBuilder builder = new ProcessBuilder(cmd);
            Path in = null;
            if (input != null) {
                in = Files.createTempFile("codecheck", "");
                Files.write(in, input.getBytes(StandardCharsets.UTF_8));
                builder.redirectInput(in.toFile());
            }
            Path out = Files.createTempFile("codecheck", "");
            builder.redirectErrorStream(true);
            builder.redirectOutput(out.toFile());
            Process process = builder.start();
            boolean completed = process.waitFor(millis, TimeUnit.MILLISECONDS);
            String result = new String(Files.readAllBytes(out), StandardCharsets.UTF_8);
            if (!completed) result += "\nTimeout after " + millis + " milliseconds\n";
            if (in != null) Files.delete(in);
            Files.delete(out);
            return result;
        } catch (Exception ex) {
            return getStackTrace(ex);
        }
    }
    
    public static String getStackTrace(Throwable t) {
        StringWriter out = new StringWriter();
        t.printStackTrace(new PrintWriter(out));
        return out.toString();
    }
}
