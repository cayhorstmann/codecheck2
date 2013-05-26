package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
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

public class Util {
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

    public static boolean sameContents(Path p1, Path p2) throws IOException {
        return Files.exists(p1) && Files.exists(p2) && Arrays.equals(Files.readAllBytes(p1), Files.readAllBytes(p2));
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
            return java.nio.file.Files.readAllLines(path, StandardCharsets.UTF_8);
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
    public static Set<Path> getDescendantFiles(final Path dir) throws IOException {
        final Set<Path> result = new TreeSet<>();
        if (dir == null || !Files.exists(dir))
            return result;
        Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                result.add(dir.relativize(file));
                return FileVisitResult.CONTINUE;
            }
        });
        return result;
    }

    public static Set<Path> getDescendantFiles(Path dir, List<String> subdirs) throws IOException {
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

    public static boolean isMain(Path dir, Path p) {
        if (!p.toString().endsWith(".java"))
            return false;
        String contents = Util.read(dir, p);
        return contents != null && contents.contains("public static void main(String[] args)");
    }

    public static byte[] readBytes(Path path) {
        try {
            return java.nio.file.Files.readAllBytes(path);
        } catch (IOException ex) {
            return null;
        }
    }
}
