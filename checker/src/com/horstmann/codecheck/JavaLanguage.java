package com.horstmann.codecheck;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.AccessControlException;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

public class JavaLanguage implements Language {
    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isSource(java.nio.file.Path)
     */
    @Override
    public boolean isSource(Path p) {
        return p.toString().endsWith(".java");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isTester(java.lang.String)
     */
    @Override
    public boolean isTester(String modulename) {
        return modulename != null && modulename.matches(".*Tester[0-9]*");
    }

    private static Pattern mainPattern = Pattern
            .compile("public\\s+static\\s+void\\s+main\\s*\\(\\s*String(\\s*\\[\\s*\\]\\s*\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*|\\s+\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*\\s*\\[\\s*\\])\\s*\\)");

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#isMain(java.nio.file.Path,
     * java.nio.file.Path)
     */
    @Override
    public boolean isMain(Path dir, Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(dir, p);
        return contents != null && mainPattern.matcher(contents).find();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#moduleOf(java.nio.file.Path)
     */
    @Override
    public String moduleOf(Path path) {
        String name = path.toString();
        if (!name.endsWith(".java"))
            return null;
        name = name.substring(0, name.length() - 5); // drop .java
        return name.replace(FileSystems.getDefault().getSeparator(), ".");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#pathOf(java.lang.String)
     */
    @Override
    public Path pathOf(String moduleName) {
        Path p = FileSystems.getDefault().getPath("", moduleName.split("[.]"));
        Path parent = p.getParent();
        if (parent == null)
            return FileSystems.getDefault().getPath(moduleName + ".java");
        else
            return parent.resolve(p.getFileName().toString() + ".java");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#compile(java.lang.String,
     * java.nio.file.Path, com.horstmann.codecheck.Report)
     */
    @Override
    public boolean compile(String modulename, Path dir, Report report) {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        OutputStream outStream = new ByteArrayOutputStream();
        OutputStream errStream = new ByteArrayOutputStream();
        int result = compiler.run(null, outStream, errStream, "-sourcepath",
                dir.toString(), "-d", dir.toString(),
                dir.resolve(pathOf(modulename)).toString());
        if (result != 0) {
            String errorReport = errStream.toString();
            if (errorReport.trim().equals(""))
                report.output(null, "Error compiling " + modulename);
            else
                report.error("Compiler error", errorReport);
        }
        return result == 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#run(java.lang.String,
     * java.nio.file.Path, java.lang.String, java.lang.String, int)
     */
    @Override
    @SuppressWarnings("deprecation")
    public String run(final String mainclass, final Path classpathDir,
            String args, String input, int timeoutMillis) throws IOException,
            ReflectiveOperationException {
        InputStream oldIn = System.in;
        PrintStream oldOut = System.out;
        PrintStream oldErr = System.err;
        if (input == null)
            input = "";
        final ByteArrayOutputStream newOut = new ByteArrayOutputStream();
        final PrintStream newOutPrint = new PrintStream(newOut);
        System.setIn(new ByteArrayInputStream(input.getBytes("UTF-8")) {
            public int available() {
                return 0;
            }

            public int read() {
                int c = super.read();
                if (c != -1) {
                    newOut.write((char) c);
                }
                return c;
            }

            public int read(byte[] b) {
                return read(b, 0, b.length);
            }

            public int read(byte[] b, int off, int len) {
                // int r = super.read(b, off, len);
                if (len == 0 || off >= b.length)
                    return 0;
                int r = 0;
                int c = super.read();
                if (c == -1)
                    return -1;
                boolean done = false;
                while (!done) {
                    b[off + r] = (byte) c;
                    r++;
                    if (c == '\n')
                        done = true;
                    else {
                        c = super.read();
                        if (c == -1)
                            done = true;
                    }
                }
                if (r != -1) {
                    newOut.write(b, off, r);
                }
                return r;
            }
        });

        String result = "";
        System.setOut(newOutPrint);
        System.setErr(newOutPrint);
        final URLClassLoader loader = new URLClassLoader(
                new URL[] { classpathDir.toFile().toURI().toURL() });
        try {

            final AtomicBoolean done = new AtomicBoolean(false);

            final String[] argsArray;
            if (args == null || args.trim().equals(""))
                argsArray = new String[0];
            else
                argsArray = args.trim().split("\\s+");
            loader.setDefaultAssertionStatus(true);

            final Thread mainmethodThread = new Thread() {
                public void run() {
                    try {
                        Class<?> klass = loader.loadClass(mainclass);
                        final Method mainmethod = klass.getMethod("main",
                                String[].class);
                        mainmethod.invoke(null, (Object) argsArray);
                    } catch (InvocationTargetException ex) {
                        Throwable cause = ex.getCause();
                        if (cause instanceof AccessControlException && cause.getMessage().equals("access denied (\"java.lang.RuntimePermission\" \"exitVM.0\")")) {
                            // do nothing
                        } else if (cause == null)
                            ex.printStackTrace(newOutPrint);
                        else
                            cause.printStackTrace(newOutPrint);
                    } catch (Throwable t) {
                        t.printStackTrace(newOutPrint);
                    }
                    done.set(true);
                }
            };

            mainmethodThread.start();

            try {
                mainmethodThread.join(timeoutMillis);
            } catch (InterruptedException e) {
            }
            result = newOut.toString("UTF-8");
            if (!done.get()) {
                if (!result.endsWith("\n"))
                    result += "\n";
                result += "Timed out after "
                        + (timeoutMillis >= 2000 ? timeoutMillis / 1000
                                + " seconds" : timeoutMillis + " milliseconds");
                mainmethodThread.stop();
            }
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
            System.setErr(oldErr);
            // System.setSecurityManager(null);
            loader.close();
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#writeTester(java.nio.file.Path,
     * java.nio.file.Path, java.nio.file.Path, java.util.List, java.lang.String,
     * java.util.List)
     */
    @Override
    public void writeTester(Path sourceDir, Path targetDir, Path file,
            List<String> modifiers, String name, List<String> argsList)
            throws IOException {
        String className = moduleOf(Util.tail(file));
        boolean isStatic = modifiers.contains("static");
        List<String> lines = Util.readLines(sourceDir.resolve(file));
        int i = 0;
        while (i < lines.size() && !lines.get(i).contains(className))
            i++;
        if (i == lines.size())
            throw new RuntimeException("Can't find class " + className
                    + " for inserting CALL in " + file);
        lines.set(
                i,
                lines.get(i).replace("class " + className,
                        "class " + className + "CodeCheck"));
        i = lines.size() - 1;
        while (i >= 0 && !lines.get(i).trim().equals("}"))
            i--;
        if (i == -1)
            throw new RuntimeException("Can't find } for inserting CALL in "
                    + file);
        // Insert main here
        lines.add(i++, "    public static void main(String[] args) ");
        lines.add(i++, "    {");
        if (!isStatic) {
            lines.add(i++, "        " + className + " obj1 = new " + className
                    + "();");
            lines.add(i++, "        " + className + "CodeCheck obj2 = new "
                    + className + "CodeCheck();");
        }
        for (int k = 0; k < argsList.size(); k++) {
            lines.add(i++, "        if (args[0].equals(\"" + (k + 1) + "\"))");
            lines.add(i++, "        {");
            lines.add(i++, "            Object expected = "
                    + (isStatic ? "" : "obj2.") + name + "(" + argsList.get(k)
                    + ");");
            lines.add(i++,
                    "            System.out.println(_toString(expected));");
            lines.add(i++, "            Object actual = "
                    + (isStatic ? className : "obj1") + "." + name + "("
                    + argsList.get(k) + ");");
            lines.add(i++, "            System.out.println(_toString(actual));");
            lines.add(
                    i++,
                    "            System.out.println(java.util.Objects.deepEquals(actual, expected));");
            lines.add(i++, "        }");
        }
        lines.add(i++, "    }");
        lines.add(i++, "    private static String _toString(Object obj)");
        lines.add(i++, "    {");
        lines.add(i++, "      if (obj == null) return \"null\";");
        lines.add(i++, "      if (obj instanceof Object[])");
        lines.add(i++,
                "         return java.util.Arrays.deepToString((Object[]) obj);");
        lines.add(i++, "      if (obj.getClass().isArray())");
        lines.add(
                i++,
                "         try { return (String) java.util.Arrays.class.getMethod(\"toString\", obj.getClass()).invoke(null, obj); }");
        lines.add(i++, "         catch (Exception ex) {}");
        lines.add(i++, "      return obj.toString();");
        lines.add(i++, "    }");

        // expected == null ? null : expected instanceof Object[] ?
        // java.util.Arrays.deepToString((Object[]) expected) :
        // expected.getClass().isArray() ? java.util.Arrays.toString(expected) :
        // expected
        Files.write(targetDir.resolve(pathOf(className + "CodeCheck")), lines,
                StandardCharsets.UTF_8);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#pseudoCommentDelimiters()
     */
    @Override
    public String[] pseudoCommentDelimiters() {
        return new String[] { "//", "" };
    }

    private static String patternString = ".*\\S\\s+(\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)\\s*=\\s*([^\\s;]+)\\s*;.*";
    private static Pattern pattern = Pattern.compile(patternString);

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#variablePattern()
     */
    @Override
    public Pattern variablePattern() {
        return pattern;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.horstmann.codecheck.Language#substitutionSeparator()
     */
    @Override
    public String substitutionSeparator() {
        return ";";
    }
}
