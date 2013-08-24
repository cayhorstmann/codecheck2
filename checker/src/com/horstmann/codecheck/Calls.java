package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Calls {
    private List<String> argsList = new ArrayList<>();
    private Path file;
    private String name;
    private boolean isStatic;

    public String getName() {
        return name;
    }

    public Path getFile() {
        return file;
    }

    public int getSize() {
        return argsList.size();
    }

    public String getArgs(int i) {
        return argsList.get(i);
    }

    public void addCall(Path file, String args, String next) {
        if (this.file == null)
            this.file = file;
        else if (!this.file.equals(file))
            throw new RuntimeException("CALL in " + this.file + " and " + file);
        argsList.add(args);
        if (next.length() > 0) {
            if (name != null)
                throw new RuntimeException("Multiple methods tagged CALL in " + file);
            int i = next.indexOf("(");
            if (i == -1)
                throw new RuntimeException("No method below CALL in " + file);
            while (i > 0 && Character.isWhitespace(next.charAt(i - 1))) i--;
            if (i == 0)
                throw new RuntimeException("No method below CALL in " + file);
            int j = i;
            while (j > 0 && !Character.isWhitespace(next.charAt(j - 1))) j--;
            name = next.substring(j, i);
            isStatic = next.contains(" static ");
        }
    }

    public void writeTester(Path sourceDir, Path targetDir) throws IOException {
        String className = Util.javaClass(Util.tail(file));
        List<String> lines = Util.readLines(sourceDir.resolve(file));
        int i = 0;
        while (i < lines.size() && !lines.get(i).contains(className)) i++;
        if (i == lines.size())
            throw new RuntimeException("Can't find class " + className + " for inserting CALL in " + file);
        lines.set(i, lines.get(i).replace("class " + className, "class " + className + "CodeCheck"));
        i = lines.size() - 1;
        while (i >= 0 && !lines.get(i).trim().equals("}")) i--;
        if (i == -1)
            throw new RuntimeException("Can't find } for inserting CALL in " + file);
        // Insert main here
        lines.add(i++, "    public static void main(String[] args) ");
        lines.add(i++, "    {");
        if (!isStatic) {
            lines.add(i++, "        " + className + " obj1 = new " + className + "();");
            lines.add(i++, "        " + className + "CodeCheck obj2 = new " + className + "CodeCheck();");
        }
        for (int k = 0; k < argsList.size(); k++) {
            lines.add(i++, "        if (args[0].equals(\"" + (k + 1) + "\"))");
            lines.add(i++, "        {");
            lines.add(i++, "            Object expected = " + (isStatic ? "" : "obj2.") + name + "(" + argsList.get(k) + ");");
            lines.add(i++, "            System.out.println(expected);");
            lines.add(i++, "            Object actual = " + (isStatic ? className : "obj1") + "." + name + "(" + argsList.get(k) + ");");
            lines.add(i++, "            System.out.println(actual);");
            lines.add(i++, "            System.out.println(java.util.Objects.deepEquals(actual, expected));");
            lines.add(i++, "        }");
        }
        lines.add(i++, "    }");
        Files.write(targetDir.resolve(Util.javaPath(className + "CodeCheck")), lines, StandardCharsets.UTF_8);
    }
}
