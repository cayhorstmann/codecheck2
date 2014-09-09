package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Calls {
    private Language language;
    private List<String> argsList = new ArrayList<>();
    private Path file;
    private String name;
    private List<String> modifiers;

    public Calls(Language language) {
        this.language = language;
    }

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
                throw new RuntimeException("Multiple methods tagged CALL in "
                        + file);
            
            name = language.functionName(next);
            
            if (name == null)
                throw new RuntimeException("No method below CALL in " + file);
            modifiers = language.modifiers(next);
        }
    }

    public void writeTester(Path sourceDir, Path targetDir) throws IOException {
        language.writeTester(sourceDir, targetDir, file, modifiers, name,
                argsList);
    }
}
