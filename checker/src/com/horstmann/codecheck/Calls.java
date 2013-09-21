package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Calls {
	private Language language;
    private List<String> argsList = new ArrayList<>();
    private Path file;
    private String name;
    private boolean isStatic;      

    public Calls(Language language) {
		super();
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
    	@SuppressWarnings("unchecked")
		List<String> modifiers = isStatic ? Collections.singletonList("static") : Collections.EMPTY_LIST;
    	language.writeTester(sourceDir, targetDir, file, modifiers, name, argsList);
    }        
}
