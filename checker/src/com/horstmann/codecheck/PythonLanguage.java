package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public class PythonLanguage implements Language {

	public PythonLanguage() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean isSource(Path p) {
		return p.toString().endsWith(".py");
	}

	@Override
	public boolean isTester(String modulename) {
		return modulename != null && modulename.matches(".*Tester[0-9]*");
	}

    private static Pattern mainPattern = Pattern.compile("def\\s+main\\s*:");
    
    /* (non-Javadoc)
	 * @see com.horstmann.codecheck.Language#isMain(java.nio.file.Path, java.nio.file.Path)
	 */
    @Override
	public boolean isMain(Path dir, Path p) {
        if (!isSource(p))
            return false;
        String contents = Util.read(dir, p);
        return contents != null && mainPattern.matcher(contents).find();
    }

	@Override
	public String moduleOf(Path path) {
        String name = path.toString();
        if (!name.endsWith(".py"))
            return null;
        return name.substring(0, name.length() - 3); // drop .py
	}

	@Override
	public Path pathOf(String moduleName) {
		Path p = FileSystems.getDefault().getPath("", moduleName);
        Path parent = p.getParent();
        if (parent == null)
            return FileSystems.getDefault().getPath(moduleName + ".py");
        else
            return parent.resolve(p.getFileName().toString() + ".py");
	}

	@Override
	public boolean compile(String modulename, Path dir, Report report) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String run(String mainclass, Path classpathDir, String args,
			String input, int timeoutMillis) throws IOException,
			ReflectiveOperationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void writeTester(Path sourceDir, Path targetDir, Path file,
			List<String> modifiers, String name, List<String> argsList)
			throws IOException {
		// TODO Auto-generated method stub

	}

    @Override
	public String[] pseudoCommentDelimiters() { return new String[] { "##", "" }; }
    
    private static String patternString = "\\s*([A-Za-z][A-Za-z0-9]*)\\s*=\\s*(.+)";
    private static Pattern pattern = Pattern.compile(patternString);
    
    /* (non-Javadoc)
	 * @see com.horstmann.codecheck.Language#variablePattern()
	 */
    @Override
	public Pattern variablePattern() { return pattern; }
    
    /* (non-Javadoc)
	 * @see com.horstmann.codecheck.Language#substitutionSeparator()
	 */
    @Override
	public String substitutionSeparator() { return ";"; }
    
    @Override
    public boolean accept(Path file, Path dir, Set<Path> studentFiles,
        Report report, Score score) {
        return false;
    }
}
