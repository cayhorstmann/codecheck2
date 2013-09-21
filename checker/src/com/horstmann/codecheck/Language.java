package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Pattern;

public interface Language {

	boolean isSource(Path p);

	boolean isTester(String modulename);

	boolean isMain(Path dir, Path p);

	String moduleOf(Path path);

	Path pathOf(String moduleName);

	boolean compile(String modulename, Path dir, Report report);

	/**
	 * Runs a Java program
	 *
	 * @param mainclass
	 * @param classpathDir
	 * @param input
	 * @return an array of two strings, holding the program output and errors
	 * @throws IOException
	 * @throws ReflectiveOperationException
	 */
	String run(String mainclass, Path classpathDir, String args, String input,
			int timeoutMillis) throws IOException, ReflectiveOperationException;

	void writeTester(Path sourceDir, Path targetDir, Path file,
			List<String> modifiers, String name, List<String> argsList)
			throws IOException;

	String[] pseudoCommentDelimiters();

	Pattern variablePattern();

	String substitutionSeparator();

}