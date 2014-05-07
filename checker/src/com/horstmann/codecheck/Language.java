package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public interface Language {

	boolean isSource(Path p);

	boolean isTester(String modulename);
	
	boolean isUnitTest(String modulename);

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
	
	void runUnitTest(String modulename, Path dir, Report report, Score score);
	
	/**
	 * Accepts an optional file for processing (such as checkstyle.xml)
	 * @param file the file to check for optional processing
	 * @param dir the directory containing the student files
	 * @param studentFiles all student files
	 * @param report the report to add to, if an action was taken
	 * @param score the score object to reward
	 * @return true if an action was taken
	 */
	boolean accept(Path file, Path dir, Set<Path> studentFiles, Report report, Score score); // TODO: Make an object describing the problem
}