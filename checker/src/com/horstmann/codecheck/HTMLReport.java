package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

import javax.imageio.ImageIO;

public class HTMLReport implements Report {
	private StringBuilder builder;
	private Path dir;

	// TODO: Directory
	public HTMLReport(String title, Path outputDir) {
		dir = outputDir;
		builder = new StringBuilder();
		// Internal HTML can't deal with this
		// TODO: Better property name? Comes from Runner
		if (!"false".equals(System.getProperty("labrat.img.inline")))
			builder.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
		builder.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
		builder.append("<html xmlns=\"http://www.w3.org/1999/xhtml\"><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" /><title>");
		escape(title);
		builder.append("</title></head><body>");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#header(java.lang.String)
	 */
	@Override
	public HTMLReport header(String text) {
		builder.append("<p><b>");
		escape(text);
		builder.append("</b></p>\n");
		return this;
	}

	private HTMLReport caption(String text) {
		if (text != null && !text.trim().equals("")) {
			builder.append("<p>");
			escape(text);
			builder.append(":</p>");
		}
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#output(java.lang.String)
	 */
	@Override
	public HTMLReport output(CharSequence text) {
		output(null, text);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#output(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public HTMLReport output(String captionText, CharSequence text) {
		if (text == null || text.equals(""))
			return this;
		caption(captionText);
		builder.append("<pre>");
		escape(text);
		builder.append("</pre>\n");
		return this;
	}

	@Override
	public HTMLReport output(
			List<String> lines, Set<Integer> matches, Set<Integer> mismatches) {
		builder.append("<pre>");
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			if (matches.contains(i)) green(line);
			else if (mismatches.contains(i)) red(line);
			else escape(line);
			builder.append("\n");
 		}
		builder.append("</pre>\n");
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#error(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public HTMLReport error(String captionText, String message) {
		caption(captionText);
		output(message);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#error(java.lang.String)
	 */
	@Override
	public HTMLReport error(String message) {
		error("Error", message);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#systemError(java.lang.String)
	 */
	@Override
	public HTMLReport systemError(String message) {
		red("System Error:");
		output(message);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#systemError(java.lang.Throwable)
	 */
	@Override
	public HTMLReport systemError(Throwable t) {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		PrintStream pout = new PrintStream(out);
		t.printStackTrace(pout);
		systemError(out.toString());
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#image(java.lang.String,
	 * java.nio.file.Path)
	 */
	@Override
	public HTMLReport image(String captionText, Path file) {
		caption(captionText);
		image(file);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#image(java.nio.file.Path)
	 */
	@Override
	public HTMLReport image(Path file) {
		builder.append("<p>");
		if ("false".equals(System.getProperty("labrat.img.inline"))) { // TODO: Legacy
			builder.append("<img src=\"");
			builder.append(file.toUri().toString());
			builder.append("\">");
		} else
			try {
				String data = new sun.misc.BASE64Encoder().encode(Files
						.readAllBytes(file));
				builder.append("<img alt=\"screen capture\" src=\"data:image/png;base64,");
				builder.append(data);
				builder.append("\"/>");
			} catch (IOException ex) {
				error("No image");
			}
		builder.append("</p>");
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#image(java.lang.String, byte[])
	 */
	@Override
	public HTMLReport image(String captionText, BufferedImage img) {
		caption(captionText);
		image(img);
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#image(byte[])
	 */
	@Override
	public HTMLReport image(BufferedImage img) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try {
            ImageIO.write(img, "PNG", out);
            out.close();
            byte[] pngBytes = out.toByteArray();
			String data = new sun.misc.BASE64Encoder().encode(pngBytes);
			builder.append("<p>");
			builder.append("<img alt=\"screen capture\" src=\"data:image/png;base64,");
			builder.append(data);
			builder.append("\"/>");
			builder.append("</p>");
        } catch (IOException e) {
        	error("No image");
        }
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#file(java.nio.file.Path,
	 * java.nio.file.Path)
	 */
	@Override
	public HTMLReport file(Path dir, Path file) {
		caption(file.toString());
		Path source = dir.resolve(file);
		boolean lineNumbers = file.toString().endsWith(".java"); // TODO:
																	// Arbitrary
		if (Files.exists(source)) {
			try {
				List<String> lines = Files.readAllLines(source,
						Charset.forName("UTF-8"));
				if (lineNumbers) {
					builder.append("<table><tr><td style=\"padding-right: 1em;\"><pre>");
					for (int i = 1; i <= lines.size(); i++) {
						builder.append(String.format(
								"<span style=\"color: gray;\">%3d</span>\n", i));
					}
					builder.append("</pre></td><td>");
				}
				builder.append("<pre>");
				for (String line : lines) {
					escape(line);
					builder.append("\n");
				}
				builder.append("</pre>");
				if (lineNumbers) {
					builder.append("</td></tr></table>");
				}
			} catch (IOException e) {
				systemError(e);
			}

		} else {
			error("Not found");
		}
		return this;
	}

	private HTMLReport escape(CharSequence s) {
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (c == '<')
				builder.append("&lt;");
			else if (c == '>')
				builder.append("&gt;");
			else if (c == '&')
				builder.append("&amp;");
			else
				builder.append(c);
		}
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#add(com.horstmann.codecheck.Score)
	 */
	@Override
	public HTMLReport add(Score score) {
		header("Score");
		builder.append("<p>");
		builder.append("" + score);
		builder.append("</p>");
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.horstmann.codecheck.Report#save(java.nio.file.Path)
	 */
	@Override
	public HTMLReport save(String problemId, String out) throws IOException {
		Path outPath = dir.resolve(out + ".html");
		builder.append("<p><a href=\"" + problemId + ".jar\">Download</a>");
		builder.append("</body></html>\n");
		Files.write(outPath, builder.toString().getBytes());
		return this;
	}

	private HTMLReport tableStart() {
		builder.append("<table style=\"font-size: 0.9em;\">\n");
		return this;
	}

	private HTMLReport tableEnd() {
		builder.append("</table>\n");
		return this;
	}

	private HTMLReport rowStart() {
		builder.append("<tr>");
		return this;
	}

	private HTMLReport rowEnd() {
		builder.append("</tr>\n");
		return this;
	}

	private HTMLReport cellStart() {
		builder.append("<td style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">");
		return this;
	}

	private HTMLReport cellEnd() {
		builder.append("</td>\n");
		return this;
	}

	private HTMLReport cell(CharSequence text) {
		cellStart();
		if (text.length() == 0)
			builder.append("&nbsp;");
		else
			escape(text);
		cellEnd();
		return this;
	}

	private HTMLReport headerCell(CharSequence text) {
		builder.append("<th style=\"background: #EEE; margin: 0.5em; padding: 0.25em;\">");
		if (text.length() == 0)
			builder.append("&nbsp;");
		else
			escape(text);
		builder.append("</th>");
		return this;
	}

	private HTMLReport green(String text) {
		builder.append("<span style=\"color: green\">");
		escape(text);
		builder.append("</span>");
		return this;
	}

	private HTMLReport red(String text) {
		builder.append("<span style=\"color: red\">");
		escape(text);
		builder.append("</span>");
		return this;
	}

	@Override
	public HTMLReport pass(boolean b) {
		if (b) green("pass"); else red("fail");
		return this;
	}

	@Override
	public HTMLReport compareTokens(List<Boolean> matches, List<String> actual,
			List<String> expected) {
		tableStart().rowStart().headerCell("Actual output").headerCell("Expected output")
				.rowEnd().rowStart();
		builder.append("<td style=\"background: #EEE; margin: 0.5em; padding: 0.25em; vertical-align: top;\">");
		builder.append("<pre>");
		for (int i = 0; i < actual.size(); i++) {
			if (i < matches.size() && matches.get(i))
				escape(actual.get(i));
			else
				red(actual.get(i));
			builder.append("\n");
		}
		builder.append("</pre>");
		cellEnd().cellStart();
		builder.append("<pre>");
		for (int i = 0; i < expected.size(); i++) {
			escape(expected.get(i));
			builder.append("\n");
		}
		builder.append("</pre>");
		cellEnd().rowEnd().tableEnd();
		return this;
	}
	
	@Override
	public HTMLReport runTable(String[] argNames, String[][] args, String[] actual,
			String[] expected, boolean[] outcomes) {
		tableStart().rowStart();
		for (String n : argNames) headerCell(n);
		headerCell("Actual").headerCell("Expected").headerCell("").rowEnd();
		for (int i = 0; i < args.length; i++) {
			rowStart();
			for (String a : args[i]) cell(a);
			cell(actual[i]);
			cell(expected[i]);
			cellStart();
			pass(outcomes[i]);
			cellEnd().rowEnd();
		}
		tableEnd();
		return this;
	}
	
	public HTMLReport comment(String text) {
		if (builder.charAt(builder.length() - 1) != '\n') builder.append('\n');
		builder.append("<!-- ");
		builder.append(text.replaceAll("--", "-&#45;"));
		builder.append(" -->\n");
		return this;
	}
}
