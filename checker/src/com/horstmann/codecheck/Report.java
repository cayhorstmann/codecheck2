package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

public interface Report
{

   public abstract Report header(String section, String text);
   
   public abstract Report run(String caption);

   public abstract Report output(CharSequence text);

   public abstract Report error(String message);

   public abstract Report systemError(String message);

   public abstract Report systemError(Throwable t);

   public abstract Report image(String caption, Path file) throws IOException;

   public abstract Report image(Path file) throws IOException;

   public abstract Report image(String caption, BufferedImage image) throws IOException;

   public abstract Report image(BufferedImage image) throws IOException;

   public abstract Report file(Path dir, Path file) throws IOException;

   public abstract Report file(String file, String contents);
   
   public abstract Report args(String args);
   
   public abstract Report input(String input);

   public abstract Report add(Score score);

   public abstract Report save(String problemId, String out) throws IOException;
   
   Report pass(boolean b);
   
   Report compareTokens(List<Boolean> matches, List<String> actual, List<String> expected);
   Report output(List<String> lines, Set<Integer> matches, Set<Integer> mismatches);
   Report runTable(String[] functionNames, String[] argNames, String[][] args, String[] actual, String[] expected, boolean[] outcomes);
   
   Report comment(String key, String value);
   
   Report footnote(String text);
}