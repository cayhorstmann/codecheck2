package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

public interface Report
{

   public abstract Report header(String text);

   public abstract Report output(CharSequence text);

   public abstract Report output(String caption, CharSequence text);

   public abstract Report error(String caption, String message);

   public abstract Report error(String message);

   public abstract Report systemError(String message);

   public abstract Report systemError(Throwable t);

   public abstract Report image(String caption, Path file);

   public abstract Report image(Path file);

   public abstract Report image(String caption, BufferedImage image);

   public abstract Report image(BufferedImage image);

   public abstract Report file(Path dir, Path file);

   public abstract Report add(Score score);

   public abstract Report save(String problemId, String out) throws IOException;
   
   Report pass(boolean b);
   
   Report compareTokens(List<Boolean> matches, List<String> actual, List<String> expected);
   Report output(List<String> lines, Set<Integer> matches, Set<Integer> mismatches);
   Report runTable(String[] argNames, String[][] args, String[] actual, String[] expected, boolean[] outcomes);
   
   Report comment(String text);
}