package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

public interface Report
{
    static class Match 
    {
        public String actual;
        public String expected;
        public boolean matches;
        public String explanation;
    }

   Report header(String section, String text);
   
   Report run(String caption);

   Report output(CharSequence text);

   Report error(String message);

   Report systemError(String message);

   Report systemError(Throwable t);

   Report image(String caption, BufferedImage image);

   Report image(BufferedImage image);

   Report file(String file, String contents);
   
   Report args(String args);
   
   Report input(String input);

   Report add(Score score);
   
   Report pass(boolean b);
   
   Report compareTokens(String filename, List<Match> matches);
   Report output(List<String> lines, Set<Integer> matches, Set<Integer> mismatches);
   Report runTable(String[] functionNames, String[] argNames, String[][] args, String[] actual, String[] expected, boolean[] outcomes);
   
   Report comment(String key, String value);

   Report hiddenOutputMessage(); 
   
   Report footnote(String text);
   
   default void close() {}
   String getText();
   
   default Report errors(List<Error> errors) { return this; }
   
   String extension();
}