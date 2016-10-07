package com.horstmann.codecheck;

import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.io.ByteArrayOutputStream;


import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class Runner {

   private static void copy(File from, String sub, File to) throws IOException {
      Path source = from.toPath().resolve(sub);
      if (!Files.exists(source)) return;
      Path target = to.toPath();
      Files.walk(source).forEach(p -> {
            try {
               Path q = target.resolve(source.relativize(p));
               if (Files.isDirectory(p)) {
                  if (!Files.exists(q)) Files.createDirectory(q);
               }
               else
                  Files.copy(p, q);
            } catch (IOException ex) {
               throw new UncheckedIOException(ex);
            }
         });
   }

    public static void main(String[] args) {
        final JFrame frame = new JFrame();
        final JTextArea reportOutput = new JTextArea();
        JScrollPane reportScroll = new JScrollPane(reportOutput);
        frame.add(reportScroll);
        try {
            JComboBox levelCombo = new JComboBox(new String[] 
               { "1", "2", "3", "4", "5", "6", "7", "8", "9", "grade" });
                                                           

            final JFileChooser chooser = new JFileChooser();
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Select the directory containing the student/solutions directories");

            class Script implements ActionListener {
                String mode;

                Script(String mode) {
                    this.mode = mode;
                }

                public void actionPerformed(ActionEvent event) {
                    try {
                       reportOutput.setText("");
                        if (chooser.showOpenDialog(frame) != JFileChooser.APPROVE_OPTION)
                            return;
                        File basedir = chooser.getSelectedFile();
                        File studentdir = new File(basedir, "student");
                        if (!studentdir.exists()) {
                            reportOutput.append(studentdir + " not found.");
                            return;
                        }
                        File solutiondir = new File(basedir, "solution");
                        if (!solutiondir.exists()) {
                            reportOutput.append(solutiondir + " not found.");
                            return;
                        }
                        File submissiondir = Util.createTempDirectory().toFile();
                        String level = (String) levelCombo.getSelectedItem();
                        if (mode.equals("solution")) {
                           copy(basedir, "solution", submissiondir);
                           if (level.equals("grade")) {
                              for (int i = 1; i <= 9; i++)
                                 copy(basedir, "solution" + i, submissiondir);
                              copy(basedir, "grader", submissiondir);
                           } else {
                              for (int i = 1; i <= Integer.parseInt(level); i++)
                                 copy(basedir, "solution" + i, submissiondir);                           
                           }                           
                        }
                        else if (mode.equals("student")) {
                           copy(basedir, "student", submissiondir);
                           if (level.equals("grade")) {
                              for (int i = 1; i <= 9; i++)
                                 copy(basedir, "student" + i, submissiondir);
                              copy(basedir, "grader", submissiondir);
                           }
                           else {
                              for (int i = 1; i <= Integer.parseInt(level); i++)
                                 copy(basedir, "student" + i, submissiondir);                                                         
                           }
                        } 

                        File outputFile = new File(submissiondir, "error.txt");
                        File errorFile = new File(submissiondir, "error.txt");
                        String classPath = "";
                        for (URL url :((URLClassLoader) Runner.class.getClassLoader()).getURLs()) {
                            if (!classPath.equals("")) classPath += File.pathSeparator;
                            classPath += new File(url.toURI()).getAbsolutePath();
                        }
                        ProcessBuilder pb = new ProcessBuilder(
                           "java",
                           "-Dcom.horstmann.codecheck",
                           "-classpath",
                           classPath,
                           "com.horstmann.codecheck.Main",
                           level,
                           submissiondir.getAbsolutePath(),
                           basedir.getAbsolutePath());
                        pb.redirectErrorStream(true)
                           .directory(submissiondir)
                           .redirectOutput(outputFile);
                        pb.start().waitFor();

                        File report = new File(submissiondir, "report.html");
                        if (report.exists())
                           Desktop.getDesktop().browse(report.toURI());
                        if (errorFile.exists())
                           reportOutput.setText(new String(Files.readAllBytes(errorFile.toPath())));
                    } catch (Throwable ex) {
                       ByteArrayOutputStream out = new ByteArrayOutputStream();
                       ex.printStackTrace(new PrintStream(out));
                       reportOutput.append(out.toString());
                    }
                }
            }

            JPanel panel = new JPanel();
            JButton grade = new JButton("Check solution");
            panel.add(grade);
            grade.addActionListener(new Script("solution"));
            JButton check = new JButton("Check student files");
            panel.add(check);
            check.addActionListener(new Script("student"));            
            panel.add(new JLabel("Level:"));
            panel.add(levelCombo);
            frame.add(panel, "North");
            reportOutput.setPreferredSize(new Dimension(500, 500));
            frame.pack();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        } catch (Throwable ex) {
           ByteArrayOutputStream out = new ByteArrayOutputStream();
           ex.printStackTrace(new PrintStream(out));
           reportOutput.append(out.toString());
        }
    }
}
