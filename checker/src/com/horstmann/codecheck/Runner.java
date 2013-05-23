package com.horstmann.codecheck;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;

import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

public class Runner {
    public static void main(String[] args) {
        final JFrame frame = new JFrame();
        try {
            final JEditorPane reportOutput = new JEditorPane();
            JScrollPane reportScroll = new JScrollPane(reportOutput);
            frame.add(reportScroll);

            final JFileChooser chooser = new JFileChooser();
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Select the directory containing the student/grader/solutions directories");

            class Script implements ActionListener {
                String mode;

                Script(String mode) {
                    this.mode = mode;
                }

                public void actionPerformed(ActionEvent event) {
                    try {
                        if (chooser.showOpenDialog(frame) != JFileChooser.APPROVE_OPTION)
                            return;
                        File basedir = chooser.getSelectedFile();
                        File studentdir = new File(basedir, "student");
                        if (!studentdir.exists()) {
                            JOptionPane.showMessageDialog(frame, studentdir + " not found.");
                            return;
                        }
                        File solutiondir = new File(basedir, "solution");
                        if (!solutiondir.exists()) {
                            JOptionPane.showMessageDialog(frame, solutiondir + " not found.");
                            return;
                        }
                        File graderdir = new File(basedir, "grader");
                        if (mode.equals("grade") && !graderdir.exists()) {
                            JOptionPane.showMessageDialog(frame, graderdir + " not found.");
                            return;
                        }

                        File submissiondir = Files.createTempDirectory("labrat").toFile();
                        File outputFile = new File(submissiondir, "error.txt");
                        File errorFile = new File(submissiondir, "error.txt");
                        String classPath = "";
                        for (URL url :((URLClassLoader) Runner.class.getClassLoader()).getURLs()) {
                            if (!classPath.equals("")) classPath += File.pathSeparator;
                            classPath += new File(url.toURI()).getAbsolutePath();
                        }
                        ProcessBuilder pb = new ProcessBuilder("java",
                                                               "-Dlabrat.img.inline=false",
                                                               "-classpath",
                                                               classPath,
                                                               "com.horstmann.labrat.Main",
                                                               mode,
                                                               solutiondir.getAbsolutePath(),
                                                               basedir.getAbsolutePath())
                        .directory(submissiondir)
                        .redirectOutput(outputFile)
                        .redirectError(errorFile);
                        pb.start().waitFor();

                        File report = new File(submissiondir, "report.html");
                        if (report.exists())
                            reportOutput.setPage(report.toURI().toURL());
                        else if (errorFile.exists())
                            reportOutput.setPage(errorFile.toURI().toURL());
                    } catch (Throwable ex) {
                        JOptionPane.showMessageDialog(frame, ex.getMessage());
                        ex.printStackTrace();
                    }
                }
            }

            JPanel panel = new JPanel();
            JButton check = new JButton("Student View");
            panel.add(check);
            check.addActionListener(new Script("check"));
            JButton grade = new JButton("Grader View");
            panel.add(grade);
            grade.addActionListener(new Script("grade"));
            frame.add(panel, "North");
            reportOutput.setPreferredSize(new Dimension(500, 500));
            frame.pack();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        } catch (Throwable ex) {
            JOptionPane.showMessageDialog(frame, ex.getMessage());
            ex.printStackTrace();
        }
    }
}
