package com.horstmann.codecheck;

import java.awt.AWTException;
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Robot;
import java.awt.Window;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

public class SnapFrame implements Runnable {
    // className output [--keys keys] arg0 arg1 ...
    public static void main(String[] args) throws Exception {
        String className = args[0];
        String outputFile = args[1];
        String robotKeys = null;
        int nonProgArgCount = 2;
        if (args.length >= 3 && args[2].equals("--keys")) {
            robotKeys = args[3];
            nonProgArgCount += 2;
        }
        String[] progArgs = new String[Math.max(0, args.length - nonProgArgCount)];
        for (int i = 0; i < progArgs.length; i++)
            progArgs[i] = args[i + nonProgArgCount];
        run(className, outputFile, robotKeys, progArgs, new File(".").getAbsoluteFile().toPath(), true);
        System.exit(0);
    }

    // Also called from Instruct.java
    /**
     * @param className
     * @param outputFile
     * @param robotKeys
     * @param progArgs
     * @param dir the directory containing the class files
     * @param exit
     * @return
     */
    public static String run(String className, String outputFile, String robotKeys, final String[] progArgs,
                             Path dir, boolean exit)
    throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException,
        InterruptedException, MalformedURLException, IOException {
        new JFrame().setVisible(true); // to fire up AWT
        keys = robotKeys;
        output = outputFile;
        // Capture frames before app starts
        oldFrames = Arrays.asList(Frame.getFrames());
        okToExit = exit;

        // Call main
        URLClassLoader loader = new URLClassLoader(new URL[] { dir.toUri().toURL() });
        try {
            Class<?> cl = loader.loadClass(className);
            final Method m = cl.getMethod("main", String[].class);
            // TODO: Move to main entry point

            final ByteArrayOutputStream err = new ByteArrayOutputStream();
            final PrintStream errPrint = new PrintStream(err);
            final Thread mainmethodThread = new Thread() {
                public void run() {
                    try {
                        m.invoke(null, (Object) progArgs);
                    } catch (InvocationTargetException ex) {
                        Throwable cause = ex.getCause();
                        if (cause == null)
                            ex.printStackTrace(errPrint);
                        else if (!(cause instanceof StudentSecurityManager.ExitException))
                            cause.printStackTrace(errPrint);
                    } catch (Throwable t) {
                        t.printStackTrace(errPrint);
                    }
                }
            };

            mainmethodThread.start();
            // Start robot thread
            Thread robotThread = new Thread(new SnapFrame());
            robotThread.start();

            final int delayMillis = 10000; // TODO: Parameterize

            try {
                mainmethodThread.join(delayMillis);
            } catch (InterruptedException e) {
                e.printStackTrace(errPrint);
            }
            try {
                robotThread.join(delayMillis);
            } catch (InterruptedException e) {
                e.printStackTrace(errPrint);
            }

            return err.toString();
        } finally {
            loader.close();
        }
    }

    private static String output;
    private static String keys;
    private static List<Frame> oldFrames;
    private static boolean okToExit;

    private Window frame;

    // TODO: When the server does this for many students, isn't there a chance of conflicting frames?
    // In that case, should one store something with the frame (e.g. a title) to identify ours?
    public Window getFrame() {
        if (frame != null)
            return frame;
        // find first new frame
        Frame[] frames = Frame.getFrames();
        for (int i = 0; i < frames.length; i++) {
            if (!oldFrames.contains(frames[i]) && frames[i] instanceof JFrame) {
                frame = frames[i];
                return frame;
            }
        }

        // No frame--try finding an option pane dialog
        for (Frame f : Frame.getFrames()) {
            for (Window w : f.getOwnedWindows())
                if (w instanceof Dialog) {
                    frame = w;
                    return frame;
                }
        }

        return null;
    }

    // I tried to use an anonymous class, but Ant didn't load it
    public void run() {
        Robot robot;
        try {
            robot = new Robot();
        } catch (AWTException ex) {
            return;
        }
        robot.delay(2000);
        robot.waitForIdle();

        if (keys != null && !keys.equals("-")) {
            int k = 0;
            while (k < keys.length()) {
                int keyCode = 0;
                int modifiers = 0;
                char key = keys.charAt(k);
                k++;
                if (key == '~') { // 100 ms delay
                    keyCode = 0;
                } else if (key == '(' && getFrame() != null) { // mouse click
                    int end = k;
                    while (end < keys.length() && keys.charAt(end) != ',')
                        end++;
                    int x = Integer.parseInt(keys.substring(k, end)) + frame.getBounds().x;
                    end++;
                    k = end;
                    while (end < keys.length() && keys.charAt(end) != ')')
                        end++;
                    int y = Integer.parseInt(keys.substring(k, end)) + frame.getBounds().y;
                    k = end + 1;
                    robot.mouseMove(x, y);
                    robot.mousePress(InputEvent.BUTTON1_MASK);
                    robot.mouseRelease(InputEvent.BUTTON1_MASK);
                } else if (key == '%') {
                    key = keys.charAt(k);
                    k++;
                    if (key == 't')
                        keyCode = KeyEvent.VK_TAB;
                    else if (key == 'n')
                        keyCode = KeyEvent.VK_ENTER;
                    else if (key == 's')
                        keyCode = KeyEvent.VK_SPACE;
                } else if (key == '[') {
                    int end = k;
                    while (end < keys.length() && keys.charAt(end) != ']')
                        end++;
                    String keyName = keys.substring(k, end);
                    KeyStroke stroke = KeyStroke.getKeyStroke(keyName);
                    if (stroke != null) {
                        keyCode = stroke.getKeyCode();
                        modifiers = stroke.getModifiers();
                    }
                    k = end + 1;
                } else if ('a' <= key && key <= 'z') {
                    keyCode = KeyEvent.VK_A + key - 'a';
                } else if ('1' <= key && key <= '9') {
                    keyCode = KeyEvent.VK_1 + key - '1';
                } else if (key == '0') {
                    keyCode = KeyEvent.VK_0;
                }

                if (modifiers != 0) {
                    if ((modifiers & InputEvent.SHIFT_DOWN_MASK) != 0)
                        robot.keyPress(KeyEvent.VK_SHIFT);
                    if ((modifiers & InputEvent.CTRL_DOWN_MASK) != 0)
                        robot.keyPress(KeyEvent.VK_CONTROL);
                    if ((modifiers & InputEvent.META_DOWN_MASK) != 0)
                        robot.keyPress(KeyEvent.VK_META);
                    if ((modifiers & InputEvent.ALT_DOWN_MASK) != 0)
                        robot.keyPress(KeyEvent.VK_ALT);
                    if ((modifiers & InputEvent.ALT_GRAPH_DOWN_MASK) != 0)
                        robot.keyPress(KeyEvent.VK_ALT_GRAPH);
                }

                if (keyCode != 0) {
                    robot.keyPress(keyCode);
                    robot.keyRelease(keyCode);
                }

                if (modifiers != 0) {
                    if ((modifiers & InputEvent.ALT_GRAPH_DOWN_MASK) != 0)
                        robot.keyRelease(KeyEvent.VK_ALT_GRAPH);
                    if ((modifiers & InputEvent.ALT_DOWN_MASK) != 0)
                        robot.keyRelease(KeyEvent.VK_ALT);
                    if ((modifiers & InputEvent.META_DOWN_MASK) != 0)
                        robot.keyRelease(KeyEvent.VK_META);
                    if ((modifiers & InputEvent.CTRL_DOWN_MASK) != 0)
                        robot.keyRelease(KeyEvent.VK_CONTROL);
                    if ((modifiers & InputEvent.SHIFT_DOWN_MASK) != 0)
                        robot.keyRelease(KeyEvent.VK_SHIFT);
                }

                robot.delay(100);
            }
            robot.delay(1000);
            robot.waitForIdle();
        }

        if (getFrame() != null) {
            BufferedImage image = robot.createScreenCapture(frame.getBounds());

            // TODO: full-screen option
            String extension = output.substring(output.lastIndexOf('.') + 1);
            try {
                ImageIO.write(image, extension, new File(output));
            } catch (IOException ex) {
                ex.printStackTrace();
            }

            // Got our snapshot--no reason to wait for the app to close
            if (okToExit)
                System.exit(0);
        }
    }
}
