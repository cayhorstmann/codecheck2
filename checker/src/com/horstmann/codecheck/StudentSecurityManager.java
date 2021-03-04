package com.horstmann.codecheck;

import java.security.Permission;

import javax.swing.JFrame;

public class StudentSecurityManager extends SecurityManager {
    static boolean okToExit = false;
    public static class ExitException extends SecurityException {
    }

    @Override
    public void checkExit(int status) {
        Class<?>[] stack = getClassContext();
        if (stack[1] != JFrame.class && !okToExit)
            throw new ExitException();
        super.checkExit(status);
    }

    @Override
    public void checkPermission(Permission perm) {
        // allow anything.
        // TODO: We don't want that
    }

    @Override
    public void checkPermission(Permission perm, Object context) {
        // allow anything.
    }
}
