package com.horstmann.codecheck;

import java.io.IOException;
import java.io.InputStream;

public class CommandLineResourceLoader implements ResourceLoader {
    @Override
    public InputStream loadResource(String path) throws IOException {
        return getClass().getResourceAsStream(path);
    }
    @Override
    public String getProperty(String key) {
        return System.getProperty(key);
    }
}
