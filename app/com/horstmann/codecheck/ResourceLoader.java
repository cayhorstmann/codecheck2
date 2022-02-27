package com.horstmann.codecheck;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public interface ResourceLoader {
    InputStream loadResource(String path) throws IOException;
    default String loadResourceAsString(String path) throws IOException { 
        return new String(loadResource(path).readAllBytes(), StandardCharsets.UTF_8);
    }
    String getProperty(String key);
}
