package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Input{ 
    private String key; 
    private String value; 
    private boolean hidden; 


    public Input(String key, String value, boolean hidden) {
        this.key = key; 
        this.value = value; 
        this.hidden = hidden; 
    }

    public boolean getHidden(){
        return hidden; 
    }

    public String getKey() {
        return key; 
    }

    public String getValue() {
        return value; 
    }
}