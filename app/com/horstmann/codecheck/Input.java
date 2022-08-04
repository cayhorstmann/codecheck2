package com.horstmann.codecheck;


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