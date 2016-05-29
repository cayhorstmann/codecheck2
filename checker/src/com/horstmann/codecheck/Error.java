package com.horstmann.codecheck;

public class Error {
    public String file;
    public int line;
    public int column;
    public String message;
    
    public Error() {}
    
    public Error(String file, int line, int column, String message) {
        this.file = file;
        this.line = line;
        this.column = column;
        this.message = message;
    }
}