package com.horstmann.codecheck;

import com.google.common.base.Objects;

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
    
    @Override public boolean equals(Object otherObject) {
        if (this == otherObject) return true;
        if (otherObject == null) return false;
        if (getClass() != otherObject.getClass()) return false;
        Error other = (Error) otherObject;
        return Objects.equal(file, other.file) 
                && line == other.line && column == other.column
                && Objects.equal(message, other.message);         
    }
}