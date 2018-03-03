package com.horstmann.codecheck;

import java.util.Objects;

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
        return Objects.equals(file, other.file) 
                && line == other.line && column == other.column
                && Objects.equals(message, other.message);         
    }

   @Override public int hashCode() {
      return Objects.hash(file, line, column, message);
   }
}
