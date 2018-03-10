package com.horstmann.codecheck;

public class CodeCheckException extends RuntimeException {
    public CodeCheckException() {        
    }

    public CodeCheckException(String message, Throwable cause) {
        super(message, cause);
    }

    public CodeCheckException(String message) {
        super(message);
    }

    public CodeCheckException(Throwable cause) {
        super(cause);
    }    
}
