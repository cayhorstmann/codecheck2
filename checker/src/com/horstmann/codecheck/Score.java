package com.horstmann.codecheck;

public class Score {
    private int total;
    private int passed;
    private String PASS = "<span style=\"color: green\">pass</span>";
    private String FAIL = "<span style=\"color: red\">fail</span>";

    public void pass() {
        total++;
        passed++;
    }
    public void fail() {
        total++;
    }
    public String pass(boolean b) {
        total++;
        if (b) passed++;
        return b ? PASS : FAIL;
    }
    public void pass(boolean b, Report r) {
        r.html(pass(b));
    }
    public void pass(boolean b, StringBuilder builder) {
        builder.append(pass(b));
    }


    public String toString() {
        return total == 0 ? "" + passed : passed + "/" + total;
    }
}
