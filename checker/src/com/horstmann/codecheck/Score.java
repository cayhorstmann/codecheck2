package com.horstmann.codecheck;

public class Score {
    private int total;
    private int passed;

    public void pass() {
        total++;
        passed++;
    }
    public void fail() {
        total++;
    }
    
    public void pass(boolean b) {
        total++;
        if (b) passed++;
    }
    
    public void pass(boolean b, Report r) {
        pass(b);
        r.pass(b);
    }
    
    public void add(int passed, int total, Report r) {
        this.passed += passed;
        this.total += total;
        pass(passed == total, r);
    }

    public String toString() {
        return total == 0 ? "" + passed : passed + "/" + total;
    }
}
