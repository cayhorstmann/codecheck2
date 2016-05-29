package com.horstmann.codecheck;

public class Score {
    private int total;
    private int passed;
    private boolean invalid;

    public void pass() {
        if (invalid) return;
        total++;
        passed++;
    }
    public void fail() {
        if (invalid) return;
        total++;
    }
    
    public void pass(boolean b) {
        if (invalid) return;
        total++;
        if (b) passed++;
    }
    
    public void pass(boolean b, Report r) {
        if (invalid) return;
        pass(b);
        r.pass(b);
    }
    
    public void add(int passed, int total, Report r) {
        if (invalid) return;
        this.passed += passed;
        this.total += total;
        pass(passed == total, r);
    }
    
    public void setInvalid() {
        invalid = true;
    }
    
    public boolean isInvalid() {
        return invalid;
    }

    public String toString() {
        if (invalid) return "0";
        return total == 0 ? "" + passed : passed + "/" + total;
    }
}
