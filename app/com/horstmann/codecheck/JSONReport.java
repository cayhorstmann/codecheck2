package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.imageio.ImageIO;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class JSONReport implements Report {
    public static class Item {
        public Item() {}
        public Item(String name, String contents) {
            this.name = name;
            this.value = contents;
        }
        public String name;
        public String value;
    }
    
    public static class Run {
        public String caption;
        public List<Item> args;
        public String input;
        public String output;
        public List<Match> matchedOutput;
        public List<Item> files = new ArrayList<>();
        public List<Item> images = new ArrayList<>();
        public String errors; 
        public List<Error> errorData = new ArrayList<>();
        public String html;
        public Boolean passed;
    }
    
    public static class Section {
        public String type;
        public String errors;
        public List<Error> errorData = new ArrayList<>();
        public List<Run> runs;
    }
    
    public static class ReportData {    
        public String errors;
        //public List<Item> studentFiles = new ArrayList<>();
        //public List<Item> providedFiles = new ArrayList<>();
        public List<Section> sections = new ArrayList<>();
        public Map<String, String> metaData = new LinkedHashMap<>();
        public String score; // TODO: Score each item
    }
    
    protected ReportData data = new ReportData();
    private Section section;    
    private Run run; 
    
    /* TODO:
     * testMethod:
     * call:
     * sub:
     */

    public JSONReport(String title) {
    }
    
    @Override
    public JSONReport header(String sectionType, String text) {
        section = new Section();
        section.type = sectionType;
        if (!"studentFiles".equals(sectionType) && !"providedFiles".equals(sectionType))
            data.sections.add(section);
        run = null;
        return this;
    }

    @Override
    public JSONReport run(String caption) { 
        run = new Run();
        run.passed = true;
        if (section.runs == null) section.runs = new ArrayList<>();
        section.runs.add(run);
        run.caption = caption; 
        return null;
    }
    
    
    @Override
    public JSONReport output(CharSequence text) {
        if (run.output == null) run.output = text.toString();
        else run.output += "\n" + text;
        
        StringBuilder builder = new StringBuilder();
        if (run.html != null) builder.append(run.html);
        builder.append("<p><b>Output:</b></p>");
        builder.append("<pre>");
        builder.append(HTMLReport.htmlEscape(text));
        builder.append("</pre>");
        run.html = builder.toString();                    

        return this;
    }

    @Override
    public JSONReport error(String message) {
        if (message == null) return this;
        if (run != null) {
            run.passed = false;
            if (run.errors == null) run.errors = message;
            else run.errors += "\n" + message;
            StringBuilder builder = new StringBuilder();
            if (run.html != null) builder.append(run.html);
            builder.append("<p><b>Error:</b></p>");
            builder.append("<pre>");
            builder.append(HTMLReport.htmlEscape(message));
            builder.append("</pre>");
            run.html = builder.toString();
        } else if (section != null) {
            if (section.errors == null) section.errors = message;
            else section.errors += "\n" + message;
        }
        else if (data.errors == null) data.errors = message;
        else data.errors += "\n" + message;
        return this;
    }

    @Override
    public JSONReport systemError(String message) {
        return error(message);
    }

    @Override
    public JSONReport systemError(Throwable t) {
        return systemError(Util.getStackTrace(t));
    }

    @Override
    public JSONReport args(String args) {
        if (args == null || args.trim().length() == 0) return this;
        run.args = new ArrayList<>();
        run.args.add(new Item("Command line arguments", args));
        return this;
    }
    
    @Override
    public JSONReport input(String input) {
        run.input = input;
        StringBuilder builder = new StringBuilder();
        if (run.html != null) builder.append(run.html);           
        if (run.input != null) {
            builder.append("<p><b>Input:</b></p><pre>");
            builder.append(HTMLReport.htmlEscape(run.input));
            builder.append("</pre>");
        }
        run.html = builder.toString();
        return this;
    }
    
    @Override
    public JSONReport image(String caption, BufferedImage image) {
        if (image == null) return this;
        try {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            ImageIO.write(image, "PNG", out);
            out.close();
            byte[] pngBytes = out.toByteArray();
            String data = Base64.getEncoder().encodeToString(pngBytes);
            run.images.add(new Item(caption, data));
        } catch (Exception ex) {
            run.images.add(new Item(caption, null));
        }
        return this;
    }

    @Override
    public JSONReport image(BufferedImage image) {
        image("", image);
        return this;
    }
    
    @Override
    public JSONReport file(String file, String contents) {
        Item item = new Item(file, contents);
        if (!"studentFiles".equals(section.type) && !"providedFiles".equals(section.type)) { 
            run.files.add(item);
            
            StringBuilder builder = new StringBuilder();
            if (run.html != null) builder.append(run.html);
            builder.append("<p><b>");
            builder.append(HTMLReport.htmlEscape(file));
            builder.append("</b></p><pre>");
            builder.append(HTMLReport.htmlEscape(contents));
            builder.append("</pre>");
            run.html = builder.toString();                    
        }
        return this;
    }

    @Override
    public JSONReport add(Score score) {
        data.score = "" + score;
        return this;
    }

    @Override
    public String extension() { return "json"; }

    @Override
    public String getText() { 
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(Include.NON_DEFAULT);        
        try {
            return mapper.writeValueAsString(data);
        } catch (JsonProcessingException e) {    
            return null;
        }
    }

    @Override
    public JSONReport pass(boolean b) {
        if (run != null) {
            if (!b) run.passed = false;
        }
        return this;
    }

    @Override
    public JSONReport compareTokens(String filename, List<Match> matchData) {
        run.matchedOutput = new ArrayList<>();
        for (Match m : matchData)
            run.matchedOutput.add(m);
        StringBuilder builder = new StringBuilder();
        if (run.html != null) builder.append(run.html);
        builder.append("<table border='1' style='border-collapse: collapse;'>");
        builder.append("<tr><th>Actual output</th><th>Expected output</th></tr>");
        builder.append("<tr><td><pre>");
        for (Match m : matchData) {
            StringBuilder row = HTMLReport.htmlEscape(m.actual);
            if (m.matches)
                builder.append(row);
            else {
                builder.append("<em style='color: red;'>"); 
                builder.append(row);
                builder.append("</em>");
            }        
            builder.append("\n");
        }
        builder.append("</pre></td><td><pre>");
        for (Match m : matchData) {
            builder.append(HTMLReport.htmlEscape(m.expected));
            builder.append("\n");
        }
        builder.append("</pre></td></tr></table>");
        run.html = builder.toString();
        
        return this;
    }

    @Override
    public JSONReport output(List<String> lines, Set<Integer> matches,
            Set<Integer> mismatches) {
        run.matchedOutput = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            Match m = new Match(); 
            run.matchedOutput.add(m);
            m.actual = lines.get(i);
            if (matches.contains(i)) m.matches = true;
            else if (mismatches.contains(i)) m.matches = false;
        }
        
        StringBuilder builder = new StringBuilder();
        if (run.html != null) builder.append(run.html);
        builder.append("<pre>");
        for (int i = 0; i < lines.size(); i++) {
            StringBuilder line = HTMLReport.htmlEscape(lines.get(i));
            if (matches.contains(i)) {
                builder.append("<span style='color: green;'>");
                builder.append(line);
                builder.append("</span>");
            }
            else if (mismatches.contains(i)) {
                builder.append("<span style='color: red;'>");
                builder.append(line);
                builder.append("</span>");                
            }
            else
                builder.append(line);
            builder.append("\n");
        }
        builder.append("</pre>\n");
        run.html = builder.toString();                    
        
        return this;
    }

    @Override
    public JSONReport runTable(String[] methodNames, String[] argNames, String[][] args, String[] actual,
            String[] expected, boolean[] outcomes) {
        if (section.runs == null) section.runs = new ArrayList<>();
        for (int i = 0; i < actual.length; i++)
        {
            Run run = new Run();
            run.passed = true;
            if (methodNames != null) run.caption = methodNames[i];
            section.runs.add(run);
            run.matchedOutput = new ArrayList<>();
            Match m = new Match();
            run.matchedOutput.add(m);
            m.actual = actual[i]; 
            m.expected = expected[i];
            m.matches = outcomes[i];
            run.args = new ArrayList<>();            
            for (int j = 0; j < argNames.length; j++) {
                run.args.add(new Item(argNames[j], args[i][j]));
            }
            
            StringBuilder builder = new StringBuilder();
            if (run.html != null) builder.append(run.html);
            builder.append("<table border='1' style='border-collapse: collapse;'><tr><th>&#160;</th>");
            if (methodNames != null) builder.append("<th>Name</th>");
            for (String n : argNames) { 
                builder.append("<th>"); 
                builder.append(HTMLReport.htmlEscape(n)); 
                builder.append("</th>"); 
            }
            builder.append("<th>Actual</th><th>Expected</th></tr>");
            builder.append("<tr><td>");
            if (outcomes[i]) builder.append("<span style='color: green'>Pass");
            else builder.append("<span style='color: red'>Fail");
            builder.append("</span></td><td>");
            if (methodNames != null) { 
                builder.append("<code>"); 
                builder.append(HTMLReport.htmlEscape(methodNames[i]));
                builder.append("</code></td><td>");
            }
            for (String a : args[i]) { 
                builder.append("<code>"); 
                builder.append(HTMLReport.htmlEscape(a.trim()));
                builder.append("</code></td><td>");
            }
            builder.append("<code>"); 
            builder.append(HTMLReport.htmlEscape(actual[i].trim()));
            builder.append("</code></td><td><code>"); 
            builder.append(HTMLReport.htmlEscape(expected[i].trim()));
            builder.append("</code></td></tr>");
            builder.append("</table>");  
            run.html = builder.toString();
        }
        return this;
    }

    @Override
    public JSONReport comment(String key, String value) {
        data.metaData.put(key, value);
        return this;
    }

    @Override
    public JSONReport hiddenOutputMessage() {
        //data.metaData.put("Given the instructions from your instructor, the output is hidden!");
        StringBuilder builder = new StringBuilder();
        builder.append("[hidden]"); 
        run.html = builder.toString();
        return this;
    }

    @Override
    public JSONReport footnote(String text) {        
        return this;
    }
    
    @Override
    public JSONReport errors(List<Error> errorData) 
    {
        if (section != null)
            section.errorData.addAll(errorData);
        return this; 
    } 
}
