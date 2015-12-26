package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.imageio.ImageIO;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
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
    
    public static class Match {
        public Boolean matches;
        public String actual;
        public String expected;
    }
    
    public static class Run {
        public String caption;
        public List<Item> args;
        public String input;
        public String output;
        public List<Match> matchedOutput;
        public List<Item> files = new ArrayList<>();
        public List<Item> images = new ArrayList<>();
        public String errors; // TODO: Parse errors
    }
    
    public static class Section {
        public String type;
        public String errors;
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
    
    private ReportData data = new ReportData();
    private Section section;    
    private Run run; 
    private Path dir;
    
    /* TODO:
     * testMethod:
     * call:
     * sub:
     */

    public JSONReport(String title, Path dir) {
        this.dir = dir;
    }
    
    @Override
    public Report header(String sectionType, String text) {
        section = new Section();
        section.type = sectionType;
        if (!"studentFiles".equals(sectionType) && !"providedFiles".equals(sectionType))
            data.sections.add(section);
        run = null;
        return this;
    }

    @Override
    public Report run(String caption) { 
        run = new Run();
        if (section.runs == null) section.runs = new ArrayList<>();
        section.runs.add(run);
        run.caption = caption; 
        return null;
    }
    
    
    @Override
    public Report output(CharSequence text) {
        if (run.output == null) run.output = text.toString();
        else run.output += "\n" + text;
        return this;
    }

    @Override
    public Report error(String message) {
        if (run != null) {
            if (run.errors == null) run.errors = message;
            else run.errors += "\n" + message;
        } else if (section != null) {
            if (section.errors == null) section.errors = message;
            else section.errors += "\n" + message;
        }
        else if (data.errors == null) data.errors = message;
        else data.errors += "\n" + message;
        return this;
    }

    @Override
    public Report systemError(String message) {
        if (data.errors == null) data.errors = message;
        else run.errors += "\n" + message;
        return this;
    }

    @Override
    public Report systemError(Throwable t) {
        systemError(Util.getStackTrace(t));
        return this;
    }

    @Override
    public Report args(String args) {
        run.args = new ArrayList<>();
        run.args.add(new Item("Command line arguments", args));
        return this;
    }
    
    @Override
    public Report input(String input) {
        run.input = input;
        return this;
    }
    
    @Override
    public Report image(String caption, Path file) throws IOException {
        String data = Base64.getEncoder().encodeToString(Files
                .readAllBytes(file));
        run.images.add(new Item(caption, data));
        return this;
    }

    @Override
    public Report image(Path file) throws IOException {
        image("", file);
        return this;
    }

    @Override
    public Report image(String caption, BufferedImage image) throws IOException {
        if (image == null) return this;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ImageIO.write(image, "PNG", out);
        out.close();
        byte[] pngBytes = out.toByteArray();
        String data = Base64.getEncoder().encodeToString(pngBytes);
        run.images.add(new Item(caption, data));
        return this;
    }

    @Override
    public Report image(BufferedImage image) throws IOException {
        image("", image);
        return this;
    }

    @Override
    public Report file(Path dir, Path file) throws IOException {
        return file(file.toString(), Util.read(dir.resolve(file)));
    }
    
    @Override
    public JSONReport file(String file, String contents) {
        Item item = new Item(file, contents);
        /*
        if ("studentFiles".equals(section.type))
            data.studentFiles.add(item);
        else if ("providedFiles".equals(section.type))
            data.providedFiles.add(item);
                    
        else */
        if (!"studentFiles".equals(section.type) && !"providedFiles".equals(section.type)) 
            run.files.add(item);
        return this;
    }

    @Override
    public Report add(Score score) {
        data.score = "" + score;
        return this;
    }

    @Override
    public Report save(String problemId, String out) throws IOException {
        Path outPath = dir.resolve(out + ".json");
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(Include.NON_DEFAULT);
        mapper.writeValue(outPath.toFile(), data);
        // JSON.std.write(data, outPath.toFile());
        return this;
    }

    @Override
    public Report pass(boolean b) {
        // TODO Auto-generated method stub
        return this;
    }

    @Override
    public Report compareTokens(List<Boolean> matches, List<String> actual,
            List<String> expected) {
        run.matchedOutput = new ArrayList<>();
        for (int i = 0; i < actual.size() || i < expected.size(); i++)
        {
            Match m = new Match();
            run.matchedOutput.add(m);
            if (i < actual.size()) m.actual = actual.get(i);
            if (i < expected.size()) m.expected = expected.get(i);
            if (i < matches.size()) m.matches = matches.get(i);
        }
        return this;
    }

    @Override
    public Report output(List<String> lines, Set<Integer> matches,
            Set<Integer> mismatches) {
        run.matchedOutput = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            Match m = new Match(); 
            run.matchedOutput.add(m);
            m.actual = lines.get(i);
            if (matches.contains(i)) m.matches = true;
            else if (mismatches.contains(i)) m.matches = false;
        }
        return this;
    }

    @Override
    public Report runTable(String[] argNames, String[][] args, String[] actual,
            String[] expected, boolean[] outcomes) {
        if (section.runs == null) section.runs = new ArrayList<>();
        for (int i = 0; i < actual.length; i++)
        {
            Run run = new Run();
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
        }
        return this;
    }

    @Override
    public Report comment(String key, String value) {
        data.metaData.put(key, value);
        return this;
    }

    @Override
    public Report footnote(String text) {        
        return this;
    }  
}