package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

public class Problem {
    @JsonSerialize(using = TileSerializer.class)
    public static class Tile {
        public String code;
        public String text;
    }
    
    public static class TileSerializer extends JsonSerializer<Tile> {
        @Override
        public void serialize(Tile value, JsonGenerator jgen,
            SerializerProvider provider) throws IOException, JsonProcessingException {
            if (value.text.isBlank()) 
                jgen.writeString(value.code);
            else {
                jgen.writeStartObject();
                jgen.writeStringField("code", value.code);
                jgen.writeStringField("text", value.text);
                jgen.writeEndObject();
            }
        }

        @Override
        public Class<Tile> handledType() {
            return Tile.class;
        }
    }
    
    public static class EditorState {
        public List<String> editors;
        public List<Tile> fixed;
        public List<Tile> tiles;
    }
    
    public static class DisplayData {
        public Map<String, EditorState> requiredFiles = new LinkedHashMap<>();
        public Map<String, String> useFiles = new LinkedHashMap<>();
        public String description;
    }

    private Map<Path, byte[]> problemFiles;
    private Map<Path, byte[]> useFiles = new Util.FileMap(); 
        // the files (sources and inputs) from problemFiles that must be copied to the directory 
        // in which the submission/solution program is run
    private Map<Path, byte[]> solutionFiles = new Util.FileMap(); 
        // the source files from problemFiles that are to be submitted 
    private Map<Path, byte[]> inputFiles = new Util.FileMap();
        // the input files that are to be submitted (in input mode) 
    private Language language;
    private Annotations annotations;
    private boolean inputMode = false;
    private boolean oldStyle; // separate student/solution directories
    private String start; // pseudocode delimiters
    private String end;

    private static final Pattern IMG_PATTERN = Pattern
            .compile("[<]\\s*[iI][mM][gG]\\s*[sS][rR][cC]\\s*[=]\\s*['\"]([^'\"]*)['\"][^>]*[>]");
    private static final Pattern LINK_START = Pattern
            .compile("<\\s*[aA]\\s+[^>]*[hH][rR][eE][fF]\\s*=\\s*['\"]([^'\"]+)['\"][^>]*>");
    private static final Pattern LINK_END = Pattern.compile("<\\s*/\\s*[aA]\\s*>");

    public Problem(Map<Path, byte[]> problemFiles) throws IOException {
        language = Language.languageFor(problemFiles.keySet());        
        if (language == null) throw new CodeCheckException("Cannot find language from " + problemFiles.keySet());
            
        this.problemFiles = problemFiles;
        annotations = new Annotations(language);
        String[] delims = language.pseudoCommentDelimiters();
        start = delims[0];
        end = delims[1];
        
        oldStyle = problemFiles.keySet().stream().anyMatch(p -> p.getName(0).toString().equals("student")); 
        if (oldStyle) {
            // Directories for student/solution files  
            for (Path p : problemFiles.keySet()) {
                if (!Util.matches(Util.tail(p), ".*", "*~", "check.properties", "*.zy", "*.class", "a.out")) {
                    String initial = p.getName(0).toString(); 
                    if (initial.equals("student")) {
                        useFiles.put(Util.tail(p), problemFiles.get(p));
                    }
                    else if (initial.equals("solution")) {
                        solutionFiles.put(Util.tail(p), problemFiles.get(p));                       
                    }                   
                }
            }
            annotations.read(useFiles);
            annotations.read(solutionFiles);
        } else {
            // Solution files marked with annotation
            for (Path p : problemFiles.keySet()) {
                if (!Util.matches(p, ".*", "*~", "*.class", "a.out", "*.pyc",  
                    "index.html", "index.ch", "problem.html", 
                    "Input", "*.in", "q.properties", "check.properties",
                    "tracer.js", "param.js", "edit.key", "*.zy")) {
                    useFiles.put(p, problemFiles.get(p));
                }
            }
            annotations.read(useFiles);
            // Move any files annotated with SOLUTION, SHOW or EDIT to solution 
            for (Path p : annotations.getSolutions()) {
                solutionFiles.put(p, useFiles.get(p));
                useFiles.remove(p);
            }
        }
        
        Path inputPath = Paths.get("Input");
        inputMode = problemFiles.containsKey(inputPath);            
        if (inputMode) {
            // The user can edit all non-hidden source files
            Set<Path> useFilePaths = new HashSet<Path>(useFiles.keySet());
            for (Path p : useFilePaths) {
                if (language.isSource(p) && !annotations.getHidden().contains(p)) {
                    solutionFiles.put(p, useFiles.get(p));
                    useFiles.remove(p);
                }
            }
            inputFiles.put(inputPath, problemFiles.get(inputPath));
            for (String runargs : annotations.findAll("ARGS"))
                for (String arg : runargs.split("\\s+"))
                    if (isTextFile(arg)) {
                        Path argPath = Paths.get(arg);
                        if (useFiles.containsKey(argPath)) {
                            inputFiles.put(argPath, useFiles.get(argPath));
                            useFiles.remove(argPath);
                        }
                    }            
        }   
        if (solutionFiles.isEmpty() && !problemFiles.containsKey(Path.of("tracer.js"))) {                                   
            throw new CodeCheckException("No solution files found");
        }
    }


    public Map<Path, byte[]> getUseFiles() {
        return Collections.unmodifiableMap(useFiles);
    }
    
    public Map<Path, byte[]> getSolutionFiles() {
        return Collections.unmodifiableMap(solutionFiles);
    }
    
    public Map<Path, byte[]> getInputFiles() {
        return Collections.unmodifiableMap(inputFiles);
    }
    
    public boolean getInputMode() {
        return inputMode;
    }
    
    public Language getLanguage() {
        return language;
    }
    
    public Annotations getAnnotations() {
        return annotations;
    }
    
    private String readDescription(String descriptionFile) {
        Path descriptionPath = Paths.get(descriptionFile);
        if (!problemFiles.containsKey(descriptionPath))
            return null;
        String description = Util.getString(problemFiles, descriptionPath);
        // Strip off HTML header. If the file contains "<body>" or "<BODY>",
        String lcdescr = description.toLowerCase();
        int start = lcdescr.indexOf("<body>");
        int end = -1;
        if (start != -1) {
            start += "<body>".length();
            end = lcdescr.lastIndexOf("</body>");
        } else {
            start = lcdescr.indexOf("<html>");
            if (start != -1) {
                start += "<html>".length();
                end = lcdescr.lastIndexOf("</html>");
            }
        }

        StringBuilder result = new StringBuilder(description);
        if (end != -1)
            result.replace(end, result.length(), "");
        if (start != -1)
            result.replace(0, start, "");

        // Check if links are relative or not, if relative links, change it to normal text
        Matcher linkMatcherStart = LINK_START.matcher(result);
        Matcher linkMatcherEnd = LINK_END.matcher(result);
        int startLink = 0;
        int endLink = 0;
        while (linkMatcherStart.find(startLink) && linkMatcherEnd.find(startLink)) {
            startLink = linkMatcherStart.start();
            endLink = linkMatcherEnd.end();
            String hrefLink = result.substring(linkMatcherStart.start(1), linkMatcherStart.end(1)).toLowerCase();     
            if (!(hrefLink.startsWith("http://") || hrefLink.startsWith("https://"))) {
                int startContent = linkMatcherStart.end();
                int endContent = linkMatcherEnd.start();
                String contentOfLink = result.substring(startContent, endContent);
                result.replace(startLink, endLink, contentOfLink);
                startLink += contentOfLink.length();
            }
            else
              startLink = endLink;
        }

        Matcher matcher = IMG_PATTERN.matcher(result);
        start = 0;
        while (matcher.find(start)) {
            start = matcher.start(1);
            end = matcher.end(1);
            String src = result.substring(start, end).trim();
            if (!(src.startsWith("http://") || src.startsWith("https://"))) {
                String replacement;
                String dataURI;
                Path srcPath = Paths.get(src);
                if (problemFiles.containsKey(srcPath)) {
                    byte[] imageBytes = problemFiles.get(srcPath);
                    Base64.Encoder encoder = Base64.getEncoder();
                    dataURI = encoder.encodeToString(imageBytes);
                } else
                    dataURI = "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";
                replacement = "data:image/png;base64," + dataURI;
                result.replace(start, end, replacement);
                start += replacement.length();
            }
        }

        return result.toString();
    }

    //TODO: Should be in Annotations
    private void clearPseudoComments(String[] lines, int from, int to) {
        for (int i = from; i < to; i++) {
            String line = lines[i];
            if (line != null) {
                Annotations.Annotation ann = Annotations.parse(line, start, end);
                if (ann.key.equals("SUB")) {
                    int n = lines[i].indexOf(start + "SUB");
                    int n2 = end.equals("") ? lines[i].length() : lines[i].indexOf(end, n) + end.length();
                    lines[i] = lines[i].substring(0, n) + lines[i].substring(n2);
                } 
                else if (ann.key.equals("REQUIRED") || ann.key.equals("FORBIDDEN")) {
                    lines[i] = null;
                    if (i < lines.length - 1 && lines[i + 1] != null) {
                        String nextLine = lines[i + 1].trim();
                        if (nextLine.startsWith(start) && nextLine.endsWith(end)) {
                            lines[i + 1] = null;
                            i++;
                        }
                    }
                } else if (ann.isValid)
                    lines[i] = null;                
            }
        }               
    }
    
    private String removePseudoComments(String contents) {
        String[] lines = contents.split("\n");
        clearPseudoComments(lines, 0, lines.length);
        StringBuilder result = new StringBuilder();
        for (String l : lines) {
            if (l != null) {
                result.append(l);
                result.append("\n");
            }
        }
        return result.toString();
    }
    
    /**
     * Yields a list of sections, alternating with editable and noneditable
     * sections. Starts with an editable section or null if the initial section
     * should be noneditable.
     */
    private EditorState processHideShow(String contents) {
        if (contents == null) {
            ArrayList<String> editors = new ArrayList<String>();
            editors.add("");
            EditorState state = new EditorState();
            state.editors = editors;
            return state;
        }

        String[] lines = contents.stripTrailing().split("\n");

        boolean hasEdit = false;
        boolean hasShow = false;
        boolean hasTile = false;
        boolean firstHide = false;
        for (int i = 0; i < lines.length && !hasTile && !hasEdit; i++) {
            Annotations.Annotation ann = Annotations.parse(lines[i], start, end);
            if (i == 0 && (ann.key.equals("HIDE") || ann.key.equals("HIDDEN"))) firstHide = true;
            else if (ann.key.equals("EDIT")) hasEdit = true;
            else if (ann.key.equals("SHOW")) hasShow = true;
            else if (ann.key.equals("TILE")) hasTile = true;            
        }
        if (lines.length == 0 || firstHide && !hasShow && !hasEdit) {
            EditorState state = new EditorState();          
            state.editors = new ArrayList<String>(); // Empty list means file is hidden
            return state;                   
        }
        if (hasTile) {
            return tileMode(lines);
        } else if (hasEdit) {
            return hideEditMode(lines);
        } else { 
            return hideShowMode(lines);
        }
    }


    private EditorState hideShowMode(String[] lines) {
        EditorState state = new EditorState();
        state.editors = new ArrayList<String>();
        boolean hiding = false;
        boolean somethingHidden = false;
        boolean isSolution = false;

        for (int i = 0; i < lines.length; i++) {
            String line = lines[i].trim();
            Annotations.Annotation ann = Annotations.parse(line, start, end);
                    
            if (ann.key.equals("SOLUTION")) {
                isSolution = true;
                lines[i] = null;
            } else if (ann.key.equals("HIDE")) {
                hiding = true;
                somethingHidden = true;
                lines[i] = null;
            } else if (ann.key.equals("SHOW")) {
                hiding = false;
                isSolution = true;
                String showString = start + "SHOW";
                int n1 = lines[i].indexOf(showString);
                int n2 = showString.length();
                int n3 = lines[i].lastIndexOf(end);
                if (n1 + n2 < n3)
                    lines[i] = lines[i].substring(0, n1) + lines[i].substring(n1 + n2 + 1, n3);
                else
                    lines[i] = null;
            } else if (hiding) {
                lines[i] = null;
            }
        }
        if (isSolution && !somethingHidden) {
            state.editors.add("");
            return state;
        }
        clearPseudoComments(lines, 0, lines.length);
        StringBuilder allRemainingLines = new StringBuilder();
        for (String l : lines) {
            if (l != null) {
                allRemainingLines.append(l);
                allRemainingLines.append("\n");
            }
        }
        state.editors.add(allRemainingLines.toString());
        return state;
    }

    private EditorState hideEditMode(String[] lines) {
        EditorState state = new EditorState();
        state.editors = new ArrayList<String>();
        int sectionStart = 0;
        boolean hiding = false;
        boolean editOnPreviousLine = false;
        boolean startWithEdit = false;
        for (int i = 0; i < lines.length; i++) {            
            String line = lines[i].trim();
            Annotations.Annotation ann = Annotations.parse(line, start, end); 
            if (ann.key.equals("EDIT")) {
                hiding = false;
                if (!editOnPreviousLine) { // emit preceding readonly section
                    clearPseudoComments(lines, sectionStart, i);
                    StringBuilder section = new StringBuilder();
                    for (int j = sectionStart; j < i; j++) {
                        if (lines[j] != null) {
                            section.append(lines[j]);
                            section.append("\n");
                        }
                    }
                    if (section.length() == 0) // Don't add a blank readonly section at the top
                    {
                        if (state.editors.size() > 0)
                            state.editors.add("\n");
                        else
                            startWithEdit = true;
                    } else
                        state.editors.add(section.toString());
                    sectionStart = i;
                }
                editOnPreviousLine = true;

                String editString = start + "EDIT";
                int n1 = lines[i].indexOf(editString);
                int n2 = editString.length();
                int n3 = lines[i].lastIndexOf(end);
                if (n1 + n2 < n3)
                    lines[i] = lines[i].substring(0, n1) + lines[i].substring(n1 + n2 + 1, n3);
                else
                    lines[i] = ""; // Edit section is never empty
            } else {
                if (editOnPreviousLine) { // emit edit section
                    clearPseudoComments(lines, sectionStart, i);
                    StringBuilder section = new StringBuilder();
                    for (int j = sectionStart; j < i; j++) {
                        if (lines[j] != null) {
                            section.append(lines[j]);
                            section.append("\n");
                        }
                    }
                    if (section.toString().trim().length() == 0) {
                        section.insert(0, "\n");
                        section.append("\n");
                    }
                    if (state.editors.size() == 0)
                        startWithEdit = true;
                    state.editors.add(section.toString());
                    sectionStart = i;
                }
                editOnPreviousLine = false;

                if (ann.key.equals("HIDE"))
                    hiding = true;
                if (hiding)
                    lines[i] = null;
            }
        }
        // Emit final section
        clearPseudoComments(lines, sectionStart, lines.length);
        StringBuilder section = new StringBuilder();
        for (int j = sectionStart; j < lines.length; j++) {
            if (lines[j] != null) {
                section.append(lines[j]);
                section.append("\n");
            }
        }
        if (editOnPreviousLine && section.toString().trim().length() == 0) {
            section.insert(0, "\n");
            section.append("\n");
        }

        state.editors.add(section.toString());
        if (!startWithEdit)
            state.editors.add(0, null);
        return state;
    }
    /*
//TILE

makes each line (until the next pseudocomment) a draggable tile. 
An opening brace groups with the preceding non-brace line. 
For example, this is a single tile:

if (x == 0)
{

//TILE n

makes the next n lines to a tile, then reverts to single line tiles

//FIXED

moves next lines to the left as single line tiles, until the next //FIXED or //TILE. 
 
Note: Anything before the first TILE is implicitly FIXED

//OR code
//OR n 
// code 1
// code 2
// ...
// code n

Adds to the distractors of the preceding tile. 
However, OR tiles following good tiles become global distractors. That means
global distractors either follow //FIXED or are on top

When a { follows one or more //OR in tile mode (not global), it is added to all preceding distractors 
and the tile preceding them.

If a tile has one or more //PSEUDO comments, then 

whitespace1 code1//PSEUDO pseudocode1
whitespace2 code2//PSEUDO pseudocode2
...

is transformed to the submitted code 

whitespace1 code1
whitespace2 code2

and the code to be displayed on the tiles

whitespace1 pseudocode1
whitespace2 pseudocode2
 
     */
    
    private String removeComments(StringBuilder tile) {
        String[] lines = tile.toString().split("\n");
        StringBuilder result = new StringBuilder();
        for (String line : lines) {
            if (result.length() > 0) result.append("\n");
            int i = line.indexOf(start);
            int j = line.lastIndexOf(end);
            if (i < 0 || j < 0) result.append(line);
            else result.append(line.substring(0, i) + line.substring(i + start.length(), j) + line.substring(j + end.length()));
        }
        return result.toString();
    }

    private static int lengthOfWhitespacePrefix(String line) {
        int k = 0;
        while (k < line.length() && Character.isWhitespace(line.charAt(k))) k++;
        return k;
    }
    
    private static List<Integer> indents(String[] lines) {
        SortedSet<Integer> prefixLengths = new TreeSet<>();
        for (String line : lines)       
            if (!line.isBlank())
                prefixLengths.add(lengthOfWhitespacePrefix(line));          
        return new ArrayList<>(prefixLengths);
    }
    
    public Tile makeTile(String contents, boolean fixed, List<Integer> indents) {
        String[] lines = contents.split("\n");
        normalizeIndentation(lines, indents);
        if (!fixed) stripIndentation(lines);
        ArrayList<String> pseudoLines = new ArrayList<>();
        for (int i = 0; i < lines.length; i++) {
            Annotations.Annotation ann = Annotations.parse(lines[i], start, end);
            if (ann.key.equals("PSEUDO")) {
                int ws = lengthOfWhitespacePrefix(lines[i]);
                pseudoLines.add(lines[i].substring(0, ws) + ann.args);
                lines[i] = ann.before;
            }
        }           
        Tile tile = new Tile();
        tile.code = String.join("\n", lines);
        tile.text = String.join("\n", pseudoLines);
        return tile;
    }
    
    private void normalizeIndentation(String[] lines, List<Integer> indents) {
        for (int i = 0; i < lines.length; i++) {
            int k = lengthOfWhitespacePrefix(lines[i]);
            lines[i] = "\t".repeat(indents.indexOf(k)) + lines[i].substring(k);
        }
    }
    
    private void stripIndentation(String[] lines) {
        int shortestPrefix = Integer.MAX_VALUE;
        for (int i = 0; i < lines.length; i++)          
            shortestPrefix = Math.min(shortestPrefix, lengthOfWhitespacePrefix(lines[i]));
        for (int i = 0; i < lines.length; i++) {
            lines[i] = lines[i].substring(shortestPrefix);
        }       
    }           

    private EditorState tileMode(String[] lines) {
        EditorState state = new EditorState();
        state.fixed = new ArrayList<Tile>();
        state.tiles = new ArrayList<Tile>();
        ArrayList<String> globalDistractors = new ArrayList<String>(); 
        ArrayList<List<String>> draggable = new ArrayList<List<String>>();
        ArrayList<String> currentGroup = new ArrayList<String>();       
        List<Integer> indents = indents(lines);
        // Before doing anything else, kill any trailing \r
        for (int i = 0; i < lines.length; i++) {
            lines[i] = lines[i].stripTrailing();
        }       
        boolean tileMode = false;
        for (int i = 0; i < lines.length; i++) {            
            String line = lines[i];
            Annotations.Annotation ann = Annotations.parse(line, start, end);
            int arg = -1;
            if (ann.isValid) {
                try { arg = Integer.parseInt(ann.args); } catch (NumberFormatException ex) {} 
            }
            if (ann.key.equals("TILE")) {
                tileMode = true;
                if (currentGroup.size() > 0) {
                    draggable.add(currentGroup);
                    currentGroup = new ArrayList<String>();
                }
                
                if (arg > 0) {
                    StringBuilder tile = new StringBuilder(); 
                    for (int j = 1; j <= arg; j++) {
                        if (j > 1) tile.append("\n");
                        tile.append(lines[i + j]);
                    }
                    currentGroup.add(tile.toString());
                    i += arg;
                } 
            } else if (ann.key.equals("OR")) {
                if (arg > 0) {
                    StringBuilder tile = new StringBuilder(); 
                    for (int j = 1; j <= arg; j++) {
                        if (j > 1) tile.append("\n");
                        tile.append(lines[i + j]);
                    }
                    (tileMode ? currentGroup : globalDistractors).add(removeComments(tile));
                    i += arg;
                } else {
                    (tileMode ? currentGroup : globalDistractors).add(ann.before + ann.args);
                }
            } else if (ann.key.equals("FIXED")) {
                tileMode = false;
            } else if (tileMode) {
                if (line.strip().equals("{")) { 
                    if (currentGroup.size() > 0) {
                        ArrayList<String> withBrace = new ArrayList<String>();
                        for (String tile :currentGroup) {
                            withBrace.add(tile + "\n" + line);
                        }
                        draggable.add(withBrace);
                        currentGroup = new ArrayList<String>();
                    }
                } else if ((!ann.isValid || ann.key.equals("PSEUDO")) && !line.isBlank()) {
                    if (currentGroup.size() > 0) {
                        draggable.add(currentGroup);
                        currentGroup = new ArrayList<String>();
                    }
                    currentGroup.add(line);
                }
            } else { // Region of fixed
                boolean done = false;
                StringBuilder tile = new StringBuilder();
                while (!done) {
                    if (i == lines.length) done = true;
                    else {
                        line = lines[i];
                        ann = Annotations.parse(line, start, end);
                        if (ann.isValid && !ann.key.equals("PSEUDO")) {
                            if (Set.of("FIXED", "OR", "TILE").contains(ann.key)) { 
                                done = true; 
                                i--;
                            } else 
                                i++;
                        } else {
                            if (tile.length() > 0) tile.append("\n"); 
                            tile.append(line);
                            i++;
                        }
                    }
                }
                state.fixed.add(makeTile(tile.toString(), /* fixed = */ true, indents));                
            }
        }
        if (currentGroup.size() > 0) 
            draggable.add(currentGroup);
        for (String distractor : globalDistractors)
            draggable.add(List.of(distractor));
        Collections.shuffle(draggable);
        for (List<String> group : draggable) {
            Collections.shuffle(group);
            for (String tile : group)
                state.tiles.add(makeTile(tile, /* fixed = */ false, indents)); 
        }
        // Put all the right braces at the end
        for (int i = state.tiles.size() - 1; i >= 0; i--) {
            if (state.tiles.get(i).code.equals("}")) { 
                Tile brace = state.tiles.remove(i);
                state.tiles.add(brace); 
            }
        }           
        return state;
    }

    private static boolean isTextFile(String p) { return p.toLowerCase().endsWith(".txt"); }
    private static boolean isTextFile(Path p) { return isTextFile(p.toString()); }
    
    public Problem.DisplayData getProblemData() {
        DisplayData data = new DisplayData();
        String description = readDescription("index.html");
        if (description == null) { // TODO: Legacy
            description = readDescription("problem.html");
        }

        data.description = description;
        
        data.useFiles = new LinkedHashMap<String, String>();
        for (Map.Entry<Path, byte[]> entry : useFiles.entrySet()) {
            Path path = entry.getKey();
            if (language.isSource(path) && !solutionFiles.containsKey(path) && !annotations.getHidden().contains(path) || isTextFile(path)) {
                String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
                data.useFiles.put(path.toString(), removePseudoComments(contents));
            }
        }
        if (oldStyle) {
            for (Path path : solutionFiles.keySet()) {
                EditorState state = new EditorState();
                state.editors = List.of(removePseudoComments(Util.getString(useFiles, path))); 
                data.requiredFiles.put(path.toString(), state);
            }
        } else {
            for (Map.Entry<Path, byte[]> entry : solutionFiles.entrySet()) {
                Path path = entry.getKey();
                String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
                data.requiredFiles.put(path.toString(), processHideShow(contents));
            }
            for (Map.Entry<Path, byte[]> entry : inputFiles.entrySet()) {
                Path path = entry.getKey();
                String contents = new String(entry.getValue(), StandardCharsets.UTF_8);
                EditorState state = new EditorState();
                state.editors = List.of(contents);
                data.requiredFiles.put(path.toString(), state);
            }
        }
        return data;
    }
    
    public String getId() {
        String problemId = annotations.findUnique("ID");
        if (problemId == null) { // TODO: Move to Problem
            problemId = Util.removeExtension(solutionFiles.keySet().iterator().next().getFileName());                
        }
        else {
            problemId = problemId.replaceAll("[^A-Za-z0-9]", "").toLowerCase();
        }
        return problemId;
    }
}
