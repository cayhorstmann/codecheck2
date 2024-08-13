package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class NJSReport extends HTMLReport {
    public static class ReportData {    
        public List<Error> errors = new ArrayList<>();
        public String report; 
        public String score;
        public Map<String, String> metadata = new LinkedHashMap<>();
    }
    
    private ReportData data = new ReportData();
    
    private String sectionType;
        
    public NJSReport(String title) {
        super(title);        
    }
    
    @Override
    public NJSReport comment(String key, String value) {
        if (data == null || data.metadata == null) return this; // Superclass constructor calls comment
        super.comment(key, value);
        data.metadata.put(key, value);
        return this;
    }
    
    @Override
    public NJSReport header(String sectionType, String text) {
        if (this.sectionType != null) builder.append("</div>\n");
        //if (!"studentFiles".equals(sectionType) && !"providedFiles".equals(sectionType)) 
            super.header(sectionType, text);
        builder.append("<div class=\"" + sectionType + "\">\n");
        this.sectionType = sectionType;
        return this;
    }
        
    @Override
    public NJSReport file(String file, String contents) {
         //if (!"studentFiles".equals(sectionType) && !"providedFiles".equals(sectionType)) 
             super.file(file, contents);
        return this;
    }

    @Override
    public NJSReport errors(List<Error> errors) {
        data.errors.addAll(errors);
        return this;
    }
    
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
    public String extension() { return "json"; }
    
    @Override
    public void close() {
        if (sectionType != null) builder.append("</div>\n");
        
        addFootnotes();
        builder.append("</body></html>\n");     
        data.report = builder.toString();
    }
    
    @Override
    public NJSReport add(Score score) {
        data.score = score.toString();
        super.add(score);
        return this;
    }
}
