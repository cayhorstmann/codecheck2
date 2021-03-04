package com.horstmann.codecheck;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class CodioReport extends HTMLReport {
    private String section;

    public CodioReport(String title, Path outputDir) {
        super(title, outputDir);
    }
    
    @Override
    public HTMLReport file(Path dir, Path file) {
        if ("studentFiles".equals(section) || "providedFiles".equals(section)) return this;
        else return super.file(dir, file);
    }
    
    @Override
    public HTMLReport file(String file, String contents) {
        if ("studentFiles".equals(section) || "providedFiles".equals(section)) return this;
        else return super.file(file, contents);
    }
    
    @Override
    public HTMLReport header(String section, String text) {
        this.section = section;
        if ("studentFiles".equals(section) || "providedFiles".equals(section)) return this;
        return super.header(section, text);
    }

    @Override
    public HTMLReport save(String problemId, String out) throws IOException {
        // No download link, no metadata
        // TODO: Should Report have a footer method?
        builder.append("</body></html>\n");
        Path outPath = dir.resolve(out + ".html");
        Files.write(outPath, builder.toString().getBytes());
        return this;
    }        
}
