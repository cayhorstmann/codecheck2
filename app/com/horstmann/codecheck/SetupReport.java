package com.horstmann.codecheck;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class SetupReport extends JSONReport {
	private Problem problem;
	private Map<String, String> hiddenFiles = new HashMap<>();
	private Map<String, Object> attributes = new HashMap<>();
	private List<Condition> conditions = new ArrayList<>();
	
	static class Condition {
		public boolean forbidden;
		public String path;
		public String regex;
		public String message;
	}
	
	public SetupReport(String title) {
		super(title);
	}
	
	public void setProblem(Problem problem) {
		this.problem = problem;
	}
	
	public JSONReport file(String fileName, byte[] contents, boolean hidden) {
		if (Report.isImage(fileName)) {
			hiddenFiles.put(fileName, Report.imageData(fileName, contents));
		}
		else if (hidden) {
			hiddenFiles.put(fileName, new String(contents, StandardCharsets.UTF_8));
		}
		// Other files are already in Problem
		// TODO: What if it's another binary file?
    	return this;
    }
	
	@Override public JSONReport attribute(String key, Object value) {
		attributes.put(key, value);
		return this;
	}
	
	@Override public JSONReport condition(boolean passed, boolean forbidden, Path path, String regex, String message) {
		Condition c = new Condition();
		c.forbidden = forbidden;
		c.path = path.toString();
		c.regex = regex;
		c.message = message;
		conditions.add(c);
		return this;
	}
	
	@Override public String getText() {
		for (Section section : data.sections) {
			for (Run run : section.runs) {
				if (run.output == null || run.output == "") {
					StringBuilder output = new StringBuilder();
					for (Match match : run.matchedOutput) {
						output.append(match.actual);
						output.append("\n");
					}
					run.matchedOutput = null;
					run.output = output.toString();
				}
				run.passed = null;
				run.html = null;
			}
		}
		data.metaData.clear();
		data.score = null;
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(Include.NON_DEFAULT);
		ObjectNode dataNode = (ObjectNode) mapper.convertValue(data, JsonNode.class);
		if (problem != null) {
			ObjectNode problemNode = (ObjectNode) mapper.convertValue(problem.getProblemData(), JsonNode.class);
			Iterator<Map.Entry<String, JsonNode>> fields = problemNode.fields();
			while (fields.hasNext()) {
				Map.Entry<String, JsonNode> entry = fields.next();
				dataNode.set(entry.getKey(), entry.getValue());
			}
			if (!hiddenFiles.isEmpty()) {
				dataNode.set("hiddenFiles", mapper.convertValue(hiddenFiles, JsonNode.class));
			}		
			if (!attributes.isEmpty()) {
				dataNode.set("attributes", mapper.convertValue(attributes, JsonNode.class));
			}
			if (!conditions.isEmpty()) {
				dataNode.set("conditions", mapper.convertValue(conditions, JsonNode.class));
			}
		}
		return dataNode.toString();
	}
}
