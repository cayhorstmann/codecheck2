package com.horstmann.codecheck;

import java.util.Iterator;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class SetupReport extends JSONReport {
	private Problem problem;
	
	public SetupReport(String title) {
		super(title);
	}
	
	public void setProblem(Problem problem) {
		this.problem = problem;
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
		}
		return dataNode.toString();
	}
}
