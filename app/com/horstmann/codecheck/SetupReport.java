package com.horstmann.codecheck;

public class SetupReport extends JSONReport {
	public SetupReport(String title) {
		super(title);
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
		return super.getText();
	}

}
