package com.horstmann.codecheck;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ProblemData {
	public Map<String, List<String>> requiredFiles = new LinkedHashMap<>();
	public Map<String, String> useFiles = new LinkedHashMap<>();
	public String description;
}