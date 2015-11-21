package models;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class ProblemData {
	@XmlElement public Map<String, String> requiredFiles = new LinkedHashMap<>();
	  @XmlElement public Map<String, String> useFiles = new LinkedHashMap<>();
	  @XmlElement public String description;
}
