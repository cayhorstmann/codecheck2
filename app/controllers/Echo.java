package controllers;

import java.io.IOException;

import play.libs.Json;
import play.libs.Jsonp;
import play.mvc.Controller;
import play.mvc.Result;

import com.fasterxml.jackson.databind.node.ObjectNode;

public class Echo extends Controller {
	public Result echoJson() throws IOException  {
		String callback = request().getQueryString("callback");
		ObjectNode result = Json.newObject();
		result.put("received", true);
		result.put("score", request().getQueryString("score"));
		if (callback == null)
			return ok(result.asText());
		else
			return ok(Jsonp.jsonp(callback, result));		
	}
}
