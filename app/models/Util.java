package models;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import play.mvc.Http;

public class Util {
    public static String prefix(Http.Request request) {
        boolean secure = request.secure() || request.getHeaders().getAll("X-Forwarded-Proto").contains("https");
        /*
           One shouldn't have to do this, but with Google Cloud, X-Forwarded-For has two entries (e.g. [95.90.234.41, 130.211.33.19])
           and X-Forwarded-Proto has one ([https]). From 
           https://github.com/playframework/playframework/blob/814f0c73f86eb0e85bcae7f2167c73a08fed9fd7/transport/server/play-server/src/main/scala/play/core/server/common/ForwardedHeaderHandler.scala
           line 204, Play doesn't conclude that the connection was secure. 
         */     
        return (secure ? "https://" : "http://") + request.host();
    }
    
    public static ObjectNode toJson(Object obj) { 
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(Include.NON_DEFAULT);
        return (ObjectNode) mapper.convertValue(obj, JsonNode.class);
    }
}
