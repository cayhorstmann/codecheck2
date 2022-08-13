package models;

import java.time.Duration;

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
        String prefix;
        if(request.host().equals("localhost")) {
            prefix = "../";
            long countSlash = request.uri().chars().filter(ch -> ch == '/').count() - 1;
            for (long i = 0; i < countSlash; ++i) {
                prefix += "../";
            }
            prefix = prefix.substring(0, prefix.length() - 1);
        }
        else {
            prefix = (secure ? "https://" : "http://") + request.host();
        }  
        return prefix;
    }
    
    public static ObjectNode toJson(Object obj) { 
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(Include.NON_DEFAULT);
        return (ObjectNode) mapper.convertValue(obj, JsonNode.class);
    }
    
    public static Http.Cookie buildCookie(String name, String value) {
        return Http.Cookie.builder(name, value)
                .withPath("/")
                .withMaxAge(Duration.ofDays(180))
                .withSameSite(Http.Cookie.SameSite.STRICT)
                .build();
    }
}
