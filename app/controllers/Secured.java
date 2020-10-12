package controllers;

import java.util.Optional;

import play.Logger;
import play.mvc.Http;
import play.mvc.Result;
import play.mvc.Security;

public class Secured extends Security.Authenticator {
	private Logger.ALogger logger = Logger.of("com.horstmann.codecheck");

    @Override
    public Optional<String> getUsername(Http.Request request) {
    	logger.info("Secured.getUsername: " + request.session().get("user"));
        return request.session().get("user");
    }

    @Override
    public Result onUnauthorized(Http.Request request) {
        return badRequest("Not logged in");
    }
}