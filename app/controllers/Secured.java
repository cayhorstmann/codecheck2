package controllers;

import java.util.Optional;

import play.Logger;
import play.mvc.Http;
import play.mvc.Result;
import play.mvc.Security;

public class Secured extends Security.Authenticator {
    @Override
    public Optional<String> getUsername(Http.Request request) {
        return request.session().get("user");
    }

    @Override
    public Result onUnauthorized(Http.Request request) {
        return badRequest("Not logged in");
    }
}