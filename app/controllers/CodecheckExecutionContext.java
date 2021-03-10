package controllers;

import javax.inject.Inject;

import akka.actor.ActorSystem;
import play.libs.concurrent.CustomExecutionContext;

// https://www.playframework.com/documentation/2.6.x/JavaAsync

public class CodecheckExecutionContext extends CustomExecutionContext {

	@Inject
    public CodecheckExecutionContext(ActorSystem actorSystem) {
    // uses a custom thread pool defined in application.conf
        super(actorSystem, "akka.actor.codecheck-dispatcher");
    }
}
