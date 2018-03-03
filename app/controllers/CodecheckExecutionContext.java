package controllers;

import play.libs.concurrent.CustomExecutionContext;
import play.libs.concurrent.HttpExecution;

import javax.inject.Inject;

import akka.actor.ActorSystem;

import java.util.concurrent.Executor;
import java.util.concurrent.CompletionStage;

import static java.util.concurrent.CompletableFuture.supplyAsync;

// https://www.playframework.com/documentation/2.6.x/JavaAsync

public class CodecheckExecutionContext extends CustomExecutionContext {

	@Inject
    public CodecheckExecutionContext(ActorSystem actorSystem) {
    // uses a custom thread pool defined in application.conf
        super(actorSystem, "akka.actor.codecheck-dispatcher");
    }
}
