package controllers;

import java.util.regex.*;

import play.mvc.Controller;
import play.mvc.Http;
import play.mvc.Result;
import com.horstmann.codecheck.Util;

public class Application extends Controller {
   private Pattern dfPattern = Pattern.compile("(?s:.*?(?<percent>[0-9]+)%.*)");
   
    public Result health(Http.Request request) {
       try {
          String df = Util.runProcess("/bin/df /", 1000);
          Matcher matcher = dfPattern.matcher(df);
          if (matcher.matches()) {
             String percent =  matcher.group("percent");
             if (Integer.parseInt(percent) > 95)
                return internalServerError("disk " + percent + "% full");
             Runtime rt = Runtime.getRuntime();
             double mem = rt.freeMemory() * 100.0 / rt.totalMemory();
             // TODO: Analyze output
             // http://stackoverflow.com/questions/9229333/how-to-get-overall-cpu-usage-e-g-57-on-linux
             return ok("CodeCheck\n" + df + "\n" + mem + "% JVM memory free");
          }
          else return ok("df output doesn't match pattern: " + df);
       } catch (Throwable ex) {
          return internalServerError(Util.getStackTrace(ex));
       }
    }   
}
