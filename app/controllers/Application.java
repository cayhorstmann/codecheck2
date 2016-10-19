package controllers;

import java.util.regex.*;
import play.mvc.Controller;
import play.mvc.Result;
import models.Util;

public class Application extends Controller {
   private Pattern dfPattern = Pattern.compile("(?s:.*?(?<percent>[0-9]+)%.*)");
   
    public Result health() {
       try {
          String df = Util.runProcess("/bin/df /", 1000);
          Matcher matcher = dfPattern.matcher(df);
          if (matcher.matches()) {
             String percent =  matcher.group("percent");
             if (Integer.parseInt(percent) > 95)
                return internalServerError("disk " + percent + "% full");
             else return ok("CodeCheck\n" + df);
          }
          else return ok("df output doesn't match pattern: " + df);
       } catch (Throwable ex) {
          return internalServerError(Util.getStackTrace(ex));
       }
    }   
}
