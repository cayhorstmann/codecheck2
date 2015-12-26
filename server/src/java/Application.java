import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.ApplicationPath;

@ApplicationPath("/")
public class Application extends javax.ws.rs.core.Application {
   @Override
   public Set<Class<?>> getClasses() {
      return new HashSet<>(Arrays.asList(
            Check.class, 
            CheckGit.class,
            CheckUpload.class, 
            Fetch.class, 
            UploadProblem.class, 
            Files.class, 
            Grade.class, 
            Problems.class,
            org.glassfish.jersey.jackson.JacksonFeature.class,
            org.glassfish.jersey.media.multipart.MultiPartFeature.class
            ));   
   }
}
