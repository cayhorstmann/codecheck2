import java.io.IOException;
import java.util.logging.Logger;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;


public class Initializer implements ServletContextListener {
	
	@Override
	public void contextInitialized(ServletContextEvent sce) {
		try {
			Util.loadS3Credentials(sce.getServletContext());
		} catch (IOException ex) {
			Logger.getLogger("com.horstmann.codecheck").throwing("Initializer", "contextInitialized", ex);
		}		
	}
	
	@Override
	public void contextDestroyed(ServletContextEvent sce) {	
		
	}

}
