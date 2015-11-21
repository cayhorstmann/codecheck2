import java.io.IOException;

import play.*;
import models.PlayConfig;
import models.Util;

public class Global extends GlobalSettings {
	@Override
	public void onStart(Application app) {
		try {
			Util.loadS3Credentials(PlayConfig.INSTANCE);
		} catch (IOException ex) {
			RuntimeException wrapper = new RuntimeException();
			wrapper.initCause(ex);
			throw wrapper;
		}
	}
}
