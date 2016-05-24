import java.io.IOException;

import models.PlayConfig;
import models.Util;
import play.Application;
import play.GlobalSettings;

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
