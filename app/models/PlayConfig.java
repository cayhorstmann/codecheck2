package models;

import play.Play;

public class PlayConfig implements Config {
	public static final PlayConfig INSTANCE = new PlayConfig();
	@Override
	public String get(String key) {
		// TODO Auto-generated method stub
		return Play.application().configuration().getString(key);
	}
}
