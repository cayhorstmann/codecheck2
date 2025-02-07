package controllers;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.horstmann.codecheck.ResourceLoader;

import play.db.Database;
import play.db.Databases;

@Singleton public class Config implements ResourceLoader {
    @Inject private com.typesafe.config.Config config;
    @Inject private play.api.Environment playEnv;
    private Database db;

    public InputStream loadResource(String path) throws IOException {
        return playEnv.classLoader().getResourceAsStream("public/resources/" + path);
    }
    public String getProperty(String key) {
        return config.hasPath(key) ? config.getString(key) : null;
    }
    public String getString(String key) { // TODO Why do we need both getString and getProperty?
        return getProperty(key);
    }
    public boolean hasPath(String key) {
        return getProperty(key) != null;
    }
    
    // TODO: Thread safety
    public Connection getDatabaseConnection() {
    	if (db == null) {
    		String driver = config.getString("com.horstmann.corejava.sql.driver");
    		String url = config.getString("com.horstmann.corejava.sql.url");
    		db = Databases.createFrom(driver, url);
    	}
    	return db.getConnection();
    }
}