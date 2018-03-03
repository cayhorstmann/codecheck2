package models;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

import javax.inject.Inject;
import javax.inject.Singleton;

import play.Logger;
import com.typesafe.config.Config;

import com.amazonaws.ClientConfiguration;
import com.amazonaws.Protocol;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;

@Singleton
public class S3Connection {
	private Config config;
	private String s3AccessKey = null;
	private String s3SecretKey = null;
	private String bucketSuffix = null;
	
	public @Inject S3Connection(Config config) {
		this.config = config;
		String s3CredentialsKey = "com.horstmann.codecheck.s3credentials";
		if (config.hasPath(s3CredentialsKey)) {
			String s3CredentialsPath = config.getString(s3CredentialsKey);
			try {
				Properties props = new Properties();
				props.load(Files.newBufferedReader(Paths.get(s3CredentialsPath),
						StandardCharsets.UTF_8));
				s3AccessKey = props.getProperty("accessKey");
				s3SecretKey = props.getProperty("secretKey");
			} catch (IOException ex) {
				Logger.error("Can't load S3 credentials", ex);
			}
		} 
		bucketSuffix = config.getString("com.horstmann.codecheck.s3bucketsuffix");
	}

	public boolean isOnS3(String repo) {
		return !config.hasPath("com.horstmann.codecheck.repo." + repo);
	}

	private AmazonS3 getS3Connection() throws IOException {
		AWSCredentials credentials = new BasicAWSCredentials(s3AccessKey,
				s3SecretKey);

		ClientConfiguration clientConfig = new ClientConfiguration();
		clientConfig.setProtocol(Protocol.HTTP);

		return new AmazonS3Client(credentials, clientConfig);
	}

	public void putToS3(Path file, String bucket, String key)
			throws IOException {
		InputStream in = Files.newInputStream(file);
		try {
			getS3Connection().putObject(bucket, key, in, new ObjectMetadata());
		} finally {
			in.close();
		}
	}

	public void deleteFromS3(String bucket, String key)
			throws IOException {
		getS3Connection().deleteObject(bucket, key);
	}
	
	// Delete returned path when done
	public Path unzipFromS3(String repo, String problem)
			throws IOException {
		Path problemDir = java.nio.file.Files.createTempDirectory("problem");
		String bucket = repo + "." + bucketSuffix;

		InputStream in = getS3Connection().getObject(bucket, problem)
				.getObjectContent();
		Util.unzip(in, problemDir);
		in.close();
		return problemDir;
	}
}