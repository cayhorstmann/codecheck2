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
import play.Configuration;

import com.amazonaws.ClientConfiguration;
import com.amazonaws.Protocol;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;

@Singleton
public class S3Connection {
	private Configuration config;
	private String s3AccessKey = null;
	private String s3SecretKey = null;
	
	public @Inject S3Connection(Configuration config) {
		this.config = config;
		String s3CredentialsPath = config.getString("com.horstmann.codecheck.s3credentials");
		if (s3CredentialsPath != null) {
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
	}


	public boolean isOnS3(String repo) {
		String repoPath = config.getString("com.horstmann.codecheck.repo." + repo);
		return repoPath == null;
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
	
	// Delete returnedPath.getParent() when done
	public Path unzipFromS3(String repo, String problem)
			throws IOException {
		Path unzipDir = java.nio.file.Files.createTempDirectory("problem");
		String id = problem.replaceAll("/", "_"); // No / in dir name
		Path problemDir = unzipDir.resolve(id);
		Files.createDirectory(problemDir);
		String bucket = repo + "." + config.getString("com.horstmann.codecheck.s3bucketsuffix");

		InputStream in = getS3Connection().getObject(bucket, problem)
				.getObjectContent();
		Util.unzip(in, problemDir);
		in.close();
		return problemDir;
	}
}