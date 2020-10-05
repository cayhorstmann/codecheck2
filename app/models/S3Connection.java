package models;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.ListObjectsV2Request;
import com.amazonaws.services.s3.model.ListObjectsV2Result;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import com.typesafe.config.Config;

import play.Logger;

@Singleton
public class S3Connection {
	private Config config;
	private String bucketSuffix = null;
	private AmazonS3 amazonS3;
	private AmazonDynamoDB amazonDynamoDB;
	
	public @Inject S3Connection(Config config) {
		this.config = config;
		String s3CredentialsKey = "com.horstmann.codecheck.s3credentials";
		if (config.hasPath(s3CredentialsKey)) {
			String s3CredentialsPath = config.getString(s3CredentialsKey);
			try {
				Properties props = new Properties();
				props.load(Files.newBufferedReader(Paths.get(s3CredentialsPath),
						StandardCharsets.UTF_8));
				String s3AccessKey = props.getProperty("accessKey");
				String s3SecretKey = props.getProperty("secretKey");
				String s3Region = props.getProperty("region", "us-west-1"); 
				amazonS3 = AmazonS3ClientBuilder
						.standard()
						.withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(s3AccessKey, s3SecretKey)))
						.withRegion(s3Region)
						.withForceGlobalBucketAccessEnabled(true)
						.build();
				
		    	amazonDynamoDB = AmazonDynamoDBClientBuilder
		    			.standard()
		    			.withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(s3AccessKey, s3SecretKey)))
						.withRegion("us-west-1")
		    			.build();
				
			} catch (IOException ex) {
				Logger.of("com.horstmann.codecheck").error("Can't load S3 credentials", ex);
			}
		} 
		bucketSuffix = config.getString("com.horstmann.codecheck.s3bucketsuffix");
	}

	public boolean isOnS3(String repo) {
		return !config.hasPath("com.horstmann.codecheck.repo." + repo);
	}
	
	public boolean isOnS3(String repo, String key) {
		String bucket = repo + "." + bucketSuffix;
		return getS3Connection().doesObjectExist(bucket, key);
	}

	private AmazonS3 getS3Connection() { 
		return amazonS3; 
	}

	public AmazonDynamoDB getAmazonDynamoDB() {
		return amazonDynamoDB;
	}
	
	public void putToS3(Path file, String repo, String key)
			throws IOException {
		String bucket = repo + "." + bucketSuffix;
		getS3Connection().putObject(bucket, key, file.toFile());
	}
	
	public void putToS3(String contents, String repo, String key)
			throws IOException {
		String bucket = repo + "." + bucketSuffix;
		getS3Connection().putObject(bucket, key, contents);
	}

	public void deleteFromS3(String repo, String key)
			throws IOException {
		String bucket = repo + "." + bucketSuffix;
		getS3Connection().deleteObject(bucket, key);
	}
	
	private byte[] readAllBytes(InputStream in) throws IOException { // TODO: Use InputStream.readAllBytes
		ByteArrayOutputStream buffer = new ByteArrayOutputStream();
		int nRead;
		byte[] data = new byte[16384];
		while ((nRead = in.read(data, 0, data.length)) != -1) 
			buffer.write(data, 0, nRead);
		return buffer.toByteArray();		
	}
	
	public String readFromS3(String repo, String key) 
			throws IOException {
		String bucket = repo + "." + bucketSuffix;
		/*
		return new String(getS3Connection().getObject(bucket, key).getObjectContent().readAllBytes(), 
				StandardCharsets.UTF_8);
				*/
		return new String(readAllBytes(getS3Connection().getObject(bucket, key).getObjectContent()), 
				StandardCharsets.UTF_8);
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
	
	public List<String> keys(String repo, String keyPrefix) throws AmazonServiceException {
		// https://docs.aws.amazon.com/AmazonS3/latest/dev/ListingObjectKeysUsingJava.html		
		String bucket = repo + "." + bucketSuffix;
		ListObjectsV2Request req = new ListObjectsV2Request()
				.withBucketName(bucket).withMaxKeys(100).withPrefix(keyPrefix);
		ListObjectsV2Result result;
		List<String> allKeys = new ArrayList<String>();
		
		do {
			result = getS3Connection().listObjectsV2(req);

			for (S3ObjectSummary objectSummary : result.getObjectSummaries()) {
				allKeys.add(objectSummary.getKey());
			}
	        
			String token = result.getNextContinuationToken();
			req.setContinuationToken(token);
		} while (result.isTruncated());
		return allKeys;
	}	
}