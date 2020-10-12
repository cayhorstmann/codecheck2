package models;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.RangeKeyCondition;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.PutItemSpec;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.ListObjectsV2Request;
import com.amazonaws.services.s3.model.ListObjectsV2Result;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
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
	
	public List<String> readS3keys(String repo, String keyPrefix) throws AmazonServiceException {
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

    public ObjectNode readJsonObjectFromDynamoDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
    	String result = readJsonStringFromDynamoDB(tableName, primaryKeyName, primaryKeyValue);
    	return result == null ? null : (ObjectNode)(new ObjectMapper().readTree(result)); 
    }
    
	public String readJsonStringFromDynamoDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
    	DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
    	Table table = dynamoDB.getTable(tableName); 
    	ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue);
    	try {
	    	Iterator<Item> iterator = items.iterator();
	    	if (iterator.hasNext())
	    		return iterator.next().toJSON();
	    	else
	    		return null;
    	} catch (ResourceNotFoundException ex) {
    		return null;
    	}
    }	
    
    public ObjectNode readJsonObjectFromDynamoDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
    	String result = readJsonStringFromDynamoDB(tableName, primaryKeyName, primaryKeyValue, sortKeyName, sortKeyValue);
    	return result == null ? null : (ObjectNode)(new ObjectMapper().readTree(result)); 
    }
    
    public String readJsonStringFromDynamoDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
    	DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
    	Table table = dynamoDB.getTable(tableName); 
		ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue, 
				new RangeKeyCondition(sortKeyName).eq(sortKeyValue));
		try {
			Iterator<Item> iterator = items.iterator();
	    	if (iterator.hasNext())
	    		return iterator.next().toJSON();
	    	else
	    		return null;
		} catch (ResourceNotFoundException ex) {
			return null;
		}
    }
    
    public Map<String, ObjectNode> readJsonObjectsFromDynamoDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName) throws IOException {
    	DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
    	Table table = dynamoDB.getTable(tableName); 
    	ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue);
    	Iterator<Item> iterator = items.iterator();
    	Map<String, ObjectNode> itemMap = new HashMap<>();
    	while (iterator.hasNext()) {
    		Item item = iterator.next();
    		String key = item.getString(sortKeyName);
    		itemMap.put(key, (ObjectNode)(new ObjectMapper().readTree(item.toJSON())));
    	}
    	return itemMap;
    }
    
    public void writeJsonObjectToDynamoDB(String tableName, ObjectNode obj) {
    	DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
    	Table table = dynamoDB.getTable(tableName); 
   		table.putItem(
			new PutItemSpec()
				.withItem(Item.fromJSON(obj.toString()))
		);
    }

    public void writeNewerJsonObjectToDynamoDB(String tableName, ObjectNode obj, String primaryKeyName, String timeStampKeyName) {
    	DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
    	Table table = dynamoDB.getTable(tableName); 
    		/*
    To prevent a new item from replacing an existing item, use a conditional expression that contains the attribute_not_exists function with the name of the attribute being used as the partition key for the table. Since every record must contain that attribute, the attribute_not_exists function will only succeed if no matching item exists.
    https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/SQLtoNoSQL.WriteData.html

    Apparently, the simpler putItem(item, conditionalExpression, nameMap, valueMap) swallows the ConditionalCheckFailedException 
    		 */
    	String conditionalExpression = "attribute_not_exists(" + primaryKeyName + ") OR " + timeStampKeyName + " < :" + timeStampKeyName;
		table.putItem(
    		new PutItemSpec()
    			.withItem(Item.fromJSON(obj.toString()))
    			.withConditionExpression(conditionalExpression)
    			.withValueMap(Collections.singletonMap(":" + timeStampKeyName, obj.get(timeStampKeyName).asText()))
    	);         	
    }   
}