package models;

import java.io.ByteArrayInputStream;

/*

Test plan

http://localhost:9000/assets/uploadProblem.html

Test.py

##HIDE
print('hi')
##EDIT ...

Save, then edit and modify

http://localhost:9000/newAssignment
paste URL from problem, save

visit as student
solve
use private url in another browser
clear id
solve again, different

visit as instructor
view submissions
click on a submission
make a comment
view again to see if comment saved
view as student to see if comment saved

 */


import java.io.IOException;
import java.io.InputStream;
import java.lang.System.Logger;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.FileLockInterruptionException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Singleton;

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
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.ConditionalCheckFailedException;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.AmazonS3Exception;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.horstmann.codecheck.Util;

import controllers.Config;

@Singleton public class StorageConnector {
    private StorageConnection delegate;

    @Inject public StorageConnector(Config config) {
    	String type = "local";
    	if (config.hasPath("com.horstmann.codecheck.storage.type"))
    	    type = config.getString("com.horstmann.codecheck.storage.type");
        if (type.equalsIgnoreCase("aws")) 
            delegate = new AWSStorageConnection(config);      
        else if (type.equalsIgnoreCase("sql")) {
            delegate = new SQLStorageConnection(config);        
        }
        else                
            delegate = new LocalStorageConnection(config);        
    }

    public byte[] readProblem(String repo, String key) throws IOException {
        return delegate.readProblem(repo, key);
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        delegate.writeProblem(contents, repo, key);
    }    
    
    public ObjectNode readAssignment(String assignmentID) throws IOException {
        return delegate.readAssignment(assignmentID);
    }
    
    public String readLegacyLTIResource(String resourceID) throws IOException {
    	return delegate.readLegacyLTIResource(resourceID);
    }
    
    public String readLTISharedSecret(String oauthConsumerKey) throws IOException {
    	return delegate.readLTISharedSecret(oauthConsumerKey);
    }
    
    public String readComment(String assignmentID, String workID) throws IOException {
    	return delegate.readComment(assignmentID, workID);
    }
    
    public ObjectNode readWork(String assignmentID, String workID) throws IOException {
    	return delegate.readWork(assignmentID, workID);
    }

    public String readWorkString(String assignmentID, String workID) throws IOException {
    	return delegate.readWorkString(assignmentID, workID);
    }
    
    public ObjectNode readNewestSubmission(String submissionID) throws IOException {
    	return delegate.readNewestSubmission(submissionID);
    }
    
    public Map<String, ObjectNode> readAllWork(String assignmentID) throws IOException {
    	return delegate.readAllWork(assignmentID);
    }
    
    public void writeAssignment(JsonNode node) throws IOException {
    	delegate.writeAssignment(node);
    }
    
    public void writeSubmission(JsonNode node) throws IOException {
    	delegate.writeSubmission(node);
    }
    
    public void writeComment(JsonNode node) throws IOException {
    	delegate.writeComment(node);
    }
    
    public boolean writeWork(JsonNode node) throws IOException {
    	return delegate.writeWork(node);
    }
}

interface StorageConnection {
    byte[] readProblem(String repo, String key) throws IOException;
    void writeProblem(byte[] contents, String repo, String key) throws IOException;
	ObjectNode readAssignment(String assignmentID) throws IOException;
    String readLegacyLTIResource(String resourceID) throws IOException;
    String readLTISharedSecret(String oauthConsumerKey) throws IOException;
    String readComment(String assignmentID, String workID) throws IOException;    
    ObjectNode readWork(String assignmentID, String workID) throws IOException;
    String readWorkString(String assignmentID, String workID) throws IOException;    
    ObjectNode readNewestSubmission(String submissionID) throws IOException;    
    Map<String, ObjectNode> readAllWork(String assignmentID) throws IOException;     
    void writeAssignment(JsonNode node) throws IOException;
    void writeSubmission(JsonNode node) throws IOException;
    void writeComment(JsonNode node)  throws IOException;    
    boolean writeWork(JsonNode node) throws IOException; // return true if this version was saved (because it was newer)
}

/*
 Tables:
   
 CodeCheckAssignment
   assignmentID [partition key] // non-LTI: courseID? + assignmentID, LTI: toolConsumerID/courseID + assignment ID, Legacy tool consumer ID/course ID/resource ID  
   deadline (an ISO 8601 string like "2020-12-01T23:59:59Z")
   editKey // LTI: tool consumer ID + user ID
   problems
     array of // One per group
       array of { URL, qid?, weight } // qid for book repo
  
 CodeCheckLTIResources (Legacy)
   resourceID [primary key] // LTI tool consumer ID + course ID + resource ID 
   assignmentID

 CodeCheckWork is a map from problem keys to scores and states. It only stores the most recent version.
   
 CodeCheckWork
   assignmentID [primary key]
   workID [sort key] // non-LTI: ccid/editKey, LTI: userID
   problems 
     map from URL/qids to { state, score }
   submittedAt
   tab     
 
 CodeCheckSubmissions is an append-only log of all submissions of a single problem.    
        
 CodeCheckSubmissions
   submissionID [partition key] // non-LTI: courseID? + assignmentID + problemKey + ccid/editKey , LTI: toolConsumerID/courseID + assignmentID + problemID + userID 
     // either way, that's resource ID + workID + problem key
   submittedAt [sort key] 
   state: as string, not JSON
   score
  
   with global secondary index (TODO: Not currently)
     problemID 
     submitterID
   
 CodeCheckLTICredentials
   oauth_consumer_key [primary key]
   shared_secret

CodeCheckComments
   assignmentID [partition key] // non-LTI: courseID? + assignmentID, LTI: toolConsumerID/courseID + assignment ID, Legacy tool consumer ID/course ID/resource ID  
   workID [sort key] // non-LTI: ccid/editKey, LTI: userID
   comment

   (This is a separate table from CodeCheckWork because we can't guarantee atomic updates if a student happens to update their work while the instructor updates a comment)
 
*/

class AWSStorageConnection implements StorageConnection {
    private static Logger logger = System.getLogger("com.horstmann.codecheck");     
    private String bucketSuffix = null;
    private AmazonS3 amazonS3;
    private AmazonDynamoDB amazonDynamoDB;    
    public static class OutOfOrderException extends RuntimeException {}

    public AWSStorageConnection(Config config) {
        String awsAccessKey = config.getString("com.horstmann.codecheck.aws.accessKey");
        String awsSecretKey = config.getString("com.horstmann.codecheck.aws.secretKey");
        String s3region = config.getString("com.horstmann.codecheck.s3.region"); 
        amazonS3 = AmazonS3ClientBuilder
                .standard()
                .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(awsAccessKey, awsSecretKey)))
                .withRegion(s3region)
                .withForceGlobalBucketAccessEnabled(true)
                .build();

        bucketSuffix = config.getString("com.horstmann.codecheck.s3.bucketsuffix");
    	
        String dynamoDBregion = config.getString("com.horstmann.codecheck.dynamodb.region"); 
        
        amazonDynamoDB = AmazonDynamoDBClientBuilder
                .standard()
                .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(awsAccessKey, awsSecretKey)))
                .withRegion(dynamoDBregion)
                .build();
    }    
    
    public byte[] readProblem(String repo, String key) throws IOException {
        String bucket = repo + "." + bucketSuffix;

        byte[] bytes = null;
        try {
            // TODO -- trying to avoid warning 
            // WARN - com.amazonaws.services.s3.internal.S3AbortableInputStream - Not all bytes were read from the S3ObjectInputStream, aborting HTTP connection. This is likely an error and may result in sub-optimal behavior. Request only the bytes you need via a ranged GET or drain the input stream after use
            try (InputStream in = amazonS3.getObject(bucket, key).getObjectContent()) {
                bytes = in.readAllBytes();
            }
        } catch (AmazonS3Exception ex) {
            logger.log(Logger.Level.ERROR, "S3Connection.readFromS3: Cannot read " + key + " from " + bucket);
            throw ex;
        }
        return bytes;            
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        String bucket = repo + "." + bucketSuffix;
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(contents.length);
        metadata.setContentType("application/zip");
        try {
            try (ByteArrayInputStream in = new ByteArrayInputStream(contents)) {
                amazonS3.putObject(bucket, key, in, metadata); 
            }                 
        } catch (AmazonS3Exception ex) {
            String bytes = Arrays.toString(contents);
            logger.log(Logger.Level.ERROR, "S3Connection.putToS3: Cannot put " + bytes.substring(0, Math.min(50, bytes.length())) + "... to " + bucket);
            throw ex;                
        }
    }

    public ObjectNode readAssignment(String assignmentID) throws IOException {
		return readJsonObjectFromDB("CodeCheckAssignments", "assignmentID", assignmentID);
	}
	
	public String readLegacyLTIResource(String resourceID) throws IOException {
    	ObjectNode node = readJsonObjectFromDB("CodeCheckLTIResources", "resourceID", resourceID);
    	if (node == null) return null;
    	else return node.get("assignmentID").asText();
    }

	public String readLTISharedSecret(String oauthConsumerKey) throws IOException {
        ObjectNode node = readJsonObjectFromDB("CodeCheckLTICredentials", "oauth_consumer_key", oauthConsumerKey);
        if (node == null) return null;
        else return node.get("shared_secret").asText();
    }

	public String readComment(String assignmentID, String workID) throws IOException {
    	ObjectNode node = readJsonObjectFromDB("CodeCheckComments", "assignmentID", assignmentID, "workID", workID);
        if (node == null) return "";
        else return node.get("comment").asText();    	
    }
    
	public ObjectNode readWork(String assignmentID, String workID) throws IOException {
    	return readJsonObjectFromDB("CodeCheckWork", "assignmentID", assignmentID, "workID", workID);
    }

	public String readWorkString(String assignmentID, String workID) throws IOException {
    	return readJsonStringFromDB("CodeCheckWork", "assignmentID", assignmentID, "workID", workID);
    }
    
	public ObjectNode readNewestSubmission(String submissionID) throws IOException {
    	return readNewestJsonObjectFromDB("CodeCheckSubmissions", "submissionID", submissionID);
    }
    
	public Map<String, ObjectNode> readAllWork(String assignmentID) throws IOException {
    	return readJsonObjectsFromDB("CodeCheckWork", "assignmentID", assignmentID, "workID");
    }
    
    public void writeAssignment(JsonNode node) throws IOException {
    	writeJsonObjectToDB("CodeCheckAssignments", node);
    }
    
    public void writeSubmission(JsonNode node) throws IOException {
    	writeJsonObjectToDB("CodeCheckSubmissions", node);
    }
    
    public void writeComment(JsonNode node)  throws IOException {
    	writeJsonObjectToDB("CodeCheckComments", node);
    }
    
    public boolean writeWork(JsonNode node) throws IOException {
    	return writeNewerJsonObjectToDB("CodeCheckWork", node, "assignmentID", "submittedAt");
    }

    private ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
        String result = readJsonStringFromDB(tableName, primaryKeyName, primaryKeyValue);
        return result == null ? null : Util.fromJsonString(result); 
    }

    private ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
        String result = readJsonStringFromDB(tableName, primaryKeyName, primaryKeyValue, sortKeyName, sortKeyValue);
        return result == null ? null : Util.fromJsonString(result); 
    }
    
    private String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
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
    
    private ObjectNode readNewestJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) {
        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
        Table table = dynamoDB.getTable(tableName); 
        QuerySpec spec = new QuerySpec()
                .withKeyConditionExpression(primaryKeyName + " = :primaryKey" )
                .withValueMap(new ValueMap().withString(":primaryKey", primaryKeyValue))
                .withScanIndexForward(false);
        
        ItemCollection<QueryOutcome> items = table.query(spec);
        try {
            Iterator<Item> iterator = items.iterator();
            if (iterator.hasNext()) {
                String result = iterator.next().toJSON();
                try {
                    return Util.fromJsonString(result);
                } catch (JsonProcessingException ex) {
                    return null;
                }
            }
            else
                return null;
        } catch (ResourceNotFoundException ex) {
            return null;
        }
    }        

    private String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
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

    private Map<String, ObjectNode> readJsonObjectsFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName) throws IOException {
        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
        Table table = dynamoDB.getTable(tableName); 
        ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue);
        Iterator<Item> iterator = items.iterator();
        Map<String, ObjectNode> itemMap = new HashMap<>();
        while (iterator.hasNext()) {
            Item item = iterator.next();
            String key = item.getString(sortKeyName);
            itemMap.put(key, Util.fromJsonString(item.toJSON()));
        }
        return itemMap;
    }

    private void writeJsonObjectToDB(String tableName, JsonNode obj) {
        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
        Table table = dynamoDB.getTable(tableName); 
        table.putItem(
            new PutItemSpec()
                .withItem(Item.fromJSON(obj.toString()))
        );
    }

    private boolean writeNewerJsonObjectToDB(String tableName, JsonNode obj, String primaryKeyName, String timeStampKeyName) {
        DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
        Table table = dynamoDB.getTable(tableName);
            /*
    To prevent a new item from replacing an existing item, use a conditional expression that contains the attribute_not_exists function with the name of the attribute being used as the partition key for the table. Since every record must contain that attribute, the attribute_not_exists function will only succeed if no matching item exists.
    https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/SQLtoNoSQL.WriteData.html

    Apparently, the simpler putItem(item, conditionalExpression, nameMap, valueMap) swallows the ConditionalCheckFailedException 
             */
        String conditionalExpression = "attribute_not_exists(" + primaryKeyName + ") OR " + timeStampKeyName + " < :" + timeStampKeyName;
        try {
            table.putItem(
                new PutItemSpec()
                    .withItem(Item.fromJSON(obj.toString()))
                    .withConditionExpression(conditionalExpression)
                    .withValueMap(Collections.singletonMap(":" + timeStampKeyName, obj.get(timeStampKeyName).asText())));
            return true;
        } catch(ConditionalCheckFailedException e) {
            // https://github.com/aws/aws-sdk-java/issues/1945
            logger.log(Logger.Level.WARNING, "writeNewerJsonObjectToDB: " + e.getMessage() + " " + obj);
            return false;
        }   
    }
}

/*

Stores the assignment data in the local file system.

root/
  Problems/
    repo1
      key11.zip
      key12.zip
      ...
    repo2
      key21.zip
      key22.zip
      ...
      
  CodeCheckAssignments/
    assignmentID1 <- JSON file like in DynamoDB
    assignmentID2
    ...

  CodeCheckLTICredentials  <- JSON object mapping consumer keys to shared secrets
    
  CodeCheckComments
    assignmentID1
      workID11 <- the comment
      workID12
      ...      
   assignmentID2
      workID21
      workID22
      ...      
   ...
   
  CodeCheckWork 
    assignmentID1
      workID11 <- JSON file like in DynamoDB
      workID12
      ...      
   assignmentID2
      workID21
      workID22
      ...      
   ...
   
  CodeCheckSubmissions
    submissionID1
      timestamp11 <- JSON file like in DynamoDB
      timestamp12
      ...
    submissionID2
      timestamp21
      timestamp22
      ...
    ...   
    
Legacy LTI resources are not supported.    
  
*/

class LocalStorageConnection implements StorageConnection {
    private Path root;
    private static Logger logger = System.getLogger("com.horstmann.codecheck");     
    private ObjectNode credentials;
    
    public LocalStorageConnection(Config config) {        
        this.root = Path.of(config.getString("com.horstmann.codecheck.storage.local"));
        try {
           Files.createDirectories(root);            
        } catch (IOException ex) {
            logger.log(Logger.Level.ERROR, "Cannot create " + root);
        } 
    }
    
    public byte[] readProblem(String repo, String key) throws IOException {
        byte[] result = null;
        try {
            Path repoPath = root.resolve("Problems").resolve(repo);
            Path filePath = repoPath.resolve(key + ".zip");
            result = Files.readAllBytes(filePath); 
        } catch (IOException ex) {
            logger.log(Logger.Level.ERROR, "ProblemLocalConnection.read : Cannot read " + key + " from " + repo);
            throw ex;                
        }
        
        return result;  
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        try {
            Path repoPath = root.resolve("Problems").resolve(repo);
            Files.createDirectories(repoPath);                
            Path newFilePath = repoPath.resolve(key + ".zip");
            Files.write(newFilePath, contents);
        } catch (IOException ex) {
            String bytes = Arrays.toString(contents);
            logger.log(Logger.Level.ERROR, "ProblemLocalConnection.write : Cannot put " + bytes.substring(0, Math.min(50, bytes.length())) + "... to " + repo);
            throw ex;                   
        }
    }
    
    
	public ObjectNode readAssignment(String assignmentID) throws IOException {
		return readJsonObject("CodeCheckAssignments", assignmentID);
	}
	
    public String readLegacyLTIResource(String resourceID) throws IOException {
    	return null;
    }

    public String readLTISharedSecret(String oauthConsumerKey) throws IOException {
    	if (credentials == null)
            credentials = Util.fromJsonString(Files.readString(path("CodeCheckLTICredentials")));
        return credentials.get("shared_secret").asText();
    }
    
    public String readComment(String assignmentID, String workID) throws IOException {
    	Path path = path("CodeCheckComments", assignmentID, workID);
    	if (Files.exists(path)) return Files.readString(path);
    	else return "";
    }
    
    public ObjectNode readWork(String assignmentID, String workID) throws IOException {
    	return readJsonObject("CodeCheckWork", assignmentID, workID);
    }

    public String readWorkString(String assignmentID, String workID) throws IOException {
    	return readJsonString("CodeCheckWork", assignmentID, workID);
    }

    public ObjectNode readNewestSubmission(String submissionID) throws IOException {
        Path path = path("CodeCheckSubmissions", submissionID);

        try (Stream<Path> entries = Files.list(path)) {
            Path latest = entries.filter(Files::isRegularFile).max(Path::compareTo).orElse(null);
            if (latest == null) return null;
            String content = Files.readString(latest);    
            try {
                return Util.fromJsonString(content);
            } catch (JsonProcessingException ex) {
                logger.log(Logger.Level.WARNING, "AssignmentConnector.readNewestJsonObjectFromDB: cannot read " + latest.toString() + "***File content: " + content);
                return null;
            } 
        } catch (IOException ex) {
            return null;
        }
    }
    
    public Map<String, ObjectNode> readAllWork(String assignmentID) throws IOException {
        Map<String, ObjectNode> itemMap = new HashMap<>();
        Path path = path("CodeCheckWork", assignmentID);
        try {
            try (Stream<Path> entries = Files.list(path)) {
                List<Path> files = entries.filter(Files::isRegularFile).collect(Collectors.toList());
                for (Path file : files) {
                    String fileData = Files.readString(file);   
                    ObjectNode node = Util.fromJsonString(fileData);
                    String key = node.get("workID").asText();
                    itemMap.put(key, node);
                }
            }
        } catch (IOException ex){
            logger.log(Logger.Level.WARNING, Util.getStackTrace(ex));
        }    
        return itemMap;
    }
    
    public void writeAssignment(JsonNode node) throws IOException {
    	String assignmentID = node.get("assignmentID").asText();
        Path path = path("CodeCheckAssignments", assignmentID);
        Files.createDirectories(path.getParent()); 
        Files.writeString(path, node.toString());
    }
    
    public void writeSubmission(JsonNode node) throws IOException {
        String submissionID = node.get("submissionID").asText();
        String submittedAt = node.get("submittedAt").asText();
        Path path = path("CodeCheckSubmissions", submissionID, submittedAt);
        Files.createDirectories(path.getParent()); 
        Files.writeString(path, node.toString());
    }
    
    public void writeComment(JsonNode node)  throws IOException {
        String assignmentID = node.get("assignmentID").asText();
        String workID = node.get("workID").asText();
    	Path path = path("CodeCheckComments", assignmentID, workID);
        Files.createDirectories(path.getParent());
        Files.writeString(path, node.get("comment").asText());
    } 
    
    public boolean writeWork(JsonNode node) throws IOException {    	
        String assignmentID = node.get("assignmentID").asText();
        String workID = node.get("workID").asText();
        Path path = path("CodeCheckWork", assignmentID, workID); 
        Files.createDirectories(path.getParent()); 
        String newTimeStampVal = node.get("submittedAt").asText();
        
        int tries = 10;
        while (tries > 0) {
            FileChannel channel = FileChannel.open(path, 
            		StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE);
            try (FileLock lock = channel.lock()) {
                ByteBuffer readBuffer = ByteBuffer.allocate((int) channel.size());
                channel.read(readBuffer);
                byte[] contents = readBuffer.array();
                boolean replace = false;
                if (contents.length == 0) replace = true;
                else {
                	ObjectNode prevNode = Util.fromJsonString(readBuffer.array());
                	String prevTimeStampVal = prevNode.get("submittedAt").asText();
                	replace = prevTimeStampVal.compareTo(newTimeStampVal) < 0;
                }
                if (replace) {
                    channel.truncate(0);
                    ByteBuffer writeBuffer = ByteBuffer.wrap(node.toString().getBytes());
                    channel.write(writeBuffer);
                    return true;
                }
                else return false;
            } catch (FileLockInterruptionException ex) {
            	try { Thread.sleep(1000); } catch (InterruptedException ex2) {}
                tries--;
            }                
        } 
        throw new IOException("Could not acquire lock for " + path);
    }

    private Path path(String first, String... rest) {
    	Path result = root.resolve(first);
    	for (String r : rest) result = result.resolve(r.replaceAll("[^a-zA-Z0-9_-]", ""));
    	return result;
    }

    private ObjectNode readJsonObject(String tableName, String primaryKeyValue) throws IOException {
        String result = readJsonString(tableName, primaryKeyValue);
        return result == null ? null : Util.fromJsonString(result); 
    }

    private ObjectNode readJsonObject(String tableName, String primaryKeyValue, String sortKeyValue) throws IOException {
        String result = readJsonString(tableName, primaryKeyValue, sortKeyValue);
        return result == null ? null : Util.fromJsonString(result);
    }
    
    public String readJsonString(String tableName, String primaryKeyValue) throws IOException {
    	Path path = path(tableName, primaryKeyValue);

        try {
            return Files.readString(path);
        } catch (IOException ex) {
            logger.log(Logger.Level.WARNING, "AssignmentLocalConnection.readJsonString: Cannot read " + path);
            return null;
        }
    }
    
    public String readJsonString(String tableName, String primaryKeyValue, String sortKeyValue) throws IOException {
    	Path path = path(tableName, primaryKeyValue, sortKeyValue);

        try {
            return Files.readString(path);
        } catch (IOException ex) {
            logger.log(Logger.Level.WARNING, "AssignmentLocalConnection.readJsonString: Cannot read " + path);
            return null;
        }
    }
}

/*
 
CREATE TABLE CodeCheckAssignments (assignmentID VARCHAR PRIMARY KEY, json VARCHAR)
CREATE TABLE CodeCheckLTICredentials (oauth_consumer_key VARCHAR PRIMARY KEY, shared_secret VARCHAR)
CREATE TABLE CodeCheckComments (assignmentID VARCHAR, workID VARCHAR, comment VARCHAR, UNIQUE (assignmentID, workID))
CREATE TABLE CodeCheckWork (assignmentID VARCHAR, workID VARCHAR, submittedAt VARCHAR, json VARCHAR, UNIQUE (assignmentID, workID))
CREATE TABLE CodeCheckSubmissions (submissionID VARCHAR, submittedAt VARCHAR, json VARCHAR)
 
 */

class SQLStorageConnection implements StorageConnection {
    private static Logger logger = System.getLogger("com.horstmann.codecheck");
    private Config config;
    
    public SQLStorageConnection(Config config) {
		this.config = config;
	}

	public byte[] readProblem(String repo, String key) throws IOException {
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT contents FROM Problems WHERE repo = ? AND key = ?");
            ps.setString(1, repo);
            ps.setString(2, key);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return rs.getBytes(1);
            else return null;
        } catch (SQLException ex) {
            logger.log(Logger.Level.ERROR, ex.getMessage());
            throw new IOException(ex);
        }
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        try {
            try (Connection conn = config.getDatabaseConnection()) {
            	// https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-UNIQUE-CONSTRAINTS
                PreparedStatement ps = conn.prepareStatement("""
INSERT INTO Problems VALUES (?, ?, ?) 
ON CONFLICT (repo, key) 
DO UPDATE SET contents = EXCLUDED.contents                		
""");
                ps.setString(1, repo);
                ps.setString(2, key);
                ps.setBytes(3, contents);
                ps.executeUpdate();
            }
        } catch (SQLException ex) {
            logger.log(Logger.Level.ERROR, ex.getMessage());
            throw new IOException(ex);
        }
    }    
    
    public ObjectNode readAssignment(String assignmentID) throws IOException {
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT json FROM CodeCheckAssignments WHERE assignmentID = ?");
            ps.setString(1, assignmentID);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return Util.fromJsonString(rs.getString(1));
            else return null;
        } catch (SQLException ex) {
            throw new IOException(ex);
        }		
	}
	
    public String readLegacyLTIResource(String resourceID) throws IOException{
    	return null;
	}
	
    public String readLTISharedSecret(String oauthConsumerKey) throws IOException{
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT shared_secret FROM CodeCheckLTICredentials WHERE oauth_consumer_key = ?");
            ps.setString(1, oauthConsumerKey);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return rs.getString(1);
            else return null;
        } catch (SQLException ex) {
            throw new IOException(ex);
        }				
	}
	
    public String readComment(String assignmentID, String workID) throws IOException{
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT comment FROM CodeCheckComments WHERE assignmentID = ? AND workID = ?");
            ps.setString(1, assignmentID);
            ps.setString(2, workID);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return rs.getString(1);
            else return null;
        } catch (SQLException ex) {
            throw new IOException(ex);
        }		
	}
	
    public ObjectNode readWork(String assignmentID, String workID) throws IOException{
    	String result = readWorkString(assignmentID, workID);
    	return result == null ? null : Util.fromJsonString(result);
	}
	
    public String readWorkString(String assignmentID, String workID) throws IOException{
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT json FROM CodeCheckWork WHERE assignmentID = ? AND workID = ?");
            ps.setString(1, assignmentID);
            ps.setString(2, workID);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return rs.getString(1);
            else return null;
        } catch (SQLException ex) {
            throw new IOException(ex);
        }				
	}
	
    public ObjectNode readNewestSubmission(String submissionID) throws IOException{
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement(
"""
SELECT json FROM CodeCheckSubmissions WHERE submissionID = ? AND submittedAT = 
  (SELECT MAX(submittedAt) FROM CodeCheckSubmissions WHERE submissionID = ?)           		
""");
            ps.setString(1, submissionID);
            ps.setString(2, submissionID);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return Util.fromJsonString(rs.getString(1));
            else return null;
        } catch (SQLException ex) {
            throw new IOException(ex);
        }				
		
	}
	
    public Map<String, ObjectNode> readAllWork(String assignmentID) throws IOException{
    	Map<String, ObjectNode> result = new HashMap<>();
        try (Connection conn = config.getDatabaseConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT workID, json FROM CodeCheckWork WHERE assignmentID = ?");
            ps.setString(1, assignmentID);
            ResultSet rs = ps.executeQuery();
            while (rs.next())
            	result.put(rs.getString(1), Util.fromJsonString(rs.getString(2)));
            return result;
        } catch (SQLException ex) {
            throw new IOException(ex);
        }						
	}
	
    public void writeAssignment(JsonNode node) throws IOException {
        try {
            try (Connection conn = config.getDatabaseConnection()) {
            	// https://wiki.postgresql.org/wiki/What's_new_in_PostgreSQL_9.5#INSERT_..._ON_CONFLICT_DO_NOTHING.2FUPDATE_.28.22UPSERT.22.29
                PreparedStatement ps = conn.prepareStatement("""
INSERT INTO CodeCheckAssignments VALUES (?, ?) 
ON CONFLICT (assignmentID) 
DO UPDATE SET json = EXCLUDED.json                		
""");
                ps.setString(1, node.get("assignmentID").asText());
                ps.setString(2, node.toString());
                ps.executeUpdate();
            }
        } catch (SQLException ex) {
            logger.log(Logger.Level.ERROR, ex.getMessage());
            throw new IOException(ex);
        }
	}
	
    public void writeSubmission(JsonNode node) throws IOException {
        try {
            try (Connection conn = config.getDatabaseConnection()) {
                PreparedStatement ps = conn.prepareStatement("INSERT INTO CodeCheckSubmissions VALUES (?, ?, ?)");
                ps.setString(1, node.get("submissionID").asText());
                ps.setString(2, node.get("submittedAt").asText());
                ps.setString(3, node.toString());
                ps.executeUpdate();
            }
        } catch (SQLException ex) {
            logger.log(Logger.Level.ERROR, ex.getMessage());
            throw new IOException(ex);
        }
	}
	
    public void writeComment(JsonNode node)  throws IOException {
        try {
            try (Connection conn = config.getDatabaseConnection()) {
            	// https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-UNIQUE-CONSTRAINTS
                PreparedStatement ps = conn.prepareStatement("""
INSERT INTO CodeCheckComments VALUES (?, ?, ?) 
ON CONFLICT (assignmentID, workID) 
DO UPDATE SET comment = EXCLUDED.comment                		
""");
                ps.setString(1, node.get("assignmentID").asText());
                ps.setString(2, node.get("workID").asText());
                ps.setString(3, node.get("comment").asText());
                ps.executeUpdate();
            }
        } catch (SQLException ex) {
            logger.log(Logger.Level.ERROR, ex.getMessage());
            throw new IOException(ex);
        }
	}
	
    public boolean writeWork(JsonNode node) throws IOException {
        try {
            try (Connection conn = config.getDatabaseConnection()) {
            	// https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-UNIQUE-CONSTRAINTS
                PreparedStatement ps = conn.prepareStatement("""
INSERT INTO CodeCheckWork VALUES (?, ?, ?, ?) 
ON CONFLICT (assignmentID, workID) 
DO UPDATE SET submittedAt = EXCLUDED.submittedAt, json = EXCLUDED.json
WHERE CodeCheckWork.submittedAt < EXCLUDED.submittedAt                 		
""");
                ps.setString(1, node.get("assignmentID").asText());
                ps.setString(2, node.get("workID").asText());
                ps.setString(3, node.get("submittedAt").asText());
                ps.setString(4, node.toString());
                int rowcount = ps.executeUpdate();
                return rowcount > 0;
            }
        } catch (SQLException ex) {
            logger.log(Logger.Level.ERROR, ex.getMessage());
            throw new IOException(ex);
        }
    }
}

